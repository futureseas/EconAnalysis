global google_path "H:\My Drive\"
*global google_path "G:\Mi unidad\"
global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


import delimited "C:\GitHub\EconAnalysis\Data\Ports\ports_area_and_name_codes.csv", varnames(1) clear
tempfile port_area
save `port_area'

** Get historical choice
import delimited "${google_path}Data\Cluster\cluster_aggregates.csv", clear
merge m:1 pacfin_port_code using `port_area', keep(3)
keep if landing_year >= 2000 & landing_year <= 2020
collapse (sum) total_landings, by(group_all pacfin_species_code landing_month landing_year port_area_code)
collapse (mean) mean_landings = total_landings, by(group_all pacfin_species_code landing_month port_area_code)
bysort group_all landing_month (mean_landings): keep if  mean_landings==mean_landings[_N]
tab group_all landing_month
keep landing_month group_all pacfin_species_code port_area_code
gen  hist_selection = port_area_code + "-" + pacfin_species_code 
drop pacfin_species_code port_area_code
rename landing_month set_month
tempfile hist_data
save `hist_data'


** Import data (It do not work if 180km radius for ALBC -- 90km radius is the best -- V2)
import delimited "${google_path}Data\Anonymised data\rdo_Stata_c5_full_v2_noid.csv", clear
gen group_all = 5
// gen species = substr(selection, 5, 4) 

** Merge with historical data
merge m:1 group_all set_month using `hist_data', keep(3)
gen d_hist_selection = (hist_selection == selection)

** Add addtional variables
gen psdnclosured2 = psdnclosured - psdntotalclosured
gen psdnclosure2 = psdnclosure - psdntotalclosure
replace mean_price2 = mean_price2 / 1000
replace mean_price = mean_price / 1000
replace mean_catch = mean_catch / 1000
replace d_missing_p = d_missing_p2 if mean_price > 50
replace mean_price = mean_price2 if mean_price > 50


gen d_c   = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 0) 
gen d_d   = (d_missing_p2 == 0 & d_missing == 0 & d_missing_d == 1) 
gen d_p   = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 0) 
gen d_cd  = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 1) 
gen d_pc  = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 0) 
gen d_pd  = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 1) 
gen d_pcd = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 1) 
qui tabulate set_month, generate(month)


********************************************************************
** Create labels
label variable mean_avail "Expected availability"
label variable mean_catch "Expected catch"
label variable mean_price "Expected price"
label variable mean_price2 "Expected price (30 days)"
label variable diesel_price "Expected diesel price"
label variable wind_max_220_mh "Maximum wind (< 220km)"
label variable d_missing "Binary: Missing availability"
label variable d_missing_cpue "Binary: Missing availability (CPUE)"
label variable d_missing_p "Binary: Missing price"
label variable dist_port_to_catch_area "Distance to catch area"
label variable dist_port_to_catch_area_zero "Distance to catch area"
label variable d_missing_d "Binary: Missing distance"
label variable lat_cg "Latitudinal Center of Gravity"
label variable unem_rate "State unemployment rate"
label variable dist_to_cog "Distance to Center of Gravity" 
label variable ddieselstate "Binary: Diesel price by state" 
label variable psdnclosured "Binary: PSDN Closure x PSDN" 
label variable psdnclosured2 "Binary: PSDN Seasonal Closure x PSDN chosen" 
label variable psdntotalclosured "Binary: PSDN Total Closure x PSDN chosen" 
label variable msqdclosured "Binary: MSQD Closure"
label variable msqdweekend  "Binary: Weekend x MSQD chosen"
label variable dummy_last_day "Alternative has been chosen last day"
label variable dummy_prev_days "Alternative has been chosen during the last 30 days"
label variable dummy_prev_days_port "Port has been chosen during the last 30 days"
label variable dummy_prev_year_days "Alternative has been chosen during the last 30 days (previous year)"
label variable dummy_clust_prev_days "Alternative has been chosen during the last 30 days by any member of the fleet"
label variable hist_selection "Alternative has been historically chosen during the month (>20% revenue)"
label variable d_c "Binary: Availability missing "
label variable d_p "Binary: Price missing "
label variable d_d "Binary: Distance missing "
label variable d_pd "Binary: Distance and price missing"
label variable d_pc "Binary: Price and availability missing"
label variable d_cd "Binary: Availability and distance missing"
label variable d_pcd "Binary: Availability, distance and price missing"


***************************
** Estimate nested logit **
***************************

** Filter model 

keep if selection == "SBA-MSQD" | selection == "MNA-MSQD" | ///
		selection == "LAA-MSQD" | selection == "SFA-MSQD" | /// 
		selection == "CBA-MSQD" | selection == "MRA-MSQD" | ///
		selection == "NPA-MSQD" | ///		
	    selection == "CLO-PSDN" | selection == "CWA-PSDN" | ///
		selection == "CLW-PSDN" | selection == "LAA-PSDN" | ///
		selection == "CWA-ALBC" | ///
		selection == "LAA-CMCK" | selection == "SBA-CMCK" | ///
		selection == "LAA-NANC" | selection == "No-Participation" | ///
		selection == "CLW-DCRB" | selection == "CWA-DCRB"

* drop if msqdclosure // I THINK THIS HELP!!!


** Drop cases with no choice selected
cap drop check_if_choice
sort fished_haul
by fished_haul: egen check_if_choice = sum(fished)
tab check_if_choice
keep if check_if_choice
tab check_if_choice

*** Base model to compute R2
set processors 4
asclogit fished, base("No-Participation")  alternatives(selection) case(fished_haul) 
estimates store base
estimates save ${results}nlogit_C5_base.ster, replace
scalar ll0 = e(ll)

*** Set nested logit
cap drop port
cap label drop lb_port
cap drop partp
cap label drop lb_partp
nlogitgen port = selection( ///
	MSQD: SBA-MSQD | MNA-MSQD | LAA-MSQD | SFA-MSQD | CBA-MSQD | MRA-MSQD | NPA-MSQD, ///
	PSDN: CLO-PSDN | CWA-PSDN | CLW-PSDN | LAA-PSDN, ///
	ALBC: CWA-ALBC, ///
	CMCK: LAA-CMCK | SBA-CMCK, ///
	NANC: LAA-NANC, ///
	DCRB: CLW-DCRB | CWA-DCRB, ///
	NOPORT: No-Participation)
nlogitgen partp = port(PART: MSQD | PSDN | ALBC | CMCK | NANC, PART_CRAB: DCRB, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 
constraint 1 [/port]DCRB_tau = 1
constraint 2 [/port]CMCK_tau = 1


preserve
	replace d_c   = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0)
	replace d_d   = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1)
	replace d_p   = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0)
	replace d_cd  = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1)
	replace d_pc  = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0)
	replace d_pd  = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1)
	replace d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1)
	save "${google_path}Data\Anonymised data\part_model_c5.dta", replace
restore


	replace d_c   = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
	replace d_d   = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
	replace d_p   = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
	replace d_cd  = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
	replace d_pc  = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
	replace d_pd  = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
	replace d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 

sum d_c d_d d_p d_pc d_pd d_cd d_pcd


************************
*** Run nested logit ***
************************

**** USING ALBC 90KM SDM 


*** Price in partp with price model


/* 	nlogit fished  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
			dummy_last_day unem_rate d_c d_d d_p d_pc d_pd d_cd d_pcd dcrbclosurewad waclosured  /// 
			|| partp: mean_price mean_avail psdnclosure, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
		base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) ///
		// from(start, skip)
	// estimates save ${results}nlogit_FULL_C5_v12.ster, replace */

	// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
	// 		dummy_last_day unem_rate d_d  d_pd d_cd d_pcd  /// 
	// 		|| partp: mean_price , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) ///
	// 	matrix start=e(b)

	// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
	// 		dummy_last_day unem_rate d_d d_cd msqdclosured /// 
	// 	|| partp: mean_price , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) // from(start, skip)
	//  estimates save ${results}nlogit_FULL_C5_v12_A.ster, replace 
	// estimates use ${results}nlogit_FULL_C5_v12_A.ster
	// matrix start=e(b)



** No convergence with dcrbclosurewad. Did not converge also if included in partp
tab msqdclosured
tab psdnclosured
tab waclosured 
tab dcrbclosurewad
 		
	// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
	// 		dummy_last_day unem_rate d_d d_cd msqdclosured psdnclosured /// 
	// 	|| partp: mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) from(start, skip)
	//  estimates save ${results}nlogit_FULL_C5_v12_B.ster, replace 

	// 	matrix start=e(b)



	// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
	// 		dummy_last_day unem_rate d_d d_cd msqdclosured psdnclosured dcrbclosurewad /// 
	// 	|| partp: mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) from(start, skip)
	//  estimates save ${results}nlogit_FULL_C5_v12_C.ster, replace 

	// 	matrix start=e(b)

	// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
	// 		dummy_last_day unem_rate d_d d_cd msqdclosured psdnclosured waclosured /// 
	// 	|| partp: mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) from(start, skip)
	//  estimates save ${results}nlogit_FULL_C5_v12_D.ster, replace 

	// 	matrix start=e(b)

	// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
	// 		dummy_last_day unem_rate d_d d_cd msqdclosured psdnclosured waclosured dcrbclosurewad /// 
	// 	|| partp: mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) from(start, skip)
	//  estimates save ${results}nlogit_FULL_C5_v12_E.ster, replace 


	 estimates use ${results}nlogit_FULL_C5_v12_E.ster
	 	estimates store B12_E
		matrix start=e(b)
		estimates describe B12_E
		estimates replay B12_E


*** Run this model with price constrained to zero!

	constraint 3 [PART_CRAB]mean_price = 0
	nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
			dummy_last_day unem_rate d_d d_cd msqdclosured psdnclosured waclosured dcrbclosurewad /// 
		|| partp:  mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
		base("No-Participation") case(fished_haul) constraints(1 3) vce(cluster fished_vessel_anon) from(start, skip)
	 estimates save ${results}nlogit_FULL_C5_v12_F.ster, replace 
	matrix start=e(b)
	estimates store B12_F

	lrtest B12_F B12_E, force
	lrtest B12_E B12_F, force

	*** LR test reject that coefficient for price equal to zero...







*** mean availability in partp too??





estimates use ${results}nlogit_FULL_C5_v12.ster
matrix start=e(b)
estimates store B12
lrtest B12 B10, force

**** Test constraint
// preserve
// 	replace d_c   = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
// 	replace d_d   = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
// 	replace d_p   = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
// 	replace d_cd  = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
// 	replace d_pc  = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
// 	replace d_pd  = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
// 	replace d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 
// 	constraint 3 [PART_CRAB]mean_price = 0
// 	nlogit fished mean_avail wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 			psdnclosured dummy_last_day unem_rate d_c d_d d_p d_pc d_pd d_cd d_pcd dcrbclosurewad waclosured  /// 
// 			|| partp: mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 		base("No-Participation") case(fished_haul) constraints(1 3) vce(cluster fished_vessel_anon) ///
// 		from(start, skip)
// 	estimates save ${results}nlogit_FULL_C5_v14.ster, replace
// restore
estimates use ${results}nlogit_FULL_C5_v14.ster
matrix start=e(b)
estimates store B14
lrtest B14 B12, force


**** WIth other state dependency
// preserve
// 	replace d_c   = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
// 	replace d_d   = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
// 	replace d_p   = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
// 	replace d_cd  = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
// 	replace d_pc  = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
// 	replace d_pd  = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
// 	replace d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 
// 	constraint 3 [PART_CRAB]mean_price = 0
// 	nlogit fished mean_avail wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 			psdnclosured dummy_prev_days dummy_prev_year_days unem_rate d_c d_d d_p d_pc d_pd d_cd d_pcd dcrbclosurewad waclosured  /// 
// 			|| partp: mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 		base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) ///
// 		from(start, skip)
// 	estimates save ${results}nlogit_FULL_C5_v15.ster, replace
// restore
estimates use ${results}nlogit_FULL_C5_v15.ster
estimates store B15
lrtest B15 B14, force


*****************************************************************************************************************************
*** WITH HIST SELECTION!

tab d_hist_selection


// preserve
// 	replace d_c   = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
// 	replace d_d   = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
// 	replace d_p   = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
// 	replace d_cd  = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
// 	replace d_pc  = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
// 	replace d_pd  = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
// 	replace d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 
// 	constraint 3 [PART_CRAB]mean_price = 0
//  	nlogit fished mean_avail wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
//  			psdnclosured d_hist_selection unem_rate d_c d_d d_p d_pc d_pd d_cd d_pcd dcrbclosurewad waclosured  /// 
//  			|| partp: mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
//  		base("No-Participation") case(fished_haul) constraints(1 3) vce(cluster fished_vessel_anon) ///
//  		from(start, skip)
// 	*estimates save ${results}nlogit_FULL_C5_v16.ster, replace
// 	estimates save ${results}nlogit_FULL_C5_v16.ster
// restore
// estimates use ${results}nlogit_FULL_C5_v16.ster
// matrix start=e(b)
estimates store B16
lrtest B16 B14, force


// preserve
// 	replace d_c   = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
// 	replace d_d   = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
// 	replace d_p   = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
// 	replace d_cd  = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
// 	replace d_pc  = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
// 	replace d_pd  = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
// 	replace d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 
// 	constraint 3 [PART_CRAB]mean_price = 0
//  	nlogit fished mean_avail wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
//  			psdnclosured d_hist_selection unem_rate d_c d_d d_p d_pc d_pd d_cd d_pcd dcrbclosurewad waclosured  /// 
//  			|| partp: mean_price, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
//  		base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_anon) ///
//  		from(start, skip)
// 	estimates save ${results}nlogit_FULL_C5_v17.ster
// restore
estimates use ${results}nlogit_FULL_C5_v17.ster
estimates store B17
lrtest B17 B16, force


*****************************************************************************************************************************
estimates use ${results}nlogit_FULL_C5_v14.ster
estimates store B14
estimates describe B14
estimates replay B14
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B14
lrtest base B14, force
estadd scalar lr_p = r(p): B14
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: B14
estadd scalar bic = S[1,6]: B14
estadd scalar aicc = S[1,7]: B14
estadd scalar caic = S[1,8]: B14
preserve
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc1 = count1/_N*100: B14
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: B14
restore



*****************************************************************************************************************************
*** Save model

esttab  B1 B2 B3 B4 B5 B6 using "G:\My Drive\Tables\Participation\nested_logit_FULL_${S_DATE}_C5.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 perc3 perc4 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" ///
			"Predicted choices (%) -- No SD" "- Excl. No-Participation (%) -- No SD" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se noconstant
 



