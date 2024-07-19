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
import delimited "G:\Mi unidad\Data\Cluster\cluster_aggregates.csv", clear
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


** Import data
import delimited "G:\Mi unidad\Data\Anonymised data\rdo_Stata_c4_full_noid.csv", clear
gen group_all = 4


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


gen d_c   = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
gen d_d   = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
gen d_p   = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
gen d_cd  = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
gen d_pc  = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
gen d_pd  = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
gen d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 
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

*** Base model to compute R2
set processors 4
asclogit fished, base("No-Participation")  alternatives(selection) case(fished_haul) 
estimates store base
estimates save ${results}nlogit_C4_base.ster, replace
scalar ll0 = e(ll)

tab msqdclosured // Always MSQD open!!!

*** Set nested logit
cap drop port
cap label drop lb_port
cap drop partp
cap label drop lb_partp
nlogitgen port = selection( ///
	CMCK: LAA-CMCK | SBA-CMCK, ///
	MSQD: LAA-MSQD | MNA-MSQD | MRA-MSQD | SBA-MSQD | SFA-MSQD, ///
	PSDN: LAA-PSDN | MNA-PSDN, ///
	NANC: LAA-NANC | MNA-NANC | SFA-NANC, ///
	TUNA: LAA-YTNA | LAA-BTNA, ///
    NOPORT: No-Participation)
nlogitgen partp = port(PART: CMCK | MSQD | PSDN | NANC | TUNA, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 


save "G:\Mi unidad\Data\Anonymised data\part_model_c4.dta"

************************
*** Run nested logit ***
************************
* nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
*			psdnclosured btnaclosured dummy_last_day unem_rate d_c d_d d_p d_cd d_pc d_pd d_pcd  /// 
*			|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
*		base("No-Participation") case(fished_haul) vce(cluster fished_vessel_anon)
*	estimates save ${results}nlogit_FULL_c4_22042024.ster, replace
estimates use ${results}nlogit_FULL_c4_22042024.ster
matrix start=e(b)
estimates store B1
estimates describe B1
estimates replay B1
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B1
lrtest base B1, force
estadd scalar lr_p = r(p): B1
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: B1
estadd scalar bic = S[1,6]: B1
estadd scalar aicc = S[1,7]: B1
estadd scalar caic = S[1,8]: B1
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
	estadd scalar perc1 = count1/_N*100: B1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: B1
restore


// nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 			psdnclosured btnaclosured d_hist_selection unem_rate d_c d_d d_p d_cd d_pc d_pd d_pcd  /// 
// 			|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 		base("No-Participation") case(fished_haul) vce(cluster fished_vessel_anon) ///
//  		from(start, skip)
estimates save ${results}nlogit_FULL_c4_hist_22042024.ster, replace
estimates use ${results}nlogit_FULL_c4_hist_22042024.ster
estimates store B2
estimates describe B2
estimates replay B2
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B2
lrtest base B2, force
estadd scalar lr_p = r(p): B2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: B2
estadd scalar bic = S[1,6]: B2
estadd scalar aicc = S[1,7]: B2
estadd scalar caic = S[1,8]: B2
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
	estadd scalar perc1 = count1/_N*100: B2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: B2
restore




*** Save model

esttab  B1 B2 using "G:\Mi unidad\Tables\Participation\nested_logit_FULL_${S_DATE}_c4.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 perc3 perc4 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" ///
			"Predicted choices (%) -- No SD" "- Excl. No-Participation (%) -- No SD" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se noconstant
