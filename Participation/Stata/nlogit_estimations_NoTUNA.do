global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


** Import data
use rdo_Stata_c4_full.dta, clear

** Add addtional variables
preserve
 import delimited "Stata\dbp_month_Stata_c4.csv", clear
 tempfile dbp_month
 save dbp_month
restore
merge m:1 selection set_month using "dbp_month.dta"
drop if _merge == 2
replace hist_selection = 0 if _merge == 1
drop _merge
gen psdnclosured2 = psdnclosured - psdntotalclosured
gen psdnclosure2 = psdnclosure - psdntotalclosure
replace d_missing_p = d_missing_p2 if mean_price > 50
replace mean_price = mean_price2 if mean_price > 50
replace mean_price = mean_price / 1000
replace mean_catch = mean_catch / 1000
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

keep if selection == "LAA-CMCK" | ///
		selection == "LAA-MSQD" | ///
		selection == "SFA-MSQD" | ///
		selection == "SFA-NANC" | ///
		selection == "MNA-NANC" | ///
		selection == "MNA-MSQD" | ///
		selection == "SBA-MSQD" | ///
		selection == "SBA-CMCK" | ///
        selection == "No-Participation" | ///
		selection == "MNA-PSDN" | ///
		selection == "LAA-PSDN" | ///
		selection == "LAA-NANC" | ///
		selection == "MRA-MSQD" 

** Drop cases with no choice selected
cap drop check_if_choice
sort fished_haul
by fished_haul: egen check_if_choice = sum(fished)
tab check_if_choice
keep if check_if_choice
tab check_if_choice

** Create nested logit variables
nlogitgen port = selection( ///
	LAA: LAA-CMCK | LAA-MSQD | LAA-NANC | LAA-PSDN, ///
    MNA: MNA-MSQD | MNA-NANC | MNA-PSDN, /// 
    MRA: MRA-MSQD, ///
    SBA: SBA-CMCK | SBA-MSQD, /// 
    SFA: SFA-MSQD | SFA-NANC, /// 
    NOPORT: No-Participation)
nlogitgen partp = port(PART: LAA | MNA | MRA | SBA | SFA, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 


** New Logit tree
constraint 1 [/port]MRA_tau = 1
constraint 2 [/port]NOPORT_tau = 1 
constraint 3 [/partp]NOPART_tau = 1 


*** Base model to compute R2
nlogit fished  || partp: , base(NOPART) || port: , base(NOPORT) || selection: , ///
		base("No-Participation") case(fished_haul) vce(cluster fished_vessel_num)
matrix start=e(b) 
estimates save ${results}basemodel_FULL.ster 
// estimates use ${results}basemodel_FULL.ster
estimates store base
scalar ll0 = e(ll)


*** Estimate model
eststo A1: nlogit fished mean_avail mean_price wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured ///
	d_d d_cd || partp: unem_rate, base(NOPART) || port: , base(NOPORT) || selection: weekend, ///
	base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)
	matrix start=e(b) 
	estimates save ${results}nlogit_1_FULL.ster
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A1
lrtest base A1, force
estadd scalar lr_p = r(p): A1
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A1
estadd scalar bic = S[1,6]: A1
estadd scalar aicc = S[1,7]: A1
estadd scalar caic = S[1,8]: A1
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
	estadd scalar perc1 = count1/_N*100: A1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A1
restore

eststo A2: nlogit fished mean_avail mean_price wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured ///
	d_d d_cd hist_selection || partp: unem_rate, base(NOPART) || port: , base(NOPORT) || selection: weekend, ///
	base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)
	estimates save ${results}nlogit_2_FULL.ster
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A2
lrtest A1 A2, force
estadd scalar lr_p = r(p): A2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A2
estadd scalar bic = S[1,6]: A2
estadd scalar aicc = S[1,7]: A2
estadd scalar caic = S[1,8]: A2
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
	estadd scalar perc1 = count1/_N*100: A2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A2
restore

eststo A3: nlogit fished mean_avail mean_price wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured ///
	d_d d_cd dummy_last_day dummy_prev_year_days || partp: unem_rate, base(NOPART) || port: , base(NOPORT) || selection: weekend, ///
	base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)
	estimates save ${results}nlogit_3_FULLp.ster
	matrix start=e(b) 
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A3
lrtest A1 A3, force
estadd scalar lr_p = r(p): A3
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A3
estadd scalar bic = S[1,6]: A3
estadd scalar aicc = S[1,7]: A3
estadd scalar caic = S[1,8]: A3
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
	estadd scalar perc1 = count1/_N*100: A3
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A3
restore

eststo A4: nlogit fished mean_avail mean_price wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured ///
	d_d d_cd dummy_last_day dummy_prev_year_days || partp: unem_rate, base(NOPART) || port: , base(NOPORT) || selection: weekend lunarill, ///
	base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)
	estimates save ${results}nlogit_4_FULL.ster
	matrix start=e(b) 
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A4
lrtest A3 A4, force
estadd scalar lr_p = r(p): A4
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A4
estadd scalar bic = S[1,6]: A4
estadd scalar aicc = S[1,7]: A4
estadd scalar caic = S[1,8]: A4
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
	estadd scalar perc1 = count1/_N*100: A4
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A4
restore



******************************************
*** Using expected catch instead of SDM

replace d_c   = (d_missing_p == 0 & d_missing_catch == 1 & d_missing_d == 0) 
replace d_d   = (d_missing_p == 0 & d_missing_catch == 0 & d_missing_d == 1) 
replace d_p   = (d_missing_p == 1 & d_missing_catch == 0 & d_missing_d == 0) 
replace d_cd  = (d_missing_p == 0 & d_missing_catch == 1 & d_missing_d == 1) 
replace d_pc  = (d_missing_p == 1 & d_missing_catch == 1 & d_missing_d == 0) 
replace d_pd  = (d_missing_p == 1 & d_missing_catch == 0 & d_missing_d == 1) 
replace d_pcd = (d_missing_p == 1 & d_missing_catch == 1 & d_missing_d == 1) 

eststo A5: nlogit fished mean_catch mean_price wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured ///
	d_c d_d d_cd d_pc d_pcd dummy_last_day dummy_prev_year_days || partp: unem_rate, base(NOPART) || port: , base(NOPORT) || selection: weekend lunarill, ///
	base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)
	estimates save ${results}nlogit_5_FULL.ster
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A5
lrtest A4 A5, force
estadd scalar lr_p = r(p): A5
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A5
estadd scalar bic = S[1,6]: A5
estadd scalar aicc = S[1,7]: A5
estadd scalar caic = S[1,8]: A5
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
	estadd scalar perc1 = count1/_N*100: A5
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A5
restore


**************************************
*** Save results

esttab A1 A2 A3 A4 A5 using "G:\My Drive\Tables\Participation\nested_logit-${S_DATE}_FULL.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se noconstant drop(weekend _cons)
