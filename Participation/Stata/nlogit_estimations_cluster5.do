global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


** Import data
import delimited "C:\Data\PacFIN data\rdo_Stata_c5_full.csv"
save "C:\Data\PacFIN data\rdo_Stata_c5_full.dta", replace
use "C:\Data\PacFIN data\rdo_Stata_c5_full.dta", clear

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
asclogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured dummy_last_day d_d d_pd d_cd d_pcd, base("No-Participation") casevar(weekend) alternatives(selection) case(fished_haul) 

constraint 1 [/port]DCRB_tau = 1
constraint 2 [/port]CMCK_tau = 1

// nlogit   fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured dummy_last_day d_c d_d d_p d_pc d_pd  /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_id)
// estimates save ${results}nlogit_FULL_C5.ster, replace

// nlogit   fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured dummy_last_day d_c d_d d_p d_pc d_pd  /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1 2) vce(cluster fished_vessel_id)
// estimates save ${results}nlogit_FULL_C5_v2.ster, replace

// + Unemployment rate
// nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured dummy_last_day d_d d_pd d_cd unem_rate /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_id) ///
// 	from(start, skip)
// matrix start=e(b)
// estimates save ${results}nlogit_FULL_C5_v3.ster, replace

// + PCD variable
// nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured dummy_last_day unem_rate d_d d_pd d_cd d_pcd /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_id) ///
// 	from(start, skip)
// matrix start=e(b)
// estimates save ${results}nlogit_FULL_C5_v4.ster, replace

// + Contraint
// nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured dummy_last_day unem_rate d_d d_pd d_cd d_pcd /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1 2) vce(cluster fished_vessel_id) ///
// 	from(start, skip)
// matrix start=e(b)
// estimates save ${results}nlogit_FULL_C5_v5.ster, replace

// + Squid closure
// nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured dummy_last_day unem_rate d_d d_pd d_cd d_pcd msqdclosured /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1 2) vce(cluster fished_vessel_id) ///
// 	from(start, skip)
// matrix start=e(b)
// estimates save ${results}nlogit_FULL_C5_v6.ster, replace

// + WA closure (DIDN'T WORK)
// nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured dummy_last_day unem_rate d_d d_pd d_cd d_pcd msqdclosured waclosured /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1 2) vce(cluster fished_vessel_id) ///
// 	from(start, skip)
// matrix start=e(b)
// estimates save ${results}nlogit_FULL_C5_v7.ster, replace


estimates use ${results}nlogit_FULL_C5_v6.ster
matrix start=e(b)
nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured dummy_last_day unem_rate d_d d_pd d_cd d_pcd msqdclosured dcrbclosurewad /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) constraints(1 2) vce(cluster fished_vessel_id) ///
	from(start, skip)
matrix start=e(b)
estimates save ${results}nlogit_FULL_C5_v8.ster, replace

nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured dummy_last_day unem_rate d_d d_pd d_cd d_pcd msqdclosured dcrbclosurewad /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_id) ///
	from(start, skip)
matrix start=e(b)
estimates save ${results}nlogit_FULL_C5_v9.ster, replace


nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured dummy_last_day unem_rate d_d d_pd d_cd d_pcd msqdclosured dcrbclosurewad /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id) ///
	from(start, skip)
matrix start=e(b)
estimates save ${results}nlogit_FULL_C5_v10.ster, replace

nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured dummy_last_day d_d d_pd d_cd d_pcd msqdclosured dcrbclosurewad /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id) ///
	from(start, skip)
matrix start=e(b)
estimates save ${results}nlogit_FULL_C5_v11.ster, replace

*****************************************************************************************************************************

/* // This doesn't converge
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
nlogitgen partp = port(PART: MSQD | PSDN | ALBC | CMCK | NANC | DCRB, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul)

nlogit   fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured dummy_last_day d_c d_d d_p d_pc d_pd  /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_C5_v3.ster, replace
 */
*****************************************************************************************************************************


estimates use ${results}nlogit_FULL_C5.ster
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
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: B1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: B1
restore

*****************************************************************************************************************************
estimates use ${results}nlogit_FULL_C5_v2.ster
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
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: B2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: B2
restore

*****************************************************************************************************************************
estimates use ${results}nlogit_FULL_C5_v3.ster
estimates store B3
estimates describe B3
estimates replay B3
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B3
lrtest base B3, force
estadd scalar lr_p = r(p): B3
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: B3
estadd scalar bic = S[1,6]: B3
estadd scalar aicc = S[1,7]: B3
estadd scalar caic = S[1,8]: B3
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
	estadd scalar perc1 = count1/_N*100: B3
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: B3
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: B3
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: B3
restore

*****************************************************************************************************************************
estimates use ${results}nlogit_FULL_C5_v4.ster
estimates store B4
estimates describe B4
estimates replay B4
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B4
lrtest base B4, force
estadd scalar lr_p = r(p): B4
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: B4
estadd scalar bic = S[1,6]: B4
estadd scalar aicc = S[1,7]: B4
estadd scalar caic = S[1,8]: B4
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
	estadd scalar perc1 = count1/_N*100: B4
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: B4
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: B4
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: B4
restore

*****************************************************************************************************************************
estimates use ${results}nlogit_FULL_C5_v5.ster
estimates store B5
estimates describe B5
estimates replay B5
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B5
lrtest base B5, force
estadd scalar lr_p = r(p): B5
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: B5
estadd scalar bic = S[1,6]: B5
estadd scalar aicc = S[1,7]: B5
estadd scalar caic = S[1,8]: B5
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
	estadd scalar perc1 = count1/_N*100: B5
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: B5
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: B5
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: B5
restore

*****************************************************************************************************************************
estimates use ${results}nlogit_FULL_C5_v6.ster
estimates store B6
estimates describe B6
estimates replay B6
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B6
lrtest base B6, force
estadd scalar lr_p = r(p): B6
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: B6
estadd scalar bic = S[1,6]: B6
estadd scalar aicc = S[1,7]: B6
estadd scalar caic = S[1,8]: B6
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
	estadd scalar perc1 = count1/_N*100: B6
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: B6
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: B6
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: B6
restore

*****************************************************************************************************************************
// estimates use ${results}nlogit_FULL_C5_v7.ster
// estimates store B7
// estimates describe B7
// estimates replay B7
// di "R2-McFadden = " 1 - (e(ll)/ll0)
// estadd scalar r2 = 1 - (e(ll)/ll0): B7
// lrtest base B7, force
// estadd scalar lr_p = r(p): B7
// estat ic, all
// matrix S = r(S)
// estadd scalar aic = S[1,5]: B7
// estadd scalar bic = S[1,6]: B7
// estadd scalar aicc = S[1,7]: B7
// estadd scalar caic = S[1,8]: B7
// preserve
// 	qui predict phat
// 	by fished_haul, sort: egen max_prob = max(phat) 
// 	drop if max_prob != phat
// 	by fished_haul, sort: gen nvals = _n == 1 
// 	count if nvals
// 	dis _N
// 	gen selection_hat = 1
// 	egen count1 = total(fished)
// 	dis count1/_N*100 "%"
// 	estadd scalar perc1 = count1/_N*100: B7
// 	drop if selection == "No-Participation"
// 	egen count2 = total(fished)
// 	dis _N
// 	dis count2/_N*100 "%"
// 	estadd scalar perc2 = count2/_N*100: B7
// restore
// preserve
// 	replace dummy_last_day = 0
// 	replace dummy_prev_days = 0
// 	replace dummy_prev_year_days = 0
// 	replace dummy_prev_days_port = 0
// 	qui predict phat
// 	by fished_haul, sort: egen max_prob = max(phat) 
// 	drop if max_prob != phat
// 	by fished_haul, sort: gen nvals = _n == 1 
// 	count if nvals
// 	dis _N
// 	gen selection_hat = 1
// 	egen count1 = total(fished)
// 	dis count1/_N*100 "%"
// 	estadd scalar perc3 = count1/_N*100: B7
// 	drop if selection == "No-Participation"
// 	egen count2 = total(fished)
// 	dis _N
// 	dis count2/_N*100 "%"
// 	estadd scalar perc4 = count2/_N*100: B7
// restore


*****************************************************************************************************************************
*** Save model

esttab  B1 B2 B3 B4 B5 B6 using "G:\My Drive\Tables\Participation\nested_logit_FULL_${S_DATE}_C5.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 perc3 perc4 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" ///
			"Predicted choices (%) -- No SD" "- Excl. No-Participation (%) -- No SD" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se noconstant
 



