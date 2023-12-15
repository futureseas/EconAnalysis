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
merge m:1 selection set_month using "dbp_month.dta"
drop if _merge == 2
replace hist_selection = 0 if _merge == 1
drop _merge
gen psdnclosured2 = psdnclosured - psdntotalclosured
gen psdnclosure2 = psdnclosure - psdntotalclosure
replace mean_price = mean_price / 1000
replace mean_price2 = mean_price2 / 1000
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
label variable d_missing_p2 "Binary: Missing price (30 days)"
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

keep if selection == "SBA-MSQD" | ///
		selection == "MNA-MSQD" | ///
		selection == "LAA-MSQD" | ///
		selection == "MNA-NANC" | ///
		selection == "SFA-MSQD" | ///
		selection == "LAA-CMCK" | ///
		selection == "MNA-PSDN" | ///
		selection == "LAA-PSDN" | ///
		selection == "MRA-MSQD" | ///
		selection == "LAA-NANC" | ///
		selection == "SBA-CMCK" | ///
		selection == "SFA-NANC" | ///
		selection == "LAA-YTNA" | ///
        selection == "No-Participation"  

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
	CMCK: LAA-CMCK | SBA-CMCK, ///
	MSQD: LAA-MSQD | MNA-MSQD | MRA-MSQD | SBA-MSQD | SFA-MSQD, ///
	PSDN: LAA-PSDN | MNA-PSDN, ///
	NANC: LAA-NANC | MNA-NANC | SFA-NANC, ///
	TUNA: LAA-YTNA, ///
    NOPORT: No-Participation)
nlogitgen partp = port(PART: CMCK | MSQD | PSDN | NANC | TUNA, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 

// *** Estimate model (base)
// estimates use ${results}nlogit_FULL_v2.ster

// *** Estimate model (with weekend by species)
// estimates use ${results}nlogit_FULL_v4.ster

// *** Estimate model (with price * TUNA)
// estimates use ${results}nlogit_FULL_v5.ster

// *** Estimate model (with clustered SE)
// estimates use ${results}nlogit_FULL_v6.ster

// *** Estimate model (with weekend by species + clustered SE)
// estimates use ${results}nlogit_FULL_v7.ster

// *** Estimate model (using catch)
// estimates use ${results}nlogit_FULL_v8.ster

// *** Estimate model (using SDM and lunar illumination)
// estimates use ${results}nlogit_FULL_v9.ster

// *** Estimate model (using catch and lunar illumination)
// estimates use ${results}nlogit_FULL_v10.ster

// *** Estimate model (using SDM and price 30 days)
// estimates use ${results}nlogit_FULL_v11.ster

// *** Estimate model (using catch and price 30 days)
// estimates use ${results}nlogit_FULL_v12.ster

// *** Estimate model (using SDM and price 30 days + lunar)
// estimates use ${results}nlogit_FULL_v13.ster

// *** Estimate model (using catch and price 30 days + lunar)
// estimates use ${results}nlogit_FULL_v14.ster

// *** Estimate model (no SD)
// estimates use ${results}nlogit_FULL_v15.ster

// *** Estimate model (no SD catch)
// estimates use ${results}nlogit_FULL_v16.ster


** Keep using mean avail -- Easy to explain -- but add specific dummies for missing values


nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured unem_rate dummy_last_day d_d d_p d_cd d_pd d_pc d_pcd  /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_F1.ster, replace
estimates store F1
estimates describe F1
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): F1
lrtest base F1, force
estadd scalar lr_p = r(p): F1
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: F1
estadd scalar bic = S[1,6]: F1
estadd scalar aicc = S[1,7]: F1
estadd scalar caic = S[1,8]: F1
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
	estadd scalar perc1 = count1/_N*100: F1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: F1
restore
preserve
	replace dummy_last_day = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: F1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: F1
restore

nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured unem_rate dummy_last_day dummy_prev_days d_d d_p d_cd d_pc d_pd d_pcd  /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_F2.ster, replace
estimates store F2
estimates describe F2
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): F2
lrtest F2 F2, force
estadd scalar lr_p = r(p): F2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: F2
estadd scalar bic = S[1,6]: F2
estadd scalar aicc = S[1,7]: F2
estadd scalar caic = S[1,8]: F2
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
	estadd scalar perc1 = count1/_N*100: F2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: F2
restore
preserve
	replace dummy_last_day = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: F2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: F2
restore

gen exp_value = mean_price*mean_avail
label variable exp_value "Expected value"
nlogit fished exp_value wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured unem_rate dummy_last_day dummy_prev_days d_d d_p d_cd d_pc d_pd d_pcd  /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_F3.ster, replace
estimates store F3
estimates describe F3
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): F3
lrtest F3 F3, force
estadd scalar lr_p = r(p): F3
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: F3
estadd scalar bic = S[1,6]: F3
estadd scalar aicc = S[1,7]: F3
estadd scalar caic = S[1,8]: F3
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
	estadd scalar perc1 = count1/_N*100: F3
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: F3
restore
preserve
	replace dummy_last_day = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: F3
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: F3
restore


replace d_c   = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 0) 
replace d_d   = (d_missing_p2 == 0 & d_missing == 0 & d_missing_d == 1) 
replace d_p   = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 0) 
replace d_cd  = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 1) 
replace d_pc  = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 0) 
replace d_pd  = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 1) 
replace d_pcd = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 1) 

nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured unem_rate dummy_last_day d_d d_cd d_pd d_pcd  /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_F4.ster, replace
estimates store F4
estimates describe F4
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): F4
lrtest base F4, force
estadd scalar lr_p = r(p): F4
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: F4
estadd scalar bic = S[1,6]: F4
estadd scalar aicc = S[1,7]: F4
estadd scalar caic = S[1,8]: F4
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
	estadd scalar perc1 = count1/_N*100: F4
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: F4
restore
preserve
	replace dummy_last_day = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: F4
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: F4
restore

nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		psdnclosured unem_rate dummy_last_day dummy_prev_days d_c d_d d_p d_cd d_pc d_pd d_pcd  /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_F5.ster, replace
estimates store F5
estimates describe F5
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): F5
lrtest F5 F5, force
estadd scalar lr_p = r(p): F5
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: F5
estadd scalar bic = S[1,6]: F5
estadd scalar aicc = S[1,7]: F5
estadd scalar caic = S[1,8]: F5
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
	estadd scalar perc1 = count1/_N*100: F5
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: F5
restore
preserve
	replace dummy_last_day = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: F5
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: F5
restore

esttab  F1 F2 F3 F4 F5 using "G:\My Drive\Tables\Participation\nested_logit_FULL_${S_DATE}_Final.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 perc3 perc4 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" ///
			"Predicted choices (%) -- No SD" "- Excl. No-Participation (%) -- No SD" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se noconstant

// ** Note: Model with ports nest and participation in CPS or Tuna do not converge. Model with YTNA within LAA not consistent with RUM.

** ADD TUNA * MEAN_PRICE if using regressions
