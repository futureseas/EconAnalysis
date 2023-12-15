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

*** Estimate model (with weekend by species + clustered SE)
estimates use ${results}nlogit_FULL_v7.ster
estimates store A7
estimates describe A7
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A7
lrtest base A7, force
estadd scalar lr_p = r(p): A7
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A7
estadd scalar bic = S[1,6]: A7
estadd scalar aicc = S[1,7]: A7
estadd scalar caic = S[1,8]: A7
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
	estadd scalar perc1 = count1/_N*100: A7
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A7
restore

*** Estimate model (using catch)
nlogit fished mean_catch mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		d_missing_d d_missing_p d_missing_catch psdnclosured unem_rate dummy_last_day  /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_v8.ster, replace
estimates use ${results}nlogit_FULL_v8.ster
estimates store A8
estimates describe A8
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A8
lrtest base A8, force
estadd scalar lr_p = r(p): A8
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A8
estadd scalar bic = S[1,6]: A8
estadd scalar aicc = S[1,7]: A8
estadd scalar caic = S[1,8]: A8
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
	estadd scalar perc1 = count1/_N*100: A8
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A8
restore


*** Estimate model (using SDM and lunar illumination)
nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		d_missing_d d_missing_p psdnclosured unem_rate dummy_last_day /// 
		|| partp: , base(NOPART) || port: weekend lunarill, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_v9.ster, replace
estimates use ${results}nlogit_FULL_v9.ster
estimates store A9
estimates describe A9
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A9
lrtest A7 A9, force
estadd scalar lr_p = r(p): A9
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A9
estadd scalar bic = S[1,6]: A9
estadd scalar aicc = S[1,7]: A9
estadd scalar caic = S[1,8]: A9
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
	estadd scalar perc1 = count1/_N*100: A9
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A9
restore

*** Estimate model (using catch and lunar illumination)
nlogit fished mean_catch mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		d_missing_d d_missing_p d_missing_catch psdnclosured unem_rate dummy_last_day  /// 
		|| partp: , base(NOPART) || port: weekend lunarill, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id)
estimates save ${results}nlogit_FULL_v10.ster, replace
estimates use ${results}nlogit_FULL_v10.ster
estimates store A10
estimates describe A10
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A10
lrtest A8 A10, force
estadd scalar lr_p = r(p): A10
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A10
estadd scalar bic = S[1,6]: A10
estadd scalar aicc = S[1,7]: A10
estadd scalar caic = S[1,8]: A10
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
	estadd scalar perc1 = count1/_N*100: A10
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A10
restore


*** Save table

esttab  A7 A8 A9 A10 using "G:\My Drive\Tables\Participation\nested_logit_FULL_${S_DATE}_4.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se noconstant

** Note: Model with ports nest and participation in CPS or Tuna do not converge. Model with YTNA within LAA not consistent with RUM.

