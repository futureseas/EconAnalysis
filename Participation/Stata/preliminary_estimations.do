global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global logs "${results}Logs\"
global mwtp "${path}WTP estimation\"
cd $path

clear all
cap log close
	log using ${logs}preliminary, text replace

** Import data
import delimited "C:\Data\PacFIN data\rdo_Stata_c4.csv"
replace mean_price = mean_price / 1000
gen id_obs = _n
gen const_r2 = 1


* Model set-up
global dclosure = "all"

if "$dclosure" == "1"  {
	global closure = "if psdntotalclosure == 1"
	global vars = "exp_revenue wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_missing_all msqdweekend"
	global vars_sdm = "mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_missing_all msqdweekend res" 
	global case = " "
} 
else if "$dclosure" == "0" {
	global closure = "if psdntotalclosure == 0"
	global vars = "exp_revenue wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_missing_all psdnclosured2 psdntotalclosured msqdweekend"
	global vars_sdm = "mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_missing_all psdnclosured2 psdntotalclosured msqdweekend res" 
	global case = " "
}
else if "$dclosure" == "all" {
	global closure = " "
	global vars = "exp_revenue wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_missing_all psdnclosured2 psdntotalclosured msqdweekend" 
	global vars_sdm = "mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_missing_all psdnclosured2 psdntotalclosured msqdweekend res" 
	global case = " " // at the end add: lunarill
}


** Add addtional variables
preserve
 import delimited "Stata\dbp_month_Stata_c4.csv", clear
 tempfile dbp_month
 save dbp_month, replace
restore
merge m:1 selection set_month using "dbp_month.dta"
drop if _merge == 2
replace hist_selection = 0 if _merge == 1
drop _merge
gen exp_revenue = mean_price * mean_avail
gen exp_cost_cog = diesel_price * dist_to_cog 
gen exp_cost_catch_area = diesel_price * dist_port_to_catch_area_zero
gen d_missing_all = d_missing_d
replace d_missing_all = 1 if d_missing == 1
gen psdnclosured2 = psdnclosured - psdntotalclosured
gen psdnclosure2 = psdnclosure - psdntotalclosure
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 
// encode species, gen(nspecies)


** Instrument price
egen selectionf = group(selection), label
reg mean_price i.selectionf mean_avail wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_missing_all psdnclosured2 psdntotalclosured msqdweekend ///
	diesel_price pricefishmealafi, noconstant
predict res, residuals


******************************
** Estimate conditional logits (with alternative-specific constant)

** Set choice model database
// cmset fished_haul selection
cmset fished_vessel_id time selection
sort id_obs

*** Only-constant model
qui cmclogit fished, base("No-Participation") 
scalar ll0 = e(ll)
estimates store base

// matrix s2=e(b)
// mat s = start[1, 1..5]
// forvalues i = 6(1)45 {
// 	display `i'
// 	mat s = s, s2[1,`i']
// 	mat s = s, start[1,`i']
// }

/* *** Preferred model
qui cmclogit fished $vars $closure, base("No-Participation") noconstant
matrix sA=e(b)

* No relevant variables: waclosure waclosured ddieselstate msqdclosured
eststo A0: cmclogit fished $vars $closure, ///
	casevar($case) base("No-Participation") from(sA, skip)
matrix sA=e(b)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A0
lrtest base A0, force
estadd scalar lr_p = r(p): A0
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A0
estadd scalar bic = S[1,6]: A0
estadd scalar aicc = S[1,7]: A0
estadd scalar caic = S[1,8]: A0
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
	estadd scalar perc1 = count1/_N*100: A0
restore


	

***********************************************************
*** Include state dependency (models to present on paper)

eststo A1: cmclogit fished  $vars i.hist_selection $closure, ///
	casevar($case) base("No-Participation") from(sA, skip)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A1
lrtest A0 A1, force
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
restore

eststo A2: cmclogit fished  $vars i.dummy_prev_days i.dummy_prev_year_days $closure, ///
	casevar($case) base("No-Participation") from(sA, skip)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A2
lrtest A0 A1, force
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
restore

eststo A3: cmclogit fished  $vars i.dummy_last_day i.dummy_prev_year_days $closure, ///
	casevar($case) base("No-Participation") from(sA, skip)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A3
lrtest A0 A1, force
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
restore
 */

* Out: i.dummy_clust_prev_days i.dummy_prev_days_port


*** Using SDM and price separately
qui cmclogit fished $vars_sdm $closure, base("No-Participation") noconstant
matrix sB=e(b)

eststo B0: cmclogit fished  $vars_sdm $closure, ///
	casevar($case) base("No-Participation") from(sB, skip) vce(bootstrap)
matrix sB=e(b)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B0
lrtest base B0, force
estadd scalar lr_p = r(p): B0
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: B0
estadd scalar bic = S[1,6]: B0
estadd scalar aicc = S[1,7]: B0
estadd scalar caic = S[1,8]: B0
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
	estadd scalar perc1 = count1/_N*100: B0
restore

eststo B2: cmclogit fished  $vars_sdm i.dummy_prev_days i.dummy_prev_year_days $closure, ///
	casevar($case) base("No-Participation") from(sB, skip) vce(bootstrap)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B2
lrtest B0 B2, force
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
restore

********************************************************************
label variable mean_avail "Expected availability"
label variable exp_revenue "Expected revenue"
label variable mean_price "Expected price"
label variable diesel_price "Expected diesel price"
label variable wind_max_220_mh "Maximum wind (< 220km)"
label variable d_missing_all "Binary: Missing information"
label variable d_missing "Binary: Missing availability"
label variable d_missing_p "Binary: Missing price"
label variable dist_port_to_catch_area "Distance to catch area"
label variable dist_port_to_catch_area_zero "Distance to catch area"
label variable d_missing_d "Binary: Missing distance"
label variable lat_cg "Latitudinal Center of Gravity"
label variable unem_rate "State unemployment rate"
label variable dist_to_cog "Distance to Center of Gravity" 
********************************************************************
label variable ddieselstate "Binary: Diesel price by state" 
label variable psdnclosured "Binary: PSDN Closure x PSDN" 
label variable psdnclosured2 "Binary: PSDN Seasonal Closure x PSDN chosen" 
label variable psdntotalclosured "Binary: PSDN Total Closure x PSDN chosen" 
label variable msqdclosured "Binary: MSQD Closure"
label variable msqdweekend  "Binary: Weekend x MSQD chosen"
********************************************************************
label variable dummy_last_day "Alternative has been chosen last day"
label variable dummy_prev_days "Alternative has been chosen during the last 30 days"
label variable dummy_prev_days_port "Port has been chosen during the last 30 days"
label variable dummy_prev_year_days "Alternative has been chosen during the last 30 days (previous year)"
label variable dummy_clust_prev_days "Alternative has been chosen during the last 30 days by any member of the fleet"
label variable hist_selection "Alternative has been historically chosen during the month (>20% revenue)"
********************************************************************


esttab B0 B2 using "G:\My Drive\Tables\Participation\preliminary_regressions_participation_state_dep-${S_DATE}-${dclosure}-IV_BS.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels drop($case _cons) se noconstant


eststo B1: cmclogit fished  $vars_sdm i.hist_selection $closure, ///
	casevar($case) base("No-Participation") from(sB, skip)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B1
lrtest B0 B1, force
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
restore

eststo B3: cmclogit fished  $vars_sdm i.dummy_last_day i.dummy_prev_year_days $closure, ///
	casevar($case) base("No-Participation") from(sB, skip)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B3
lrtest B0 B3, force
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
restore



********************************************************************
label variable mean_avail "Expected availability"
label variable exp_revenue "Expected revenue"
label variable mean_price "Expected price"
label variable diesel_price "Expected diesel price"
label variable wind_max_220_mh "Maximum wind (< 220km)"
label variable d_missing_all "Binary: Missing information"
label variable d_missing "Binary: Missing availability"
label variable d_missing_p "Binary: Missing price"
label variable dist_port_to_catch_area "Distance to catch area"
label variable dist_port_to_catch_area_zero "Distance to catch area"
label variable d_missing_d "Binary: Missing distance"
label variable lat_cg "Latitudinal Center of Gravity"
label variable unem_rate "State unemployment rate"
label variable dist_to_cog "Distance to Center of Gravity" 
********************************************************************
label variable ddieselstate "Binary: Diesel price by state" 
label variable psdnclosured "Binary: PSDN Closure x PSDN" 
label variable psdnclosured2 "Binary: PSDN Seasonal Closure x PSDN chosen" 
label variable psdntotalclosured "Binary: PSDN Total Closure x PSDN chosen" 
label variable msqdclosured "Binary: MSQD Closure"
label variable msqdweekend  "Binary: Weekend x MSQD chosen"
********************************************************************
label variable dummy_last_day "Alternative has been chosen last day"
label variable dummy_prev_days "Alternative has been chosen during the last 30 days"
label variable dummy_prev_days_port "Port has been chosen during the last 30 days"
label variable dummy_prev_year_days "Alternative has been chosen during the last 30 days (previous year)"
label variable dummy_clust_prev_days "Alternative has been chosen during the last 30 days by any member of the fleet"
label variable hist_selection "Alternative has been historically chosen during the month (>20% revenue)"
********************************************************************


esttab B0 B1 B2 B3 using "G:\My Drive\Tables\Participation\preliminary_regressions_participation_state_dep-${S_DATE}-${dclosure}-IV.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels drop($case _cons) se noconstant
