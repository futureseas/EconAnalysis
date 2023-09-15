global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global logs "${results}Logs\"
global mwtp "${path}WTP estimation\"
cd $path


*    Preilimnary estimations (before contraction mapping)    *
**************************************************************

clear all
cap log close
	log using ${logs}preliminary, text replace

** Import data
import delimited "Stata\rdo_Stata_c4.csv"
replace mean_price = mean_price / 1000
gen id_obs = _n
gen const_r2 = 1

** Merge historical selection
preserve
 import delimited "Stata\dbp_month_Stata_c4.csv", clear
 tempfile dbp_month
 save dbp_month, replace
restore

merge m:1 selection set_month using "dbp_month.dta"
drop if _merge == 2
replace hist_selection = 0 if _merge == 1
drop _merge

** Set choice model database
cmset fished_vessel_id time selection
// cmset fished_haul selection
sort id_obs



******************************
*** Calculate utilty fishery

* Preferred model (see below) 
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 

encode species, gen(nspecies)

cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend, base("No-Participation")

// set emptycells drop
// margins, at(mean_avail=gen(mean_avail)) at(mean_avail=gen(mean_avail_modified))
// marginsplot, xdimension(_outcome)

* Total cases = 19,832

preserve
	replace mean_avail = 1 if species == "MSQD"
	predict phat, pr
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	tab selection
	gen selection_hat = 1
	egen freq = total(selection_hat), by(selection)
	keep selection freq
	sort selection
	quietly by selection:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	drop dup
	egen tot = total(freq)
	gen perc1 = freq/tot
	drop tot freq
	tempfile simulated1
	save simulated1, replace
restore

preserve
	replace mean_avail = 0 if species == "MSQD"
	predict phat, pr
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	tab selection
	gen selection_hat = 1
	egen freq = total(selection_hat), by(selection)
	keep selection freq
	sort selection
	quietly by selection:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	drop dup
	egen tot = total(freq)
	gen perc2 = freq/tot
	drop tot freq
	tempfile simulated2
	save simulated2, replace
restore

preserve
	program change_coeff, eclass
		matrix betass = e(b)
		matrix list betass
		scalar xx = (betass[1,14] + betass[1,15] + betass[1,16] + betass[1,20] + betass[1,31] + betass[1,35] + betass[1,36] + betass[1,39] + betass[1,46])/(47-12-9)
		forvalues i=13(1)47 {
			matrix betass[1,`i'] = betass[1,`i'] + scalar(`=xx')
		}
		matrix betass[1,14] = 0
		matrix betass[1,15] = 0
		matrix betass[1,16] = 0
		matrix betass[1,20] = 0
		matrix betass[1,31] = 0
		matrix betass[1,35] = 0
		matrix betass[1,36] = 0
		matrix betass[1,39] = 0
		matrix betass[1,46] = 0
		ereturn repost b = betass, rename
	end
	change_coeff
	matrix list e(b)
	replace mean_avail = 0 if species == "MSQD"
	predict phat, pr
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	tab selection
	gen selection_hat = 1
	egen freq = total(selection_hat), by(selection)
	keep selection freq
	sort selection
	quietly by selection:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	drop dup
	egen tot = total(freq)
	gen perc3 = freq/tot
	drop tot freq
	tempfile simulated3
	save simulated3, replace
restore

cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend, base("No-Participation")

matrix betass = e(b)
matrix list betass

	predict phat, pr
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	tab selection
	gen selection_hat = 1
	egen freq = total(selection_hat), by(selection)
	keep selection freq
	sort selection
	quietly by selection:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	drop dup
	egen tot = total(freq)
	gen perc = freq/tot
	drop tot freq

/* keep if fished == 1
gen selection_hat = 1
egen freq = total(selection_hat), by(selection)
keep selection freq
sort selection
quietly by selection:  gen dup = cond(_N==1,0,_n)
drop if dup > 1
drop dup
egen tot = total(freq)
gen perc = freq/tot
drop tot freq */

merge 1:1 selection using "simulated1.dta", nogen keep(master match) 
merge 1:1 selection using "simulated2.dta", nogen keep(master match) 
merge 1:1 selection using "simulated3.dta", nogen keep(master match) 
replace perc  = 0 if perc == .
replace perc1 = 0 if perc1 == .
replace perc2 = 0 if perc2 == .
replace perc3 = 0 if perc3 == .

export excel using "G:\My Drive\Tables\Participation\Simulated_shares.xlsx", replace

******************************
** Estimate conditional logits (with alternative-specific constant)

*** Only-constant model
qui cmclogit fished, base("No-Participation") 
scalar ll0 = e(ll)
estimates store base

*** Preferred model

* Variables that do not converge: dcpue dparticipate
* No relevant variables: waclosure waclosured

eststo P1: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend, base("No-Participation")
estimates store P1
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P1
lrtest P1 base, force
estadd scalar lr_p = r(p): P1
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P1
estadd scalar bic = S[1,6]: P1
estadd scalar aicc = S[1,7]: P1
estadd scalar caic = S[1,8]: P1
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
	estadd scalar perc1 = count1/_N*100: P1
restore

***********************************************************
*** Include state dependency (models to present on paper)

eststo B1: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend i.dummy_last_day, base("No-Participation")
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B1
lrtest B1 P1, force
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

eststo B2: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend i.dummy_prev_days, base("No-Participation")
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B2
lrtest B2 P1, force
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

eststo B3: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend i.dummy_prev_year_days, base("No-Participation")
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B3
lrtest B3 P1, force
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

eststo B4: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend i.dummy_clust_prev_days, base("No-Participation")
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B4
lrtest B4 P1, force
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
restore


eststo B5: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend i.hist_selection, base("No-Participation")
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): B5
lrtest B5 P1, force
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
restore

********************************************************************
label variable mean_avail "Expected availability"
label variable mean_price "Expected price"
label variable diesel_price "Expected diesel price"
label variable wind_max_220_mh "Maximum wind (< 220km)"
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
label variable psdnclosured "Binary: PSDN Closure" 
label variable msqdclosured "Binary: MSQD Closure"
label variable msqdweekend  "Binary: MSQD Weekend"
********************************************************************
label variable dummy_last_day "Alternative has been chosen last day"
label variable dummy_prev_days "Alternative has been chosen during the last 30 days"
label variable dummy_prev_year_days "Alternative has been chosen during the last 30 days (previous year)"
label variable dummy_clust_prev_days "Alternative has been chosen during the last 30 days by any member of the fleet"
label variable hist_selection "Alternative has been historically chosen during the month (>20% revenue)"
********************************************************************

esttab P1 B1 B2 B3 B4 B5 using "${tables}preliminary_regressions_participation_state_dep_2023_09_14.rtf", starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 lr_p aic bic aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "LR-test" "AIC" "BIC" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se  noconstant



