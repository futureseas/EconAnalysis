
***************************************
**** Should I stay or should I go? ****
***************************************

** Work to do: 
* - Compute mean_catch for sardine, anchovy, squid, mackerrrel and herring. Constant and sdm explain catch.
* - Add predicted choices without "No-participation"

global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global logs "${results}Logs\"
global mwtp "${path}WTP estimation\"
cd $path

clear all
// cap log close
// 	log using ${logs}preliminary, text replace

** Import data
import delimited "C:\Data\PacFIN data\rdo_Stata_c4_nhauls5.csv"
replace mean_price = mean_price / 1000
replace mean_price2 = mean_price2 / 1000
replace mean_catch = mean_catch / 1000
replace mean_catch2 = mean_catch2 / 1000
replace pricefishmealafi = pricefishmealafi / 1000
gen exp_cost_cog = diesel_price * dist_to_cog 
gen exp_cost_catch_area = diesel_price * dist_port_to_catch_area_zero
gen id_obs = _n
gen const_r2 = 1

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
gen psdnclosured2 = psdnclosured - psdntotalclosured
gen psdnclosure2 = psdnclosure - psdntotalclosure
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 
replace d_missing_p = d_missing_p2 if mean_price > 50
replace mean_price = mean_price2 if mean_price > 50
gen exp_revenue  = mean_price  * mean_avail
gen exp_revenue2 = mean_price2 * mean_avail
egen species_int = group(species), label
gen d_p = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 0) 
gen d_c = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 0) 
gen d_d = (d_missing_p2 == 0 & d_missing == 0 & d_missing_d == 1) 
gen d_pc = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 0) 
gen d_pd = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 1) 
gen d_cd = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 1) 
gen d_pcd = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 1) 
sum d_p d_c d_d d_pc d_pd d_cd d_pcd
// encode species, gen(nspecies)

** Instrument price
egen selectionf = group(selection), label
cmset fished_vessel_id time selection

reg  mean_price2 i.selectionf mean_catch wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
				d_c d_d d_cd d_pd d_pcd psdnclosured2 psdntotalclosured msqdweekend /// 
				pricefishmealafi diesel_price, noconstant
predict res, residual


********************************************************************
* Model set-up
global closure = " "
global vars_sdm = "mean_catch mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_c d_d d_pd d_cd d_pcd psdnclosured2 psdntotalclosured msqdweekend res" 
global case_sdm = " "


********************************************************************
** Create labels

label variable mean_catch "Expected catch"
label variable exp_revenue "Expected revenue"
label variable exp_revenue2 "Expected revenue (just MA)"
label variable mean_price "Expected price"
label variable mean_price2 "Expected price (just MA)"
label variable diesel_price "Expected diesel price"
label variable wind_max_220_mh "Maximum wind (< 220km)"
label variable d_missing "Binary: Missing availability"
label variable d_missing_cpue "Binary: Missing availability (CPUE)"
label variable d_missing_p  "Binary: Missing price"
label variable d_missing_p2 "Binary: Missing price"
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

*** Using SDM and price separately
cmclogit fished $vars_sdm $closure, base("No-Participation") noconstant
matrix sB=e(b)

eststo B0: cmclogit fished  $vars_sdm $closure, ///
	casevar($case_sdm) base("No-Participation") from(sB, skip) vce(bootstrap)
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

eststo B1: cmclogit fished  $vars_sdm i.dummy_prev_days i.dummy_prev_year_days $closure, ///
	casevar($case_sdm) base("No-Participation") from(sB, skip) vce(bootstrap)
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

esttab B0 B1 using "G:\My Drive\Tables\Participation\preliminary_regressions_participation_state_dep-${S_DATE}-${dclosure}-IV_catch_BS.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels drop($case_sdm _cons) se noconstant


