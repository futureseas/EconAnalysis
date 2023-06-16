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
import delimited "Stata\dcm_data.csv"
gen id_obs = _n
gen const_r2 = 1


cmset fished_vessel_id time selection
** Set choice model database
cmset fished_haul selection
sort id_obs

** Estimate conditional logits (with alternative-specific constant)

qui cmclogit fished, base("No-Participation") 
scalar ll0 = e(ll)
/* preserve
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
restore
 */
eststo P1: cmclogit fished mean_rev_adj travel_cost wind_max_220_mh, base("No-Participation")
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P1

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
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P1
restore

eststo P2: cmclogit fished mean_rev_adj travel_cost wind_max_220_mh i.dummy_last_day, base("No-Participation") vce(cluster fished_vessel_id)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P2
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
	estadd scalar perc1 = count1/_N*100: P2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P2
restore

gen closured = msqdclosured + psdnclosured
tab closured

eststo P3: cmclogit fished mean_rev_adj travel_cost wind_max_220_mh i.closured, casevar(i.weekend) base("No-Participation") vce(cluster fished_vessel_id)
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P4
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
	estadd scalar perc1 = count1/_N*100: P3
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P3
restore

label variable mean_rev_adj "Expected revenue"
label variable travel_cost "Expected travel cost "
label variable wind_max_220_mh "Maximum wind (< 220km)"
label variable dummy_last_day "Choice has chosen last day"
label variable msqdclosure "Market squid closures"
label variable psdnclosure "Pacific sardine closure"
label variable msqdweekend "Market squid weekend closure"

esttab * using "${tables}preliminary_regressions.rtf", starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 perc2, fmt(0 3) labels("Observations" "McFadden R2" "Predicted choices" "(excluding no-participation)"))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se 
