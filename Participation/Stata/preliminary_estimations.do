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

*** SDM by species; Price by species; Center of gravity by port: Distance by lenght; Weather alone.


clear all
cap log close
	log using ${logs}preliminary, text replace

** Import data
import delimited "Stata\rdo_Stata_c4.csv"
gen id_obs = _n
gen const_r2 = 1


** Set choice model database
cmset fished_vessel_id time selection
// cmset fished_haul selection
sort id_obs

** Estimate conditional logits (with alternative-specific constant)

qui cmclogit fished, base("No-Participation") 
scalar ll0 = e(ll)


eststo P1: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing d_missing_p, base("No-Participation")
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

eststo P2: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing d_missing_p i.dummy_last_day, base("No-Participation")
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

eststo P4: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing d_missing_p i.dummy_last_day dist_port_to_catch_area_zero d_missing_d, base("No-Participation") vce(cluster fished_vessel_id)
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
	estadd scalar perc1 = count1/_N*100: P4
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P4
restore

eststo P5: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing d_missing_p i.dummy_last_day

 dist_port_to_catch_area_zero
 lat_cg



label variable mean_avail "Expected availability"
label variable mean_price "Expected price"
label variable diesel_price "Expected diesel price"
label variable wind_max_220_mh "Maximum wind (< 220km)"
label variable dummy_last_day "Choice has chosen last day"
label variable d_missing "Binary: Missing availability"
label variable d_missing_p "Binary: Missing price"
label variable dist_port_to_catch_area "Distance to catch area"
label variable dist_port_to_catch_area_zero "Distance to catch area"
label variable d_missing_d "Binary: Missing distance"
label variable lat_cg "Latitudinal Center of Gravity"


esttab * using "${tables}preliminary_regressions.rtf", starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 perc2, fmt(0 3) labels("Observations" "McFadden R2" "Predicted choices" "(excluding no-participation)"))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se 
