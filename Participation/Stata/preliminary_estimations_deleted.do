eststo P1: cmclogit fished mean_avail, base("No-Participation")
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
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P1
restore

eststo P2: cmclogit fished mean_avail mean_price, base("No-Participation")
estimates store P2
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P2
lrtest P2 P1, force
estadd scalar lr_p = r(p): P2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P2
estadd scalar bic = S[1,6]: P2
estadd scalar aicc = S[1,7]: P2
estadd scalar caic = S[1,8]: P2
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

eststo P3: cmclogit fished mean_avail mean_price diesel_price, base("No-Participation")
estimates store P3
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P3
lrtest P3 P2, force
estadd scalar lr_p = r(p): P3
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P3
estadd scalar bic = S[1,6]: P3
estadd scalar aicc = S[1,7]: P3
estadd scalar caic = S[1,8]: P3
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

eststo P4: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh, base("No-Participation")
estimates store P4
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P4
lrtest P4 P3, force
estadd scalar lr_p = r(p): P4
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P4
estadd scalar bic = S[1,6]: P4
estadd scalar aicc = S[1,7]: P4
estadd scalar caic = S[1,8]: P4
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

eststo P5: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing, base("No-Participation")
estimates store P5
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P5
lrtest P5 P4, force
estadd scalar lr_p = r(p): P5
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P5
estadd scalar bic = S[1,6]: P5
estadd scalar aicc = S[1,7]: P5
estadd scalar caic = S[1,8]: P5
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
	estadd scalar perc1 = count1/_N*100: P5
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P5
restore

eststo P6: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing d_missing_p, base("No-Participation")
estimates store P6
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P6
lrtest P6 P5, force
estadd scalar lr_p = r(p): P6
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P6
estadd scalar bic = S[1,6]: P6
estadd scalar aicc = S[1,7]: P6
estadd scalar caic = S[1,8]: P6
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
	estadd scalar perc1 = count1/_N*100: P6
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P6
restore

eststo P7: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero, base("No-Participation")
estimates store P7
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P7
lrtest P7 P5, force
estadd scalar lr_p = r(p): P7
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P7
estadd scalar bic = S[1,6]: P7
estadd scalar aicc = S[1,7]: P7
estadd scalar caic = S[1,8]: P7
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
	estadd scalar perc1 = count1/_N*100: P7
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P7
restore

eststo P8: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d, base("No-Participation")
estimates store P8
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P8
lrtest P8 P7, force
estadd scalar lr_p = r(p): P8
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P8
estadd scalar bic = S[1,6]: P8
estadd scalar aicc = S[1,7]: P8
estadd scalar caic = S[1,8]: P8
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
	estadd scalar perc1 = count1/_N*100: P8
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P8
restore

eststo P9: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d lat_cg, base("No-Participation")
estimates store P9
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P9
lrtest P9 P8, force
estadd scalar lr_p = r(p): P9
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P9
estadd scalar bic = S[1,6]: P9
estadd scalar aicc = S[1,7]: P9
estadd scalar caic = S[1,8]: P9
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
	estadd scalar perc1 = count1/_N*100: P9
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P9
restore

eststo P10: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d unem_rate, base("No-Participation")
estimates store P10
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P10
lrtest P10 P8, force
estadd scalar lr_p = r(p): P10
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P10
estadd scalar bic = S[1,6]: P10
estadd scalar aicc = S[1,7]: P10
estadd scalar caic = S[1,8]: P10
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
	estadd scalar perc1 = count1/_N*100: P10
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: P10
restore

****************************************************
****************************************************

eststo P1: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d, base("No-Participation")
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

eststo P2: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog, base("No-Participation")
estimates store P2
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P2
lrtest P2 P1, force
estadd scalar lr_p = r(p): P2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P2
estadd scalar bic = S[1,6]: P2
estadd scalar aicc = S[1,7]: P2
estadd scalar caic = S[1,8]: P2
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
restore

eststo P3: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog dprice30, base("No-Participation")
estimates store P3
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P3
lrtest P3 P2, force
estadd scalar lr_p = r(p): P3
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P3
estadd scalar bic = S[1,6]: P3
estadd scalar aicc = S[1,7]: P3
estadd scalar caic = S[1,8]: P3
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
restore

eststo P4: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ddieselstate, base("No-Participation")
estimates store P4
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P4
lrtest P4 P2, force
estadd scalar lr_p = r(p): P4
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P4
estadd scalar bic = S[1,6]: P4
estadd scalar aicc = S[1,7]: P4
estadd scalar caic = S[1,8]: P4
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
restore

eststo P5: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ddieselstate dcpue90, base("No-Participation")
estimates store P5
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P5
lrtest P5 P4, force
estadd scalar lr_p = r(p): P5
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P5
estadd scalar bic = S[1,6]: P5
estadd scalar aicc = S[1,7]: P5
estadd scalar caic = S[1,8]: P5
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
	estadd scalar perc1 = count1/_N*100: P5
restore

eststo P6: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ddieselstate psdnclosured, base("No-Participation")
estimates store P6
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P6
lrtest P6 P4, force
estadd scalar lr_p = r(p): P6
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P6
estadd scalar bic = S[1,6]: P6
estadd scalar aicc = S[1,7]: P6
estadd scalar caic = S[1,8]: P6
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
	estadd scalar perc1 = count1/_N*100: P6
restore

eststo P7: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ddieselstate psdnclosured msqdclosured , base("No-Participation")
estimates store P7
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P7
lrtest P7 P6, force
estadd scalar lr_p = r(p): P7
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P7
estadd scalar bic = S[1,6]: P7
estadd scalar aicc = S[1,7]: P7
estadd scalar caic = S[1,8]: P7
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
	estadd scalar perc1 = count1/_N*100: P7
restore

eststo P8: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend, base("No-Participation")
estimates store P8
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P8
lrtest P8 P7, force
estadd scalar lr_p = r(p): P8
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P8
estadd scalar bic = S[1,6]: P8
estadd scalar aicc = S[1,7]: P8
estadd scalar caic = S[1,8]: P8
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
	estadd scalar perc1 = count1/_N*100: P8
restore

/* eststo P9: cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured, casevar(weekend) base("No-Participation")
estimates store P9
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): P9
lrtest P9 P7, force
estadd scalar lr_p = r(p): P9
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: P9
estadd scalar bic = S[1,6]: P9
estadd scalar aicc = S[1,7]: P9
estadd scalar caic = S[1,8]: P9
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
	estadd scalar perc1 = count1/_N*100: P9
restore
*/

esttab P* using "${tables}preliminary_regressions_2023_09_13.rtf", starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 lr_p aic bic aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "LR-test" "AIC" "BIC" "AICc" "CAIC"))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se 
