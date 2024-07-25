
*****************************************************
*** All cluster results for Discrete Choice Model ***
*****************************************************


*** Dan Holland would ask for 30 days state dependency!


global google_path "H:\My Drive\"
*lobal google_path "G:\Mi unidad\"
global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


****** Cluster 4 *****

use "${google_path}Data\Anonymised data\part_model_c4.dta", clear

estimates use ${results}nlogit_C4_base.ster
estimates store base_C4 
scalar ll0_C4 = e(ll)

estimates use ${results}nlogit_FULL_c4_22042024.ster
matrix start=e(b)
estimates store C4_v1
estimates describe C4_v1
estimates replay C4_v1
di "R2-McFadden = " 1 - (e(ll)/ll0_C4)
estadd scalar r2 = 1 - (e(ll)/ll0_C4): C4_v1
lrtest base_C4 C4_v1, force
estadd scalar lr_p = r(p): C4_v1
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: C4_v1
estadd scalar bic = S[1,6]: C4_v1
estadd scalar aicc = S[1,7]: C4_v1
estadd scalar caic = S[1,8]: C4_v1
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
	estadd scalar perc1 = count1/_N*100: C4_v1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: C4_v1
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	replace d_hist_selection = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: C4_v1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: C4_v1
restore


****** Cluster 5 *****

use "${google_path}Data\Anonymised data\part_model_c5.dta", clear

estimates use ${results}nlogit_C5_base.ster
estimates store base_C5 
scalar ll0_C5 = e(ll)

estimates use ${results}nlogit_FULL_C5_v12_G.ster
estimates store C5_v1 
estimates describe C5_v1
estimates replay C5_v1
di "R2-McFadden = " 1 - (e(ll)/ll0_C5)
estadd scalar r2 = 1 - (e(ll)/ll0_C5): C5_v1
lrtest base_C5 C5_v1, force
estadd scalar lr_p = r(p): C5_v1
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: C5_v1
estadd scalar bic = S[1,6]: C5_v1
estadd scalar aicc = S[1,7]: C5_v1
estadd scalar caic = S[1,8]: C5_v1
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
	estadd scalar perc1 = count1/_N*100: C5_v1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: C5_v1
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	replace d_hist_selection = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: C5_v1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: C5_v1
restore



************* Cluster 6

use "${google_path}Data\Anonymised data\part_model_c6.dta", clear
estimates use ${results}nlogit_C6_base.ster
estimates store base_C6 
scalar ll0_C6 = e(ll)

estimates use ${results}nlogit_FULL_C6_B.ster
matrix start=e(b)
estimates store C6_v1
estimates describe C6_v1
estimates replay C6_v1
di "R2-McFadden = " 1 - (e(ll)/ll0_C6)
estadd scalar r2 = 1 - (e(ll)/ll0_C6): C6_v1
lrtest base_C6 C6_v1, force
estadd scalar lr_p = r(p): C6_v1
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: C6_v1
estadd scalar bic = S[1,6]: C6_v1
estadd scalar aicc = S[1,7]: C6_v1
estadd scalar caic = S[1,8]: C6_v1
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
	estadd scalar perc1 = count1/_N*100: C6_v1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: C6_v1
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	replace d_hist_selection = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: C6_v1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: C6_v1
restore


************* Cluster 6

use "${google_path}Data\Anonymised data\part_model_c7.dta", clear
estimates use ${results}nlogit_C7_base.ster
estimates store base_C7 
scalar ll0_C7 = e(ll)

estimates use ${results}nlogit_FULL_C7_v2.ster
matrix start=e(b)
estimates store C7_v1
estimates describe C7_v1
estimates replay C7_v1
di "R2-McFadden = " 1 - (e(ll)/ll0_C7)
estadd scalar r2 = 1 - (e(ll)/ll0_C7): C7_v1
lrtest base_C7 C7_v1, force
estadd scalar lr_p = r(p): C7_v1
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: C7_v1
estadd scalar bic = S[1,6]: C7_v1
estadd scalar aicc = S[1,7]: C7_v1
estadd scalar caic = S[1,8]: C7_v1
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
	estadd scalar perc1 = count1/_N*100: C7_v1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: C7_v1
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
	replace d_hist_selection = 0
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc3 = count1/_N*100: C7_v1
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc4 = count2/_N*100: C7_v1
restore



// *** Save model
esttab  C4_v1 C5_v1 C6_v1 C7_1 using "${google_path}Tables\Participation\nested_logit_${S_DATE}_all_clusters.tex", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 perc3 perc4 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" ///
			"Predicted choices (%) -- No SD" "- Excl. No-Participation (%) -- No SD" "LR-test" "AICc" "CAIC" ))  ///
		mgroups("Cluster 4" "Cluster 5", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels noconstant style(tex)





************************************

estimates use ${results}nlogit_FULL_c4_prev_days_2.ster
estimates use ${results}nlogit_FULL_C5_v12_G_prev_days.ster
estimates use ${results}nlogit_FULL_C6_B_prev_days.ster
estimates use ${results}nlogit_FULL_C7_prev_days.ster
