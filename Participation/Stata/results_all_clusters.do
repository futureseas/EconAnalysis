
*****************************************************
*** All cluster results for Discrete Choice Model ***
*****************************************************
*global google_path "H:\My Drive\"
global google_path "G:\Mi unidad\"
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

estimates use ${results}nlogit_FULL_c4_prev_days_2.ster
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

estimates use ${results}nlogit_FULL_c4_22042024.ster
estimates store C4_v2
estimates describe C4_v2
estimates replay C4_v2
di "R2-McFadden = " 1 - (e(ll)/ll0_C4)
estadd scalar r2 = 1 - (e(ll)/ll0_C4): C4_v2
lrtest base_C4 C4_v2, force
estadd scalar lr_p = r(p): C4_v2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: C4_v2
estadd scalar bic = S[1,6]: C4_v2
estadd scalar aicc = S[1,7]: C4_v2
estadd scalar caic = S[1,8]: C4_v2
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
	estadd scalar perc1 = count1/_N*100: C4_v2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: C4_v2
restore


****** Cluster 5 *****

use "${google_path}Data\Anonymised data\part_model_c5.dta", clear

estimates use ${results}nlogit_C5_base.ster
estimates store base_C5 
scalar ll0_C5 = e(ll)

estimates use ${results}nlogit_FULL_C5_v12_G_prev_days.ster
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

estimates use ${results}nlogit_FULL_C5_v12_G.ster
estimates store C5_v2 
estimates describe C5_v2
estimates replay C5_v2
di "R2-McFadden = " 1 - (e(ll)/ll0_C5)
estadd scalar r2 = 1 - (e(ll)/ll0_C5): C5_v2
lrtest base_C5 C5_v2, force
estadd scalar lr_p = r(p): C5_v2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: C5_v2
estadd scalar bic = S[1,6]: C5_v2
estadd scalar aicc = S[1,7]: C5_v2
estadd scalar caic = S[1,8]: C5_v2
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
	estadd scalar perc1 = count1/_N*100: C5_v2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: C5_v2
restore


************* Cluster 6

use "${google_path}Data\Anonymised data\part_model_c6.dta", clear

estimates use ${results}nlogit_C6_base.ster
estimates store base_C6 
scalar ll0_C6 = e(ll)

estimates use ${results}nlogit_FULL_C6_B_prev_days.ster
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

estimates use ${results}nlogit_FULL_C6_B.ster
estimates store C6_v2
estimates describe C6_v2
estimates replay C6_v2
di "R2-McFadden = " 1 - (e(ll)/ll0_C6)
estadd scalar r2 = 1 - (e(ll)/ll0_C6): C6_v2
lrtest base_C6 C6_v2, force
estadd scalar lr_p = r(p): C6_v2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: C6_v2
estadd scalar bic = S[1,6]: C6_v2
estadd scalar aicc = S[1,7]: C6_v2
estadd scalar caic = S[1,8]: C6_v2
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
	estadd scalar perc1 = count1/_N*100: C6_v2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: C6_v2
restore


************* Cluster 7
*global google_path "H:\My Drive\"
global google_path "G:\Mi unidad\"
global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


use "${google_path}Data\Anonymised data\part_model_c7.dta", clear
estimates use ${results}nlogit_C7_base.ster
estimates store base_C7 
scalar ll0_C7 = e(ll)

estimates use ${results}nlogit_FULL_C7_prev_days.ster
estimates store C7_v1
estimates describe C7_v1
estimates replay C7_v1
nlogittree selection port partp, choice(fished) case(fished_haul) 
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

estimates use ${results}nlogit_FULL_C7.ster
estimates store C7_v2
estimates describe C7_v2
estimates replay C7_v2
di "R2-McFadden = " 1 - (e(ll)/ll0_C7)
estadd scalar r2 = 1 - (e(ll)/ll0_C7): C7_v2
lrtest base_C7 C7_v2, force
estadd scalar lr_p = r(p): C7_v2
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: C7_v2
estadd scalar bic = S[1,6]: C7_v2
estadd scalar aicc = S[1,7]: C7_v2
estadd scalar caic = S[1,8]: C7_v2
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
	estadd scalar perc1 = count1/_N*100: C7_v2
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: C7_v2
restore



*** Save model
// esttab  C4_v1 C4_v2 C5_v1 C5_v2 C6_v1 C6_v2 C7_v1 C7_v2 using "${google_path}Tables\Participation\nested_logit_${S_DATE}_all_clusters.rtf", ///
// 		starlevels(* 0.10 ** 0.05 *** 0.01) ///
// 		label title("Table. Nested Logit.") /// 
// 		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
// 			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
// 		mgroups("Cluster 4" "Cluster 5" "Cluster 6" "Cluster 7", pattern(1 0 1 0 1 0 1 0)) ///
// 		replace nodepvars b(%9.3f) not nomtitle nobaselevels noconstant



esttab  C4_v1 C4_v2 using "${google_path}Tables\Participation\nested_logit_cluster4.tex", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels noconstant style(tex)


esttab  C5_v1 C5_v2 using "${google_path}Tables\Participation\nested_logit_cluster5.tex", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels noconstant style(tex)


esttab  C6_v1 C6_v2 using "${google_path}Tables\Participation\nested_logit_cluster6.tex", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels noconstant style(tex)


esttab  C7_v1 C7_v2 using "${google_path}Tables\Participation\nested_logit_cluster7.tex", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Nested Logit.") /// 
		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels noconstant style(tex)
