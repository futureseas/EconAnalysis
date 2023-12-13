*** Compare models FULL v/s SAMPLED ****

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



keep if selection == "LAA-CMCK" | ///
		selection == "LAA-MSQD" | ///
		selection == "SFA-MSQD" | ///
		selection == "SFA-NANC" | ///
		selection == "MNA-NANC" | ///
		selection == "MNA-MSQD" | ///
		selection == "SBA-MSQD" | ///
		selection == "SBA-CMCK" | ///
        selection == "No-Participation" | ///
		selection == "MNA-PSDN" | ///
		selection == "LAA-PSDN" | ///
		selection == "LAA-NANC" | ///
		selection == "MRA-MSQD" 

** Drop cases with no choice selected
cap drop check_if_choice
sort fished_haul
by fished_haul: egen check_if_choice = sum(fished)
tab check_if_choice
keep if check_if_choice
tab check_if_choice

** 
nlogitgen port = selection( ///
	LAA: LAA-CMCK | LAA-MSQD | LAA-NANC | LAA-PSDN, ///
    MNA: MNA-MSQD | MNA-NANC | MNA-PSDN, /// 
    MRA: MRA-MSQD, ///
    SBA: SBA-CMCK | SBA-MSQD, /// 
    SFA: SFA-MSQD | SFA-NANC, /// 
    NOPORT: No-Participation)
nlogitgen partp = port(PART: LAA | MNA | MRA | SBA | SFA, NOPART: NOPORT)
nlogitgen part = port(PART: LAA | MNA | MRA | SBA | SFA, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 


** Compare models
estimates use ${results}nlogit_1_FULL.ster
estimate store full
scalar ll_full = e(ll)
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
	estadd scalar perc1 = count1/_N*100: full
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: full
restore


***
drop port 
label drop lb_port 
nlogitgen port = selection( ///
	LAA: LAA-CMCK | LAA-MSQD | LAA-NANC | LAA-PSDN, ///
    MNA: MNA-MSQD | MNA-NANC | MNA-PSDN, /// 
    MRA: MRA-MSQD, ///
    SBA: SBA-CMCK | SBA-MSQD, /// 
    SFA: SFA-MSQD | SFA-NANC, /// 
    NoPort: No-Participation)
drop part 
label drop lb_part
nlogitgen part = port(Part: LAA | MNA | MRA | SBA | SFA, NoPart: NoPort)
nlogittree selection port part, choice(fished) case(fished_haul) 

estimates use ${results}nlogit_1_MRA.ster
estimate store sampled
estimates describe sampled

scalar ll_sampled = e(ll)
preserve
	cap drop phat
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	by fished_haul, sort: gen nvals = _n == 1 
	count if nvals
	dis _N
	gen selection_hat = 1
	egen count1 = total(fished)
	dis count1/_N*100 "%"
	estadd scalar perc1 = count1/_N*100: sampled
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: sampled
restore

*** Better to use full choice set
esttab full sampled using "G:\My Drive\Tables\Participation\nested_logit-${S_DATE}_full_vs_sampled.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se noconstant drop(weekend _cons)
