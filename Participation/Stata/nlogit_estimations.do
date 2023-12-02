global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


** Import data
import delimited "C:\Data\PacFIN data\rdo_Stata_c4_nhauls5.csv"
replace mean_price = mean_price / 1000
replace mean_price2 = mean_price2 / 1000
replace mean_catch = mean_catch / 1000
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
replace d_missing_p = d_missing_p2 if mean_price > 50
replace mean_price = mean_price2 if mean_price > 50
gen exp_revenue  = mean_price  * mean_avail
gen exp_revenue2 = mean_price2 * mean_avail


*** Create species, port and participation.
// gen species = substr(selection,-4,.) 
// replace species = "No-Participation" if species == "tion" 
// egen species_int = group(species), label
// gen spec = substr(selection,-4,.) 
// replace spec = " " if spec == "tion" 
// gen port_l = substr(selection,1,3) 
// replace port_l = " " if port_l == "No-" 
// gen part = (selection == "No-Participation")
// egen port = group(port_l), label


** Create dummies to include in the model

// replace d_missing_p = d_missing_p2
// replace d_missing   = d_missing_catch
// replace mean_avail = mean_catch
// replace mean_price = mean_price2

gen d_c = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
gen d_d = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
gen d_p = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
gen d_cd = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
gen d_pc = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
gen d_pd = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
gen d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 



********************************************************************
* Model set-up

global closure = " "
global vars_sdm = "mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_c d_d d_pd d_cd d_pcd psdnclosured2 psdntotalclosured msqdweekend" 
global vars =               "exp_revenue wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_c d_d d_pd d_cd d_pcd psdnclosured2 psdntotalclosured msqdweekend" 
global case = " " // at the end add: lunarill
global case_sdm = " "


********************************************************************
** Create labels

label variable mean_avail "Expected availability"
label variable exp_revenue "Expected revenue"
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

** Create nested logit variables
nlogitgen port = selection( ///
	BDA: BDA-DCRB | BDA-MSQD, ///
	BGA: BGA-MSQD , ///
 	CWA: CWA-DCRB , ///
    ERA: ERA-MSQD , ///
    LAA: LAA-BTNA | LAA-CMCK | LAA-JMCK | LAA-MSQD | LAA-NANC | LAA-PBNT | LAA-PSDN | LAA-RHRG | LAA-STNA | LAA-YTNA, ///
    MNA: MNA-ALBC | MNA-CMCK | MNA-JMCK | MNA-LSKT | MNA-MSQD | MNA-NANC | MNA-PSDN | MNA-UDAB, /// 
    MRA: MRA-MSQD, ///
    NPA: NPA-MSQD, /// 
    NPS: NPS-CHUM | NPS-DCRB | NPS-SOCK, /// 
    SBA: SBA-CMCK | SBA-JMCK | SBA-MSQD | SBA-PBNT | SBA-PSDN | SBA-UMCK, /// 
    SDA: SDA-BTNA, ///
    SFA: SFA-CHNK | SFA-DCRB | SFA-MSQD | SFA-NANC, /// 
    SPS: SPS-CHUM, ///
    NoPort: No-Participation)
nlogitgen part = port(Part: BDA | BGA | CWA | ERA | LAA | MNA | MRA | NPA | NPS | SBA | SDA | SFA | SPS, ///
					  NoPart: NoPort)

** Logit tree
nlogittree selection port part, choice(fished) case(fished_haul) 

** Keep few choices
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

/* 		| ///
		selection == "LAA-BTNA" | ///
		selection == "LAA-YTNA" */ 

** Drop cases with no choice selected
cap drop check_if_choice
sort fished_haul
by fished_haul: egen check_if_choice = sum(fished)
tab check_if_choice
keep if check_if_choice
tab check_if_choice

** New Logit tree
nlogittree selection port part, choice(fished) case(fished_haul) 
constraint 1 [/port]MRA_tau = 1
constraint 2 [/part]NoPart_tau = 1 
constraint 3 [/port]NoPort_tau = 1 


*** Base model to compute R2 ***
/* nlogit fished  || part: , base(NoPart) || port: , base(NoPort) || selection: , ///
		base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num)
matrix start=e(b)
estimates store base
estimates save ${results}basemodel_TUNA.ster */
estimates use ${results}basemodel_MRA.ster
estimates store base
scalar ll0 = e(ll)

/* eststo A1: nlogit fished mean_price mean_avail wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured d_d d_c d_p d_cd d_pd d_pcd || part: unem_rate, base(NoPart) || port: , base(NoPort) || selection: weekend, ///
		base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)
		matrix start=e(b)
estimates save ${results}nlogit_1_TUNA.ster
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A1
lrtest base A1, force
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
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A1
restore

eststo A2: nlogit fished mean_price mean_avail wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured diesel_price d_d d_c d_p d_cd d_pd d_pcd || part: unem_rate, base(NoPart) || port: , base(NoPort) || selection: weekend, ///
		base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)
		matrix start=e(b)
estimates save ${results}nlogit_2_TUNA.ster
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A2
lrtest A1 A2, force
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
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A2
restore
 */

estimates use ${results}nlogit_2_MRA.ster
estimates store A2


replace d_c   = (d_missing_p == 0 & d_missing_catch == 1 & d_missing_d == 0) 
replace d_d   = (d_missing_p == 0 & d_missing_catch == 0 & d_missing_d == 1) 
replace d_p   = (d_missing_p == 1 & d_missing_catch == 0 & d_missing_d == 0) 
replace d_cd  = (d_missing_p == 0 & d_missing_catch == 1 & d_missing_d == 1) 
replace d_pc  = (d_missing_p == 1 & d_missing_catch == 1 & d_missing_d == 0) 
replace d_pd  = (d_missing_p == 1 & d_missing_catch == 0 & d_missing_d == 1) 
replace d_pcd = (d_missing_p == 1 & d_missing_catch == 1 & d_missing_d == 1) 

eststo A6: nlogit fished mean_price mean_catch wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured d_d d_c d_p d_cd d_pd d_pc d_pcd || part: unem_rate, base(NoPart) || port: , base(NoPort) || selection: weekend, ///
		base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) // from(start, skip)
		matrix start=e(b)
estimates save ${results}nlogit_6_MRA2.ster
// estimates use ${results}nlogit_3_MRA2.ster
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A6
lrtest A4 A6, force
estadd scalar lr_p = r(p): A6
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A6
estadd scalar bic = S[1,6]: A6
estadd scalar aicc = S[1,7]: A6
estadd scalar caic = S[1,8]: A6
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
	estadd scalar perc1 = count1/_N*100: A6
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A6
restore

replace d_c =   (d_missing_p == 0 & d_missing_catch == 1 & d_missing_d == 0) 
replace d_d =   (d_missing_p == 0 & d_missing_catch == 0 & d_missing_d == 1) 
replace d_p =   (d_missing_p == 1 & d_missing_catch == 0 & d_missing_d == 0) 
replace d_cd =  (d_missing_p == 0 & d_missing_catch == 1 & d_missing_d == 1) 
replace d_pc =  (d_missing_p == 1 & d_missing_catch == 1 & d_missing_d == 0) 
replace d_pd =  (d_missing_p == 1 & d_missing_catch == 0 & d_missing_d == 1) 
replace d_pcd = (d_missing_p == 1 & d_missing_catch == 1 & d_missing_d == 1) 

eststo A4: nlogit fished mean_price mean_catch wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured diesel_price d_d d_c d_p d_cd d_pd d_pc d_pcd || part: unem_rate, base(NoPart) || port: , base(NoPort) || selection: weekend, ///
		base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)
		matrix start=e(b)
estimates save ${results}nlogit_4_MRA2.ster
di "R2-McFadden = " 1 - (e(ll)/ll0)
estadd scalar r2 = 1 - (e(ll)/ll0): A4
lrtest A2 A4, force
estadd scalar lr_p = r(p): A4
estat ic, all
matrix S = r(S)
estadd scalar aic = S[1,5]: A4
estadd scalar bic = S[1,6]: A4
estadd scalar aicc = S[1,7]: A4
estadd scalar caic = S[1,8]: A4
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
	estadd scalar perc1 = count1/_N*100: A4
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100: A4
restore

replace d_c =   (d_missing_p2 == 0 & d_missing_catch == 1 & d_missing_d == 0) 
replace d_d =   (d_missing_p2 == 0 & d_missing_catch == 0 & d_missing_d == 1) 
replace d_p =   (d_missing_p2 == 1 & d_missing_catch == 0 & d_missing_d == 0) 
replace d_cd =  (d_missing_p2 == 0 & d_missing_catch == 1 & d_missing_d == 1) 
replace d_pc =  (d_missing_p2 == 1 & d_missing_catch == 1 & d_missing_d == 0) 
replace d_pd =  (d_missing_p2 == 1 & d_missing_catch == 0 & d_missing_d == 1) 
replace d_pcd = (d_missing_p2 == 1 & d_missing_catch == 1 & d_missing_d == 1) 

esttab A4 A6 using "G:\My Drive\Tables\Participation\nested_logit-${S_DATE}_MRA2.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) ///
		label title("Table. Preliminary estimations.") /// 
		stats(N r2 perc1 perc2 lr_p aicc caic, fmt(0 3) ///
			labels("Observations" "McFadden R2" "Predicted choices (%)" "- Excl. No-Participation (%)" "LR-test" "AICc" "CAIC" ))  ///
		replace nodepvars b(%9.3f) not nomtitle nobaselevels se noconstant


eststo A6: nlogit fished mean_price mean_catch wind_max_220_mh dist_port_to_catch_area_zero dist_to_cog psdnclosured d_d d_c d_p d_cd d_pd d_pc d_pcd || part: unem_rate, base(NoPart) || port: , base(NoPort) || selection: weekend, ///
		base("No-Participation") case(fished_haul) constraints(1 2 3) vce(cluster fished_vessel_num) from(start, skip)

* What about expected revenue.
* Note: Add lunar at the end.
