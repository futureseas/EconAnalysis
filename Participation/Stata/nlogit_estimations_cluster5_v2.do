global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


** Import data
// import delimited "C:\Data\PacFIN data\rdo_Stata_c5_full_v2.csv", clear
// save "C:\Data\PacFIN data\rdo_Stata_c5_full_v2.dta", replace
use "C:\Data\PacFIN data\rdo_Stata_c5_full_v2.dta", clear

** Add addtional variables
gen psdnclosured2 = psdnclosured - psdntotalclosured
gen psdnclosure2 = psdnclosure - psdntotalclosure
replace mean_price2 = mean_price2 / 1000
replace mean_price = mean_price / 1000
replace mean_catch = mean_catch / 1000
replace d_missing_p = d_missing_p2 if mean_price > 50
replace mean_price = mean_price2 if mean_price > 50


gen d_c   = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 0) 
gen d_d   = (d_missing_p2 == 0 & d_missing == 0 & d_missing_d == 1) 
gen d_p   = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 0) 
gen d_cd  = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 1) 
gen d_pc  = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 0) 
gen d_pd  = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 1) 
gen d_pcd = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 1) 
qui tabulate set_month, generate(month)


********************************************************************
** Create labels
label variable mean_avail "Expected availability"
label variable mean_catch "Expected catch"
label variable mean_price "Expected price"
label variable mean_price2 "Expected price (30 days)"
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

** Filter model 

keep if selection == "SBA-MSQD" | selection == "MNA-MSQD" | ///
		selection == "LAA-MSQD" | selection == "SFA-MSQD" | /// 
		selection == "CBA-MSQD" | selection == "MRA-MSQD" | ///
		selection == "NPA-MSQD" | ///		
	    selection == "CLO-PSDN" | selection == "CWA-PSDN" | ///
		selection == "CLW-PSDN" | selection == "LAA-PSDN" | ///
		selection == "CWA-ALBC" | ///
		selection == "LAA-CMCK" | selection == "SBA-CMCK" | ///
		selection == "LAA-NANC" | selection == "No-Participation" | ///
		selection == "CLW-DCRB" | selection == "CWA-DCRB"

** Drop cases with no choice selected
cap drop check_if_choice
sort fished_haul
by fished_haul: egen check_if_choice = sum(fished)
tab check_if_choice
keep if check_if_choice
tab check_if_choice


*** Base model to compute R2
set processors 4
asclogit fished, base("No-Participation")  alternatives(selection) case(fished_haul) 
estimates store base
scalar ll0 = e(ll)


*** Set nested logit
cap drop port
cap label drop lb_port
cap drop partp
cap label drop lb_partp
nlogitgen port = selection( ///
	MSQD: SBA-MSQD | MNA-MSQD | LAA-MSQD | SFA-MSQD | CBA-MSQD | MRA-MSQD | NPA-MSQD, ///
	PSDN: CLO-PSDN | CWA-PSDN | CLW-PSDN | LAA-PSDN, ///
	ALBC: CWA-ALBC, ///
	CMCK: LAA-CMCK | SBA-CMCK, ///
	NANC: LAA-NANC, ///
	DCRB: CLW-DCRB | CWA-DCRB, ///
	NOPORT: No-Participation)
nlogitgen partp = port(PART: MSQD | PSDN | ALBC | CMCK | NANC, PART_CRAB: DCRB, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 
constraint 1 [/port]DCRB_tau = 1
constraint 2 [/port]CMCK_tau = 1

estimates use ${results}nlogit_FULL_C5_v7.ster
matrix start=e(b)
nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		unem_rate dummy_last_day psdnclosured dcrbclosurewad msqdclosured waclosured ///
		d_c d_d d_p d_cd d_pc d_pd d_pcd  /// 
		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) constraints(1 2) vce(cluster fished_vessel_id) ///
	from(start, skip)
matrix start=e(b)
estimates save ${results}nlogit_FULL_C5_v7.ster, replace
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
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
restore
preserve
	replace dummy_last_day = 0
	replace dummy_prev_days = 0
	replace dummy_prev_year_days = 0
	replace dummy_prev_days_port = 0
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
	dis _N
	dis count2/_N*100 "%"
restore
