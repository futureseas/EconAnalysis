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


********************************************************************
** Create labels
label variable mean_avail "Expected availability"
label variable mean_catch "Expected catch"
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

keep if selection == "SBA-MSQD" | ///
		selection == "MNA-MSQD" | ///
		selection == "LAA-MSQD" | ///
		selection == "MNA-NANC" | ///
		selection == "SFA-MSQD" | ///
		selection == "LAA-CMCK" | ///
		selection == "MNA-PSDN" | ///
		selection == "LAA-PSDN" | ///
		selection == "MRA-MSQD" | ///
		selection == "LAA-NANC" | ///
		selection == "SBA-CMCK" | ///
		selection == "SFA-NANC" | ///
		selection == "LAA-YTNA" | ///
        selection == "No-Participation"  

** Drop cases with no choice selected
cap drop check_if_choice
sort fished_haul
by fished_haul: egen check_if_choice = sum(fished)
tab check_if_choice
keep if check_if_choice
tab check_if_choice

cmset fished_vessel_id time selection
cmclogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
			d_missing_d d_missing_p psdnclosured unem_rate msqdweekend i.dummy_last_day, ///
	base("No-Participation")   // from(start, skip)

** Create nested logit variables
cap drop port
cap label drop lb_port
cap drop partp
cap label drop lb_partp
nlogitgen port = selection( ///
	LAA: LAA-CMCK | LAA-MSQD | LAA-PSDN | LAA-NANC, ///
	LAATUNA: LAA-YTNA, ///
    MNA: MNA-MSQD | MNA-NANC | MNA-PSDN, /// 
    MRA: MRA-MSQD, ///
    SBA: SBA-MSQD | SBA-CMCK, /// 
    SFA: SFA-MSQD | SFA-NANC, /// 
    NOPORT: No-Participation)
nlogitgen partp = port(PART: LAA | LAATUNA | MNA | MRA | SBA | SFA, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 

** New Logit tree
constraint 1 [/partp]NOPART_tau = 1 
constraint 2 [/port]NOPORT_tau = 1 
constraint 3 [/port]MRA_tau = 1
constraint 4 [/port]SBA_tau = 1
constraint 5 [/port]SFA_tau = 1
constraint 6 [/port]LAA_tau = 1
constraint 7 [/port]MNA_tau = 1
constraint 8 [/port]SBA_tau = 1
constraint 9 [/partp]PART_tau = 1 
constraint 10 [/port]LAATUNA_tau = 1


*** Estimate model
set processors 4
estimates use ${results}nlogit_FULL.ster
matrix sB=e(b)

nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
		d_missing_d d_missing_p psdnclosured unem_rate msqdweekend dummy_last_day /// 
		|| partp: , base(NOPART) || port: , base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_id) from(sB, copy)
	estimates save ${results}nlogit_FULL_clustered.ster, replace


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
	estadd scalar perc1 = count1/_N*100
	drop if selection == "No-Participation"
	egen count2 = total(fished)
	dis _N
	dis count2/_N*100 "%"
	estadd scalar perc2 = count2/_N*100
restore


** Create nested logit variables
cap drop port
cap label drop lb_port
cap drop partp
cap label drop lb_partp
nlogitgen port = selection( ///
	CMCK: LAA-CMCK | SBA-CMCK, ///
	MSDQ: LAA-MSQD | MNA-MSQD | MRA-MSQD | SBA-MSQD | SFA-MSQD
	PSDN: LAA-PSDN | MNA-PSDN
	NANC: LAA-NANC | MNA-NANC | SFA-NANC, ///
	TUNA: LAA-YTNA, ///
    NOSPEC: No-Participation)
nlogitgen partp = port(PART: CMCK | MSQD | PSDN | NANC | TUNA, NOPART: NOSPEC)
nlogittree selection port partp, choice(fished) case(fished_haul) 



** Create nested logit variables
cap drop port
cap label drop lb_port
cap drop partp
cap label drop lb_partp
nlogitgen port = selection( ///
	LAA: LAA-CMCK | LAA-MSQD | LAA-PSDN | LAA-NANC, ///
	LAATUNA: LAA-YTNA, ///
    MNA: MNA-MSQD | MNA-NANC | MNA-PSDN, /// 
    MRA: MRA-MSQD, ///
    SBA: SBA-MSQD | SBA-CMCK, /// 
    SFA: SFA-MSQD | SFA-NANC, /// 
    NOPORT: No-Participation)
nlogitgen partp = port(PART: LAA | MNA | MRA | SBA | SFA, PARTTUNA: LAATUNA, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 

