//global path_google "G:\Mi unidad"
global path_google "H:\My Drive"
global path "C:\GitHub\EconAnalysis\Participation\"
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


import delimited "C:\GitHub\EconAnalysis\Data\Ports\ports_area_and_name_codes.csv", varnames(1) clear
tempfile port_area
save `port_area'

** Get historical choice
// import delimited "G:\Mi unidad\Data\Cluster\cluster_aggregates.csv", clear
import delimited "${path_google}\Data\Cluster\cluster_aggregates.csv", clear
merge m:1 pacfin_port_code using `port_area', keep(3)
keep if landing_year >= 2000 & landing_year <= 2020
collapse (sum) total_landings, by(group_all pacfin_species_code landing_month landing_year port_area_code)
collapse (mean) mean_landings = total_landings, by(group_all pacfin_species_code landing_month port_area_code)
bysort group_all landing_month (mean_landings): keep if  mean_landings==mean_landings[_N]
tab group_all landing_month
keep landing_month group_all pacfin_species_code port_area_code
gen  hist_selection = port_area_code + "-" + pacfin_species_code 
drop pacfin_species_code port_area_code
rename landing_month set_month
tempfile hist_data
save `hist_data'


** Import data (It do not work if 180km radius for ALBC -- 90km radius is the best -- V2)
import delimited "${path_google}\Data\Anonymised data\rdo_Stata_c7_full_noid.csv", clear
gen group_all = 7
// gen species = substr(selection, 5, 4) 

** Merge with historical data
merge m:1 group_all set_month using `hist_data', keep(3)
gen d_hist_selection = (hist_selection == selection)

** Add addtional variables
gen psdnclosured2 = psdnclosured - psdntotalclosured
gen psdnclosure2 = psdnclosure - psdntotalclosure
replace mean_price2 = mean_price2 / 1000
replace mean_price = mean_price / 1000
replace mean_catch = mean_catch / 1000
replace d_missing_p = d_missing_p2 if mean_price > 50
replace mean_price = mean_price2 if mean_price > 50


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

** Filter model 
keep if ///
selection == "SBA-MSQD" | /// 
selection == "SBA-NANC" | /// 
selection == "LAA-MSQD" | /// 
selection == "MNA-MSQD" | /// 
selection == "SDA-NANC" | /// 
selection == "LAA-CMCK" | /// 
selection == "LAA-PSDN" | /// 
selection == "SFA-MSQD" | ///
selection == "MNA-CMCK" | ///
selection == "MNA-PSDN" | ///
selection == "MNA-JMCK" | ///
selection == "MRA-MSQD" | ///
selection == "LAA-JMCK" | ///
selection == "MNA-NANC" | ///
selection == "No-Participation"

VOLVER A AGREGAR

// selection == "SFA-BLCK" | ///
// selection == "MNA-SMLT" | ///

drop if msqdclosure

tab psdnclosured

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
estimates save ${results}nlogit_C7_base.ster, replace
scalar ll0 = e(ll)

*** Set nested logit
cap drop port
cap label drop lb_port
cap drop partp
cap label drop lb_partp

tab selection if fished == 1

nlogitgen port = selection( ///
	MSQD: SBA-MSQD | LAA-MSQD | MNA-MSQD | SFA-MSQD | MRA-MSQD, ///
	NANC: SBA-NANC | SDA-NANC | MNA-NANC, /// 
	OMCK: LAA-CMCK | MNA-CMCK | MNA-JMCK | LAA-JMCK, /// 
	PSDN: LAA-PSDN | MNA-PSDN, ///
	NOPORT: No-Participation) 

	// BLCK: SFA-BLCK, ///
	// SMLT: MNA-SMLT, /// 

nlogitgen partp = port(PART: MSQD | NANC | OMCK | PSDN, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 
// constraint 1 [/port]OMCK_tau = 1
// constraint 2 [/port]NANC_tau = 1
// save "${path_google}\Data\Anonymised data\part_model_c7.dta", replace


************************
*** Run nested logit ***
************************
nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
	psdnclosured  dummy_last_day  d_c d_d d_p d_cd d_pc d_pd d_pcd /// 
	|| partp: unem_rate, base(NOPART) || port: weekend , base(NOPORT) || selection: , ///
	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_anon)
// estimates save ${results}nlogit_FULL_C7.ster, replace



---------------------------------------------

estimates use ${results}nlogit_FULL_C7.ster
matrix start=e(b)
estimates store B1
lrtest base B1, force
***********************************
tab d_hist_selection
estimates save ${results}nlogit_FULL_C7_hist.ster, replace
estimates use ${results}nlogit_FULL_C7_hist.ster
estimates store B2
lrtest B1 B2, force



