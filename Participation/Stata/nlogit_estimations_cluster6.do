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
import delimited "H:\My Drive\Data\Cluster\cluster_aggregates.csv", clear
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
import delimited "H:\My Drive\Data\Anonymised data\rdo_Stata_c6_full_noid.csv", clear
gen group_all = 6
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

keep if selection == "CWA-NANC" | selection == "CLO-PSDN" | ///
		selection == "CLW-PSDN" | selection == "CWA-PSDN" | ///
		selection == "CLO-NANC" | selection == "CWA-DCRB" | ///
		selection == "CLW-NANC" | selection == "CBA-PSDN" | ///
		selection == "NPS-SOCK" | selection == "CLO-CMCK" | ///
		selection == "CLO-JMCK" | ///
		selection == "No-Participation"

tab waclosured
keep if waclosured == 0 // 98,8% of the observations
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
estimates save ${results}nlogit_C6_base.ster, replace
scalar ll0 = e(ll)

*** Set nested logit

** JMCK AND CMCK have to be separated by adding a constraint (seoarate nest did not work)

cap drop port
cap label drop lb_port
cap drop partp
cap label drop lb_partp
nlogitgen port = selection( ///
        PSDN: CLO-PSDN | CLW-PSDN | CWA-PSDN | CBA-PSDN, ///
        NANC: CLO-NANC | CLW-NANC | CWA-NANC, ///
        OMCK: CLO-CMCK | CLO-JMCK, ///
        DCRB: CWA-DCRB, ///
        SLMN: NPS-SOCK, ///
        NOPORT: No-Participation) 
nlogitgen partp = port(PART: PSDN | NANC | OMCK, CRAB_PART: DCRB, SLMN_PART: SLMN, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 
constraint 1 [/port]OMCK_tau = 1
constraint 2 [/port]NANC_tau = 1

save "H:\My Drive\Data\Anonymised data\part_model_c6.dta", replace



************************
*** Run nested logit ***
************************

// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 	dcrbclosurewad dummy_last_day unem_rate d_d /// 
// 	|| partp: psdnclosure mean_price, base(NOPART) || port: weekend , base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon)
// estimates save ${results}nlogit_FULL_C6.ster, replace
estimates use ${results}nlogit_FULL_C6.ster
matrix start=e(b)
estimates store B1
lrtest base B1, force


// tab d_hist_selection
// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 	dcrbclosurewad d_hist_selection unem_rate d_d /// 
// 	|| partp: psdnclosure mean_price, base(NOPART) || port: weekend , base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1 2) vce(cluster fished_vessel_anon) from(start, skip)
// estimates save ${results}nlogit_FULL_C6_hist.ster, replace
estimates use ${results}nlogit_FULL_C6_hist.ster
estimates store B2
lrtest B1 B2, force



