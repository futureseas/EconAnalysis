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


/* import delimited "C:\GitHub\EconAnalysis\Data\Ports\ports_area_and_name_codes.csv", varnames(1) clear
tempfile port_area
save `port_area'

** Get historical choice
import delimited "${google_path}Data\Cluster\cluster_aggregates.csv", clear
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
import delimited "${google_path}Data\Anonymised data\rdo_Stata_c5_full_v2_noid.csv", clear
gen group_all = 5


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

*** Get annual price for DCRB
sort selection set_year
by selection set_year : egen mean_price_annual = mean(mean_price2) 
gen mean_price_3 = mean_price
replace mean_price_3 = mean_price_annual if selection == "CLW-DCRB" | selection == "CWA-DCRB"

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

keep if selection == "SBA-MSQD" | selection == "MNA-MSQD" | ///
		selection == "LAA-MSQD" | selection == "SFA-MSQD" | /// 
		selection == "CBA-MSQD" | selection == "MRA-MSQD" | ///
		selection == "NPA-MSQD" | selection == "CLO-PSDN" | ///
		selection == "CWA-PSDN" | selection == "CLW-PSDN" | ///
		selection == "LAA-PSDN" | selection == "CWA-ALBC" | ///
		selection == "LAA-CMCK" | selection == "SBA-CMCK" | ///
		selection == "LAA-NANC" | selection == "CLW-DCRB" | ///
		selection == "CWA-DCRB" | selection == "No-Participation"

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
estimates save ${results}nlogit_C5_base.ster, replace
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

save "${google_path}Data\Anonymised data\part_model_c5.dta", replace
 */
use "${google_path}Data\Anonymised data\part_model_c5.dta", clear
corr dist_to_cog dist_port_to_catch_area_zero if d_missing_d == 0

* corr 0.2296 

************************
*** Run nested logit ***
************************

**** Note: Using albacore SDM within 90Km -- 
****       LR test reject that coefficient for price (estimated) in DCRB equation is equal to zero
****       Using annual price for crab
 
// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		dummy_last_day unem_rate d_d d_cd msqdclosured psdnclosured waclosured dcrbclosurewad /// 
// 		|| partp:  mean_price_3, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 		base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) 
// 	 estimates save ${results}nlogit_FULL_C5_v12_G.ster, replace 
estimates use ${results}nlogit_FULL_C5_v12_G.ster
matrix start=e(b)
estimates store B12_G


****************** USING PREV DAYS DUMMY ************************

// nlogit fished mean_avail  wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		dummy_prev_days dummy_prev_year_days unem_rate d_d d_cd msqdclosured psdnclosured waclosured dcrbclosurewad /// 
// 		|| partp:  mean_price_3, base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 		base("No-Participation") case(fished_haul) constraints(1 2) vce(cluster fished_vessel_anon) from(start, skip)
// 	 estimates save ${results}nlogit_FULL_C5_v12_G_prev_days.ster, replace 
	 estimates use ${results}nlogit_FULL_C5_v12_G_prev_days.ster
matrix start=e(b)
estimates store B12_G_prev_days
lrtest B12_G B12_G_prev_days, force
