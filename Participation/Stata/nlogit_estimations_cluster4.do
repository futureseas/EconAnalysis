global google_path "H:\My Drive\"
*global google_path "G:\Mi unidad\"
global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path
clear all


// import delimited "C:\GitHub\EconAnalysis\Data\Ports\ports_area_and_name_codes.csv", varnames(1) clear
// tempfile port_area
// save `port_area'

// ** Get historical choice
// import delimited "${google_path}Data\Cluster\cluster_aggregates.csv", clear
// merge m:1 pacfin_port_code using `port_area', keep(3)
// keep if landing_year >= 2000 & landing_year <= 2020
// collapse (sum) total_landings, by(group_all pacfin_species_code landing_month landing_year port_area_code)
// collapse (mean) mean_landings = total_landings, by(group_all pacfin_species_code landing_month port_area_code)
// bysort group_all landing_month (mean_landings): keep if  mean_landings==mean_landings[_N]
// tab group_all landing_month
// keep landing_month group_all pacfin_species_code port_area_code
// gen  hist_selection = port_area_code + "-" + pacfin_species_code 
// drop pacfin_species_code port_area_code
// rename landing_month set_month
// tempfile hist_data
// save `hist_data'


// ** Import data
// import delimited "${google_path}Data\Anonymised data\rdo_Stata_c4_full_noid.csv", clear
// gen group_all = 4


// ** Merge with historical data
// merge m:1 group_all set_month using `hist_data', keep(3)
// gen d_hist_selection = (hist_selection == selection)

// ** Add addtional variables
// gen psdnclosured2 = psdnclosured - psdntotalclosured
// gen psdnclosure2 = psdnclosure - psdntotalclosure
// replace mean_price2 = mean_price2 / 1000
// replace mean_price = mean_price / 1000
// replace mean_catch = mean_catch / 1000
// replace d_missing_p = d_missing_p2 if mean_price > 50
// replace mean_price = mean_price2 if mean_price > 50


// gen d_c   = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
// gen d_d   = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
// gen d_p   = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
// gen d_cd  = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
// gen d_pc  = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
// gen d_pd  = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
// gen d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 
// qui tabulate set_month, generate(month)







// ********************************************************************
// ** Create labels
// label variable mean_avail "Expected availability"
// label variable mean_catch "Expected catch"
// label variable mean_price "Expected price"
// label variable mean_price2 "Expected price (30 days)"
// label variable diesel_price "Expected diesel price"
// label variable wind_max_220_mh "Maximum wind (< 220km)"
// label variable d_missing "Binary: Missing availability"
// label variable d_missing_cpue "Binary: Missing availability (CPUE)"
// label variable d_missing_p "Binary: Missing price"
// label variable dist_port_to_catch_area "Distance to catch area"
// label variable dist_port_to_catch_area_zero "Distance to catch area"
// label variable d_missing_d "Binary: Missing distance"
// label variable lat_cg "Latitudinal Center of Gravity"
// label variable unem_rate "State unemployment rate"
// label variable dist_to_cog "Distance to Center of Gravity" 
// label variable ddieselstate "Binary: Diesel price by state" 
// label variable psdnclosured "Binary: PSDN Closure x PSDN" 
// label variable psdnclosured2 "Binary: PSDN Seasonal Closure x PSDN chosen" 
// label variable psdntotalclosured "Binary: PSDN Total Closure x PSDN chosen" 
// label variable msqdclosured "Binary: MSQD Closure"
// label variable msqdweekend  "Binary: Weekend x MSQD chosen"
// label variable dummy_last_day "Alternative has been chosen last day"
// label variable dummy_prev_days "Alternative has been chosen during the last 30 days"
// label variable dummy_prev_days_port "Port has been chosen during the last 30 days"
// label variable dummy_prev_year_days "Alternative has been chosen during the last 30 days (previous year)"
// label variable dummy_clust_prev_days "Alternative has been chosen during the last 30 days by any member of the fleet"
// label variable hist_selection "Alternative has been historically chosen during the month (>20% revenue)"
// label variable d_c "Binary: Availability missing "
// label variable d_p "Binary: Price missing "
// label variable d_d "Binary: Distance missing "
// label variable d_pd "Binary: Distance and price missing"
// label variable d_pc "Binary: Price and availability missing"
// label variable d_cd "Binary: Availability and distance missing"
// label variable d_pcd "Binary: Availability, distance and price missing"


// ***************************
// ** Estimate nested logit **
// ***************************




















// *** Base model to compute R2
// set processors 4
// asclogit fished, base("No-Participation")  alternatives(selection) case(fished_haul) 
// estimates store base
// estimates save ${results}nlogit_C4_base.ster, replace
// scalar ll0 = e(ll)

// *** Set nested logit
// cap drop port
// cap label drop lb_port
// cap drop partp
// cap label drop lb_partp
// nlogitgen port = selection( ///
// 	CMCK: LAA-CMCK | SBA-CMCK, ///
// 	MSQD: LAA-MSQD | MNA-MSQD | MRA-MSQD | SBA-MSQD | SFA-MSQD, ///
// 	PSDN: LAA-PSDN | MNA-PSDN, ///
// 	NANC: LAA-NANC | MNA-NANC | SFA-NANC, ///
// 	TUNA: LAA-YTNA | LAA-BTNA, ///
//     NOPORT: No-Participation)


// nlogitgen partp = port(PART: CMCK | MSQD | PSDN | NANC | TUNA, NOPART: NOPORT)
// nlogittree selection port partp, choice(fished) case(fished_haul) 
constraint 1 [/port]TUNA_tau = 1


// save "${google_path}Data\Anonymised data\part_model_c4.dta", replace

use "${google_path}Data\Anonymised data\part_model_c4.dta", clear

corr dist_to_cog dist_port_to_catch_area_zero if d_missing_d == 0
* Correlation 0.17 

tab selection d_missing_d
tab selection d_missing


************************
*** Run nested logit ***
************************

*** Note: MSQD always open in this case



* nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
*			psdnclosured btnaclosured dummy_last_day unem_rate d_c d_d d_p d_cd d_pc d_pd d_pcd  /// 
*			|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
*		base("No-Participation") case(fished_haul) vce(cluster fished_vessel_anon)
*	estimates save ${results}nlogit_FULL_c4_22042024.ster, replace
estimates use ${results}nlogit_FULL_c4_22042024.ster
estimates store B1
matrix start=e(b)


****************** USING PREV DAYS DUMMY (Note: I had to add TUNA constraint in correlation ************************ 

// nlogit fished mean_avail mean_price wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 			psdnclosured btnaclosured dummy_prev_days dummy_prev_year_days unem_rate d_c d_d d_p d_cd d_pc d_pd d_pcd  /// 
// 			|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 		base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) from(start, skip)
// 	estimates save ${results}nlogit_FULL_c4_prev_days_2.ster, replace
estimates use ${results}nlogit_FULL_c4_prev_days_2.ster
matrix start=e(b)
estimates store B2
lrtest B1 B2, force


****************** USING 30 days MA for Prices ************************ 

replace d_c   = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 0) 
replace d_d   = (d_missing_p2 == 0 & d_missing == 0 & d_missing_d == 1) 
replace d_p   = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 0) 
replace d_cd  = (d_missing_p2 == 0 & d_missing == 1 & d_missing_d == 1) 
replace d_pc  = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 0) 
replace d_pd  = (d_missing_p2 == 1 & d_missing == 0 & d_missing_d == 1) 
replace d_pcd = (d_missing_p2 == 1 & d_missing == 1 & d_missing_d == 1) 

// nlogit fished mean_avail mean_price2 wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured btnaclosured dummy_prev_days dummy_prev_year_days unem_rate d_c d_d d_p d_cd d_pc d_pd d_pcd  /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) constraints(1) vce(cluster fished_vessel_anon) from(start, skip)
// estimates save ${results}nlogit_FULL_c4_30days.ster, replace
estimates use ${results}nlogit_FULL_c4_30daysV2.ster
estimates store B_30

// ****************** ESTIMATE IV NESTED LOGIT ************************
// // Following Guevara 2015. We need to regress Price on all all the exogenous variables in X, including the intercept,
// // and at least one instrument. Then obtain the residuals and estimate the normal model + residual
// // Bootstrap errors!

// // Step 1: Calculate 30-days Fishmeal moving average
// preserve
// 	collapse (mean) pricefishmealafi , by(set_date set_month set_year)
// 	gen set_date_numeric = date(set_date, "YMD")
// 	format set_date_numeric %td
// 	tset set_date_numeric
// 	tssmooth ma fishmealprice_ma = price, window(30 0)
// 	drop pricefishmealafi set_date set_date_numeric
// 	tempfile fishmeal_ma
// 	save `fishmeal_ma'
// restore

// merge m:m set_month set_year using `fishmeal_ma', nogenerate
// replace fishmealprice_ma = fishmealprice_ma / 1000

// sum fishmealprice_ma mean_price2
// corr fishmealprice_ma mean_price2
// encode selection, gen(selection_num)
// gen species = substr(selection, 5, 8)
// gen ports   = substr(selection, 1, 3)
// encode species, generate(species2)
// encode ports, generate(ports2)

// reg mean_price2 c.fishmealprice_ma##i.species2 mean_avail wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero psdnclosured btnaclosured ///
// 	dummy_prev_days dummy_prev_year_days unem_rate d_c d_d d_p d_cd d_pc d_pd d_pcd  c.weekend#i.selection_num

// // Predict residuals
// predict residuals, residuals
// predict mean_price2_hat, xb


// // Get intial values from previous model
// estimates use ${results}nlogit_FULL_c4_prev_days_2.ster
// matrix start=e(b)

// nlogit fished mean_avail mean_price2_hat wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero ///
// 		psdnclosured btnaclosured dummy_prev_days dummy_prev_year_days unem_rate d_c d_d d_p d_cd d_pc d_pd d_pcd  /// 
// 		|| partp: , base(NOPART) || port: weekend, base(NOPORT) || selection: , ///
// 	base("No-Participation") case(fished_haul) vce(cluster fished_vessel_anon) from(start, skip)
// estimates save ${results}nlogit_FULL_c4_IV.ster, replace
// estimates use ${results}nlogit_FULL_c4_IV.ster
// estimates store B_IV
// matrix start=e(b)


