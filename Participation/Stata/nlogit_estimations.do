global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global mwtp "${path}WTP estimation\"
cd $path

clear all

*************************************************************************************************************
*** Work to do: 
*
* - Delete trips with low k
*
*************************************************************************************************************


** Import data
import delimited "C:\Data\PacFIN data\rdo_Stata_c4_nhauls5.csv"
replace mean_price = mean_price / 1000
replace mean_price2 = mean_price2 / 1000
replace mean_catch = mean_catch / 1000
replace mean_catch2 = mean_catch2 / 1000
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
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 
replace d_missing_p = d_missing_p2 if mean_price > 50
replace mean_price = mean_price2 if mean_price > 50
gen exp_revenue  = mean_price  * mean_avail
gen exp_revenue2 = mean_price2 * mean_avail
egen species_int = group(species), label
gen d_p = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 0) 
gen d_c = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 0) 
gen d_d = (d_missing_p == 0 & d_missing == 0 & d_missing_d == 1) 
gen d_pc = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 0) 
gen d_pd = (d_missing_p == 1 & d_missing == 0 & d_missing_d == 1) 
gen d_cd = (d_missing_p == 0 & d_missing == 1 & d_missing_d == 1) 
gen d_pcd = (d_missing_p == 1 & d_missing == 1 & d_missing_d == 1) 
sum d_p d_c d_d d_pc d_pd d_cd d_pcd

*** Create species, port and participation.
// gen spec = substr(selection,-4,.) 
// replace spec = " " if spec == "tion" 
// gen port_l = substr(selection,1,3) 
// replace port_l = " " if port_l == "No-" 
// gen part = (selection == "No-Participation")
// egen port = group(port_l), label


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
label variable d_d "Binary: Distance missing "
label variable d_pd "Binary: Distance and price missing"
label variable d_cd "Binary: Availability and distance missing"
label variable d_pcd "Binary: Availability, distance and price missing"



******************************
** Estimate nested logit

** Set choice model database
// cmset fished_haul selection
cmset fished_vessel_id time selection
sort id_obs

*** Create nested logit structure

tab selection 


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
    NoPart: No-Participation)

nlogitgen part = port(Part: BDA | BGA | CWA | ERA | LAA | MNA | MRA | NPA | NPS | SBA | SDA | SFA | SPS, ///
					  NoPart: NoPart)




*** Run nlogit model // exp_revenue wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_c d_d d_pd d_cd d_pcd psdnclosured2 psdntotalclosured msqdweekend

drop if selection == "LAA-RHRG" | /// 
		selection == "LAA-STNA" | ///
		selection == "MNA-ALBC" | ///
		selection == "MNA-LSKT" | ///
		selection == "MNA-ALBC" | ///
		selection == "MNA-UDAB" | ///
		selection == "NPS-DCRB" | ///
		selection == "SBA-UMCK" | ///
		selection == "SFA-CHNK"

drop if selection == "BGA-MSQD" | ///
        selection == "SBA-JMCK" | ///
        selection == "SBA-PBNT" | ///
        selection == "SPS-CHUM"

drop if selection == "BDA-DCRB" | ///
		selection == "SBA-PSDN" | ///
		selection == "SDA-BTNA"


nlogittree selection port part, choice(fished) case(fished_haul) 
cap drop check_if_choice
sort fished_haul
by fished_haul: egen check_if_choice = sum(fished)
tab check_if_choice
keep if check_if_choice
tab check_if_choice

nlogittree selection port part, choice(fished) case(fished_haul) 
nlogit fished $vars || part: , base(NoPart) || port: , base(NoPart) || selection: , base("No-Participation") noconst case(fished_haul) vce(cluster fished_vessel_num)

