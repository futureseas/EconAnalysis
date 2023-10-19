**************************************************************
*** Predict monthly vessel participation if squid collapse ***
**************************************************************

global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global logs "${results}Logs\"
global mwtp "${path}WTP estimation\"
cd $path

clear all
cap log close
	log using ${logs}preliminary, text replace

** Import data
import delimited "Stata\rdo_Stata_c4.csv"
replace mean_price = mean_price / 1000
gen id_obs = _n

** Set choice model database
cmset fished_vessel_id time selection
// cmset fished_haul selection
sort id_obs


* Preferred model 
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 
encode species, gen(nspecies)

cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend, base("No-Participation")

// set emptycells drop
// margins, at(mean_avail=gen(mean_avail)) at(mean_avail=gen(mean_avail_modified))
// marginsplot, xdimension(_outcome)

* Total cases = 19,832
	program change_coeff, eclass
		matrix betass = e(b)
		matrix list betass
		scalar xx = (betass[1,14] + betass[1,15] + betass[1,16] + betass[1,20] + betass[1,31] + betass[1,35] + betass[1,36] + betass[1,39] + betass[1,46])/(47-12-9)
		forvalues i=13(1)47 {
			matrix betass[1,`i'] = betass[1,`i'] + scalar(`=xx')
		}
		matrix betass[1,14] = 0
		matrix betass[1,15] = 0
		matrix betass[1,16] = 0
		matrix betass[1,20] = 0
		matrix betass[1,31] = 0
		matrix betass[1,35] = 0
		matrix betass[1,36] = 0
		matrix betass[1,39] = 0
		matrix betass[1,46] = 0
		ereturn repost b = betass, rename
	end
	change_coeff
	matrix list e(b)
	replace mean_avail = 0 if species == "MSQD"
	predict phat, pr
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	drop if selection == "No-Participation"
	keep if species == "MSQD" | species == "PSDN" | species == "NANC"
	gen PORT_AREA_CODE = substr(selection,1,3) 
	keep fished_vessel_id species PORT_AREA_CODE set_year set_month  
	sort fished_vessel_id species PORT_AREA_CODE set_year set_month
    quietly by fished_vessel_id species PORT_AREA_CODE set_year set_month:  gen dup = cond(_N==1,0,_n)
    keep if dup == 1
    drop dup
    rename species PACFIN_SPECIES_CODE
    rename set_month LANDING_MONTH
    rename set_year LANDING_YEAR



**** get real vessel_num!!!

export delimited using "C:\GitHub\EconAnalysis\Participation\R\monthly_participation_pred.csv", replace
