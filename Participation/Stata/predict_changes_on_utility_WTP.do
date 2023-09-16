******************************
*** Calculate changes on catch composition

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


* Preferred model (see below) 
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 
encode species, gen(nspecies)

cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend, base("No-Participation")

// set emptycells drop
// margins, at(mean_avail=gen(mean_avail)) at(mean_avail=gen(mean_avail_modified))
// marginsplot, xdimension(_outcome)

* Total cases = 19,832

preserve
	replace mean_avail = 1 if species == "MSQD"
	predict phat, pr
	predict V, xb
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	summ V
	scalar total_V_1 = r(sum)
	di total_V_1
	rename V V_1
	keep fished_haul V_1
	tempfile simulated1
	save simulated1, replace
restore

preserve
	replace mean_avail = 0 if species == "MSQD"
	predict phat, pr
	predict V, xb
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	summ V
	scalar total_V_2 = r(sum)
	di total_V_2
	rename V V_2
	keep fished_haul V_2
	tempfile simulated2
	save simulated2, replace
restore

preserve
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
	predict V, xb
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	summ V
	scalar total_V_3 = r(sum)
	di total_V_3
	rename V V_3
	keep fished_haul V_3
	tempfile simulated3
	save simulated3, replace
restore

cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend, base("No-Participation")

matrix betass = e(b)
matrix list betass
predict phat, pr
predict V, xb
by fished_haul, sort: egen max_prob = max(phat) 
drop if max_prob != phat
summ V
scalar total_V = r(sum)
di total_V
summ set_year
scalar years = r(max) - r(min) + 1
di years
keep fished_haul V


*** Willigness to pay
di total_V
di total_V_1
di total_V_2
di total_V_3

*** Willigness to pay (High availability squid v/s Squid collapse)
scalar WTP1 = (ln(total_V_1) - ln(total_V_2))/(years*abs(_b[selection:diesel_price])) 
scalar WTP2 = (ln(total_V_1) - ln(total_V_3))/(years*abs(_b[selection:diesel_price])) 
di WTP1
di WTP2

*** Willigness to pay (Historical availability of squid v/s Squid collapse)
scalar WTP1 = (ln(total_V) - ln(total_V_2))/(years*abs(_b[selection:diesel_price])) 
scalar WTP2 = (ln(total_V) - ln(total_V_3))/(years*abs(_b[selection:diesel_price])) 
di WTP1
di WTP2

******************************
merge 1:1 fished_haul using "simulated1.dta", nogen keep(master match) 
merge 1:1 fished_haul using "simulated2.dta", nogen keep(master match) 
merge 1:1 fished_haul using "simulated3.dta", nogen keep(master match) 


*** Willigness to pay (High availability squid v/s Squid collapse)
gen WTP1 = (ln(V_1) - ln(V_2))/abs(_b[selection:diesel_price])
gen WTP2 = (ln(V_1) - ln(V_3))/abs(_b[selection:diesel_price])

*** Willigness to pay (Historical availability of squid v/s Squid collapse)
gen WTP3 = (ln(V) - ln(V_2))/abs(_b[selection:diesel_price]) 
gen WTP4 = (ln(V) - ln(V_3))/abs(_b[selection:diesel_price]) 

summ WTP1 WTP2 WTP3 WTP4

// *** Willigness to pay (High availability squid v/s Squid collapse)
// gen WTP1x = (V_1 - V_2)/abs(_b[selection:diesel_price])
// gen WTP2x = (V_1 - V_3)/abs(_b[selection:diesel_price])

// *** Willigness to pay (Historical availability of squid v/s Squid collapse)
// gen WTP3x = (V - V_2)/abs(_b[selection:diesel_price]) 
// gen WTP4x = (V - V_3)/abs(_b[selection:diesel_price]) 

// summ WTP1x WTP2x WTP3x WTP4x
