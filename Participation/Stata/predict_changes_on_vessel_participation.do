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
sort id_obs
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 
encode species, gen(nspecies)

preserve
	cmclogit fished mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d dist_to_cog ///
		ddieselstate psdnclosured msqdclosured msqdweekend, base("No-Participation")
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
	tab selection
	gen selection_hat = 1
	egen freq = total(selection_hat), by(selection set_year)
	keep selection set_year freq
	sort selection set_year
	quietly by selection set_year:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	drop dup
	egen tot = total(freq), by(set_year)
	gen perc3 = freq/tot
	drop tot freq
	tempfile simulated3
	save simulated3, replace
restore

keep if fished == 1
tab selection
gen selection_hat = 1
egen freq = total(selection_hat), by(selection set_year)
keep selection set_year freq
sort selection set_year
quietly by selection set_year:  gen dup = cond(_N==1,0,_n)
drop if dup > 1
drop dup
egen tot = total(freq), by(set_year)
gen perc = freq/tot
drop tot freq

merge m:m selection using "simulated3.dta", nogen keep(master match) 
replace perc  = 0 if perc == .
replace perc3 = 0 if perc3 == .

export excel using "G:\My Drive\Tables\Participation\Simulated_shares_fig.xlsx", replace
export delimited using "C:\GitHub\EconAnalysis\Participation\R\Simulated_shares_fig.csv", replace
