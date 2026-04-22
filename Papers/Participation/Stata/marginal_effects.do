global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
cd $path
clear all




*********************
use "C:\Data\PacFIN data\rdo_Stata_c4_full.dta", clear

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
nlogitgen port = selection( ///
	CMCK: LAA-CMCK | SBA-CMCK, ///
	MSQD: LAA-MSQD | MNA-MSQD | MRA-MSQD | SBA-MSQD | SFA-MSQD, ///
	PSDN: LAA-PSDN | MNA-PSDN, ///
	NANC: LAA-NANC | MNA-NANC | SFA-NANC, ///
	TUNA: LAA-YTNA | LAA-BTNA, ///
    NOPORT: No-Participation)
nlogitgen partp = port(PART: CMCK | MSQD | PSDN | NANC | TUNA, NOPART: NOPORT)
nlogittree selection port partp, choice(fished) case(fished_haul) 
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 
encode species, gen(nspecies)
******************

*** load model
estimates use ${results}nlogit_FULL_BTNA_2.ster
estimates store nlogit_estimation
estimates describe nlogit_estimation


*** predict

qui sum mean_avail if species == "MSQD"
scalar min_msqd = `r(min)'
scalar max_msqd = `r(max)'
di min_msqd
di max_msqd

preserve
	replace mean_avail = min_msqd if species == "MSQD"
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	tab species
	gen species_hat = 1
	egen freq = total(species_hat), by(species)
	keep species freq
	sort species 
	quietly by species:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	drop dup
	egen tot = total(freq)
	gen perc1 = freq/tot
	drop tot freq
	tempfile simulated1
	save simulated1, replace
restore

	replace mean_avail = max_msqd  if species == "MSQD"
	qui predict phat
	by fished_haul, sort: egen max_prob = max(phat) 
	drop if max_prob != phat
	tab species 
	gen species_hat = 1 
	egen freq = total(species_hat), by(species)
	keep species freq
	sort species
	quietly by species:  gen dup = cond(_N==1,0,_n)
	drop if dup > 1
	drop dup
	egen tot = total(freq)
	gen perc2 = freq/tot
	drop tot freq
	
merge m:m species using "simulated1.dta", nogen keep(master match) 
