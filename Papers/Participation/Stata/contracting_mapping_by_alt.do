global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}Contraction mapping\"
global logs "${results}Logs\"
global mwtp "${path}WTP estimation\"
cd $path



****************************
**** Paticipation model ****
****************************
global exp_vars "exp_revenue wind_max_220_mh dist_to_cog dist_port_to_catch_area_zero d_missing_all"
global case_vars "i.psdnclosure i.weekend"

clear all
cap log close
	log using ${logs}contrmap, text replace

** Import data
import delimited "C:\Data\PacFIN data\rdo_Stata_c4.csv"
gen id_obs = _n
replace mean_price = mean_price / 1000
gen const_r2 = 1

** Merge historical selection
preserve
 import delimited "Stata\dbp_month_Stata_c4.csv", clear
 tempfile dbp_month
 save dbp_month, replace
restore
merge m:1 selection set_month using "dbp_month.dta"
drop if _merge == 2
replace hist_selection = 0 if _merge == 1
drop _merge
gen species = substr(selection,-4,.) 
replace species = "No-Participation" if species == "tion" 

** Create revenue variable
gen exp_revenue = mean_price * mean_avail
gen d_missing_all = d_missing_d
replace d_missing_all = 1 if d_missing == 1


***********************
** Population shares **
***********************

/* gen delta1 = rnormal(0,0.1)
by fished_vessel_id selection, sort: egen delta = mean(delta1)
drop delta1
 */

** Calculate the population shares
cap drop s ln_s
egen count2 = total(fished)
by selection, sort : egen count1 = total(fished)
gen s=count1/count2
sort s
gen ln_s=log(s) 


** Following Berry (1994), estimate delta as ln(sj)-ln(s) = xb + delta
gen ln_sO = ln_s if selection == "No-Participation"
egen ln_s0 = max(ln_sO)
gen dif_ln_s = ln_s - ln_s0
drop count1 count2 ln_sO ln_s0
reg dif_ln_s $exp_vars, noconstant
predict xb_hat
cap drop delta0
gen delta0 = dif_ln_s - xb_hat
by selection, sort : egen delta = mean(delta0)
drop xb_hat delta0 dif_ln_s 
 

** Estimate intial conditional logit
sort id_obs
cmset fished_haul selection
constraint 1 delta=1

cmclogit fished $exp_vars delta, constraints(1) noconstant casev($case_vars)
matrix start=e(b)
cap drop phat
qui predict phat
qui egen p_share = mean(phat), by(selection)
cap drop lnp_share
qui gen lnp_share = log(p_share)
qui replace delta = delta + ln_s - lnp_share
qui replace delta = 0 if selection == "No-Participation"
    preserve
      collapse (mean) lnp_share ln_s, by(selection)
      gen dif_all = ln_s-lnp_share
      sum dif_all
      local dif = max(abs(r(max)),abs(r(min)))
    restore
 di `dif'

 while `dif'>0.001 {
  set more off
  cmclogit fished $exp_vars delta, from(start, copy) constraints(1) noconstant casev($case_vars)
  matrix start=e(b)
  drop phat p_share lnp_share
  qui predict phat
  qui egen p_share=mean(phat), by(selection)
  qui gen lnp_share = log(p_share)
  qui replace delta = delta + ln_s - lnp_share
  qui replace delta = 0 if selection == "No-Participation"
    preserve
      collapse (mean) lnp_share ln_s, by(selection)
      gen dif_all = ln_s - lnp_share
      sum dif_all
      local dif = max(abs(r(max)),abs(r(min)))
    restore
  di `dif'
}
