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
global exp_vars "mean_avail mean_price diesel_price wind_max_220_mh d_missing dist_port_to_catch_area_zero d_missing_d ddieselstate psdnclosured msqdclosured msqdweekend"


clear all
cap log close
	log using ${logs}contrmap, text replace

** Import data
import delimited "Stata\rdo_Stata_c4.csv"
gen id_obs = _n
replace mean_price = mean_price / 1000
/* gen delta1 = rnormal(0,0.1)
by fished_vessel_id selection, sort: egen delta = mean(delta1)
drop delta1
 */

** Calculate the population shares
cap drop s ln_s
by fished_vessel_id, sort : egen count2 = total(fished)
by fished_vessel_id selection, sort : egen count1 = total(fished)
gen s=count1/count2
replace s = 1/(10^5) if s == 0
gen ln_s=log(s) 


** Following Berry (1994), estimate delta as ln(sj)-ln(s) = xb + delta

gen ln_sO = ln_s if selection == "No-Participation"
by fished_vessel_id, sort : egen ln_s0 = max(ln_sO)
gen dif_ln_s = ln_s - ln_s0
drop count1 count2 ln_sO ln_s0
reg dif_ln_s $exp_vars, noconstant
predict xb_hat
cap drop delta0
gen delta0 = dif_ln_s - xb_hat
by fished_vessel_id selection, sort : egen delta = mean(delta0)
drop xb_hat delta0 dif_ln_s 
 

** Estimate intial conditional logit
sort id_obs
// cmset fished_vessel_id time selection
cmset fished_haul selection
constraint 1 delta=1
cmclogit fished $exp_vars delta, ///
	base("No-Participation") noconstant constraints(1) 


*Start contraction mapping
matrix start=e(b)
  cap drop phat
  qui predict phat
  cap drop p_share
  qui by fished_vessel_id selection, sort : egen p_share = mean(phat)
  sort id_obs
  cap drop lnp_share
  qui gen lnp_share = log(p_share)
  qui gen delta1 = delta + ln_s - lnp_share
  qui gen deltaO = delta1 if selection == "No-Participation"
  by fished_vessel_id, sort : egen delta0 = max(deltaO)
  sort id_obs
  // qui replace delta1 = delta1 - delta0
  putmata x=(delta1 delta), replace
  mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual and est shares*/
  mata: st_local("dif", strofreal(dif))
  di `dif'
  replace delta = delta1

scalar it = 1
display it

 while `dif'>0.001 {
 	set more off
 	cmclogit fished $exp_vars delta, ///
 	base("No-Participation") noconstant constraints(1)  ///
 	from(start, copy)
 	matrix start=e(b)
  	drop phat p_share lnp_share delta0 deltaO delta1
  	qui predict phat
  	qui by fished_vessel_id selection, sort : egen p_share = mean(phat)
    sort id_obs
  	qui gen lnp_share = log(p_share)
  	qui gen delta1 = delta + ln_s - lnp_share
    qui gen deltaO = delta1 if selection == "No-Participation"
    by fished_vessel_id, sort : egen delta0 = max(deltaO)
    sort id_obs
    // qui replace delta1 = delta1 - delta0
  	putmata x=(delta1 delta), replace
  	mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual adn est shares*/
  	mata: st_local("dif", strofreal(dif))
  	di `dif'
    replace delta = delta1
    scalar it = it + 1
    di it
  }
