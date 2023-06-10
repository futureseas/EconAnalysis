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

clear all
cap log close
	log using ${logs}contrmap, text replace

** Import data
import delimited "Stata/dcm_data.csv"
gen id_obs = _n
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
gen ln_sO = ln_s if selection == "No-Participation"
by fished_vessel_id, sort : egen ln_s0 = max(ln_sO)
gen dif_ln_s = ln_s - ln_s0
drop count1 count2 ln_sO ln_s0

/* ** Following Berry (1994), estimate delta as ln(sj)-ln(s) = xb + delta
reg dif_ln_s mean_rev_adj travel_cost wind_max_220_mh i.dummy_last_day msqdclosure psdnclosure msqdweekend, noconstant
predict xb_hat
cap drop delta0
gen delta0 = dif_ln_s - xb_hat
by fished_vessel_id selection, sort : egen delta = mean(delta0)
drop xb_hat delta0 dif_ln_s */
gen delta = dif_ln_s


** Estimate intical conditional logit
sort id_obs
// cmset fished_vessel_id time selection
cmset fished_haul selection
constraint 1 delta=1
cmclogit fished delta, ///
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
  qui replace delta1 = delta1 - delta0
  putmata x=(delta1 delta), replace
  mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual adn est shares*/
  mata: st_local("dif", strofreal(dif))
  di `dif'
  replace delta = delta1

 while `dif'>0.001 {
 	set more off
 	cmclogit fished delta, ///
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
    qui replace delta1 = delta1 - delta0
  	putmata x=(delta1 delta), replace
  	mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual adn est shares*/
  	mata: st_local("dif", strofreal(dif))
  	di `dif'
    replace delta = delta1
  }


reg delta mean_rev_adj travel_cost wind_max_220_mh i.dummy_last_day i.msqdclosure i.psdnclosure i.msqdweekend ///
    if fished == 1 & selection != "No-Participation", noconstant 
  cap drop theta
  predict theta_pred, residuals
  by fished_vessel_id selection, sort : egen theta = mean(theta_pred)
  sort id_obs

constraint 2 theta=1
cmclogit fished mean_rev_adj travel_cost wind_max_220_mh i.dummy_last_day i.msqdclosure i.psdnclosure i.msqdweekend theta, ///
  base("No-Participation") noconstant constraints(2) 


*Start contraction mapping
matrix start=e(b)
  cap drop phat
  qui predict phat
  cap drop p_share
  qui by fished_vessel_id selection, sort : egen p_share = mean(phat)
  sort id_obs
  cap drop lnp_share 
  cap drop thetaO
  cap drop theta0
  cap drop theta1
  qui gen lnp_share = log(p_share)
  qui gen theta1 = theta + ln_s - lnp_share
  qui gen thetaO = theta1 if selection == "No-Participation"
  by fished_vessel_id, sort : egen theta0 = max(thetaO)
  sort id_obs
  qui replace theta1 = theta1 - theta0
  putmata x=(theta1 theta), replace
  mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual adn est shares*/
  mata: st_local("dif", strofreal(dif))
  di `dif'
  replace theta = theta1



// keep cntyyr lnp_share ln_s delta
//  qui bys cntyyr: gen count=_n
//  qui keep if count==1
 
// save allyears_cl_converged.dta, replace

// *Mixlogit until final convergence
// clogit choice m_asth_shocked_aqi m_asth_shocked_beds fem_beds fem_aqi edu_aqi hispanic_aqi black_aqi old_aqi black_beds edu_beds hispanic_beds old_beds lincome m1 m2 m3 hpi, group(newid)
// matrix start=e(b),0,0,0,0,0
// mixlogit choice m_asth_shocked_aqi m_asth_shocked_beds fem_beds fem_aqi edu_aqi hispanic_aqi black_aqi old_aqi black_beds edu_beds hispanic_beds old_beds lincome m1 m2 m3 delta hpi, group(newid) id(ID79) rand(aqi beds) from(start, copy) constraint(1)
//   matrix start=e(b)
//   cap drop phat p_share lnp_share
//   qui mixlpred phat 
//   qui gegen p_share=mean(phat), by(cntyyr)
//   qui gen lnp_share = log(p_share)
//   qui replace delta = delta + ln_s - lnp_share
//   putmata x=(lnp_share ln_s), replace
//   mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual adn est shares*/
//   mata: st_local("dif", strofreal(dif))
//   di `dif'
  
// while `dif'>0.001 {
//  set more off
// mixlogit choice m_asth_shocked_aqi m_asth_shocked_beds fem_beds fem_aqi edu_aqi hispanic_aqi black_aqi old_aqi black_beds edu_beds hispanic_beds old_beds lincome_nlsy m1 m2 m3 delta hpi, group(newid) id(ID79) rand(aqi beds) from(start, copy) constraint(1)
//   matrix start=e(b)
//   cap drop phat p_share lnp_share
//   qui mixlpred phat 
//   qui gegen p_share=mean(phat), by(cntyyr)
//   qui gen lnp_share = log(p_share)
//   qui replace delta = delta + ln_s - lnp_share
//   putmata x=(lnp_share ln_s), replace
//   mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual adn est shares*/
//   mata: st_local("dif", strofreal(dif))
//   di `dif'
// }

// keep cntyyr lnp_share ln_s delta
//  qui bys cntyyr: gen count=_n
//  qui keep if count==1
 
// save allyears_ml_converged.dta, replace

// log close


/**** Run mixed-logit
cmmixlogit fished
estimates save mixed_logit_results // save model
estimates use mixed_logit_results // load results*/



/*# #---------------------------------------#
# ## Generate and format the predictions ##
# #---------------------------------------#
#  
# fits <- fitted(m5, outcome = FALSE)
# mfits <- reshape2::melt(fits) %>%
#    dplyr::rename(fished_haul = Var1) %>%
#    dplyr::rename(selection_pred = Var2)
# 
# ## Compare to correct prediction using max probability value
# pred_tows <- mfits %>% group_by(fished_haul) %>% filter(value == max(value)) %>% as.data.frame
# pred_tows <- merge(rdo, pred_tows, by = "fished_haul") %>% 
#  filter(fished == TRUE) %>%
#  mutate(correct = ifelse(selection_pred == selection, 1, 0))
# pred_tows2 <- mfits %>% group_by(fished_haul) %>% filter(value == max(value)) %>% as.data.frame
# pred_tows2 <- merge(rdo, pred_tows2, by = "fished_haul") %>%
#   filter(fished == TRUE) %>%
#   mutate(correct = ifelse(selection_pred == selection, 1, 0)) %>%
#   filter(selection != "No-Participation")
# 
# sum(pred_tows$correct) / nrow(pred_tows) # 60% accuracy!
# sum(pred_tows2$correct) / nrow(pred_tows2) ## 52% accuracy!*/
