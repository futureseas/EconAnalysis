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
import delimited "dcm_data.csv"
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
drop count1 count2 
sort id_obs

** Set choice model database
cmset fished_vessel_id time selection

*Estimate intical conditional logit
cmclogit fished mean_rev_adj travel_cost wind_max_220_mh, ///
	base("No-Participation") vce(cluster fished_vessel_id)

gen delta = 0
replace delta = _b[BDA_DCRB:_cons] if selection == "BDA-DCRB"
replace delta = _b[BDA_MSQD:_cons] if selection == "BDA-MSQD"
replace delta = _b[CBA_MSQD:_cons] if selection == "CBA-MSQD"
replace delta = _b[ERA_MSQD:_cons] if selection == "ERA-MSQD"
replace delta = _b[LAA_BTNA:_cons] if selection == "LAA-BTNA"
replace delta = _b[LAA_CMCK:_cons] if selection == "LAA-CMCK"
replace delta = _b[LAA_JMCK:_cons] if selection == "LAA-JMCK"
replace delta = _b[LAA_MSQD:_cons] if selection == "LAA-MSQD"
replace delta = _b[LAA_NANC:_cons] if selection == "LAA-NANC"
replace delta = _b[LAA_PBNT:_cons] if selection == "LAA-PBNT"
replace delta = _b[LAA_PSDN:_cons] if selection == "LAA-PSDN"
replace delta = _b[LAA_RHRG:_cons] if selection == "LAA-RHRG"
replace delta = _b[LAA_STNA:_cons] if selection == "LAA-STNA"
replace delta = _b[LAA_YTNA:_cons] if selection == "LAA-YTNA"
replace delta = _b[MNA_ALBC:_cons] if selection == "MNA-ALBC"
replace delta = _b[MNA_CMCK:_cons] if selection == "MNA-CMCK"
replace delta = _b[MNA_JMCK:_cons] if selection == "MNA-JMCK"
replace delta = _b[MNA_LSKT:_cons] if selection == "MNA-LSKT"
replace delta = _b[MNA_MSQD:_cons] if selection == "MNA-MSQD"
replace delta = _b[MNA_NANC:_cons] if selection == "MNA-NANC"
replace delta = _b[MNA_PSDN:_cons] if selection == "MNA-PSDN"
replace delta = _b[MNA_UDAB:_cons] if selection == "MNA-UDAB"
replace delta = _b[MRA_MSQD:_cons] if selection == "MRA-MSQD"
replace delta = _b[NPA_MSQD:_cons] if selection == "NPA-MSQD"
replace delta = _b[SBA_CMCK:_cons] if selection == "SBA-CMCK"
replace delta = _b[SBA_JMCK:_cons] if selection == "SBA-JMCK"
replace delta = _b[SBA_MSQD:_cons] if selection == "SBA-MSQD"
replace delta = _b[SBA_PBNT:_cons] if selection == "SBA-PBNT"
replace delta = _b[SBA_PSDN:_cons] if selection == "SBA-PSDN"
replace delta = _b[SBA_UMCK:_cons] if selection == "SBA-UMCK"
replace delta = _b[SDA_BTNA:_cons] if selection == "SDA-BTNA"
replace delta = _b[SFA_CHNK:_cons] if selection == "SFA-CHNK"
replace delta = _b[SFA_DCRB:_cons] if selection == "SFA-DCRB"
replace delta = _b[SFA_MSQD:_cons] if selection == "SFA-MSQD"
replace delta = _b[SFA_NANC:_cons] if selection == "SFA-NANC"


*Estimate intical conditional logit
constraint 1 delta=1
cmclogit fished mean_rev_adj travel_cost wind_max_220_mh delta, ///
	base("No-Participation") noconstant constraints(1) vce(cluster fished_vessel_id)


*Start contraction mapping
matrix start=e(b)
  cap drop phat
  qui predict phat
  cap drop p_share
  qui by fished_vessel_id selection, sort : egen p_share = mean(phat)
  cap drop lnp_share
  qui gen lnp_share = log(p_share)
  qui replace delta = delta + ln_s - lnp_share
  putmata x=(lnp_share ln_s), replace
  mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual adn est shares*/
  mata: st_local("dif", strofreal(dif))
  di `dif'

 while `dif'>0.001 {
 	set more off
 	cmclogit fished mean_rev_adj travel_cost wind_max_220_mh delta, ///
 	base("No-Participation") noconstant constraints(1) vce(cluster fished_vessel_id) ///
 	from(start, copy)
 	matrix start=e(b)
  	drop phat p_share lnp_share
  	qui predict phat
  	qui by fished_vessel_id selection, sort : egen p_share = mean(phat)
  	qui gen lnp_share = log(p_share)
  	qui replace delta = delta + ln_s - lnp_share
  	putmata x=(lnp_share ln_s), replace
  	mata: dif=mreldif(x[.,1],x[.,2]) /*get initial relative diff between actual adn est shares*/
  	mata: st_local("dif", strofreal(dif))
  	di `dif'
  }

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
