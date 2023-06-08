
global path "..." 
global results "${path}8. results\"
global figures "${results}Figures\"
global tables "${results}Tables\"
global contrmap "${path}3. contraction mapping\"
global logs "${results}Log Files\"
global mwtp "${path}4. R WTP estimation\"
global estincome "${path}Census Income Imputation\"

***************************Main Regressions Start*******************************


********************************************************************************

*                            MIXED LOGIT REDRESSION                            * 
*                                2023-01-28                                    *

********************************************************************************
*                             Mixlogit  Regression                             * 
********************************************************************************

use fin_full_choiceset_20230128.dta, claer

set more off
constraint 1 delta=1

*Shock defined as all respiratory diagnosis for either mother or children

clogit choice shocked_aqi shocked_beds fem_aqi edu_aqi hispanic_aqi black_aqi old_aqi  black_beds fem_beds edu_beds hispanic_beds old_beds lincome m1 m2 m3, group(newid)

matrix start=e(b),0,0,0,0,0

eststo ml_all_shocks_indi: mixlogit choice shocked_aqi shocked_beds fem_aqi edu_aqi hispanic_aqi black_aqi old_aqi black_beds fem_beds edu_beds hispanic_beds old_beds lincome m1 m2 m3 delta, group(newid) id(ID79) rand(aqi beds) from(start, copy) constraint(1)


*generate Event Study style variables but for final regression
gen m_asth_shocked_n11yr=((year-year_m_asth)<=-10 & year_m_asth>1979)
gen m_asth_shocked_n10yr=((year-year_m_asth)>-10 & (year-year_m_asth)<=-5 & year_m_asth>1979)
gen m_asth_shocked_n5yr=((year-year_m_asth)>-5 & (year-year_m_asth)<=-3 & year_m_asth>1979)
gen m_asth_shocked_n3yr=((year-year_m_asth)>-3 & (year-year_m_asth)<=0 & year_m_asth>1979)
gen m_asth_shocked=m_asth_shocked_beds>0
gen m_asth_shocked_3yr=((year-year_m_asth)>0 & (year-year_m_asth)<=3 & m_asth_shocked==1)
gen m_asth_shocked_5yr=((year-year_m_asth)>3 & (year-year_m_asth)<=5 & m_asth_shocked==1)
gen m_asth_shocked_10yr=((year-year_m_asth)>5 & (year-year_m_asth)<=10 & m_asth_shocked==1)
gen m_asth_shocked_11yr=((year-year_m_asth)>10 & m_asth_shocked==1)

*save "D:\Google Drive\Datasets\NLSY\County_Analysis\fin_full_choiceset_20230128.dta", replace

local shocks m_asth_shocked_n11yr m_asth_shocked_n10yr m_asth_shocked_n5yr m_asth_shocked_n3yr m_asth_shocked_3yr m_asth_shocked_5yr m_asth_shocked_10yr m_asth_shocked_11yr
foreach v of local shocks {
gen `v'_aqi=`v'*aqi
gen `v'_beds=`v'*beds
}

clogit choice m_asth_shocked_n11yr_aqi m_asth_shocked_n10yr_aqi m_asth_shocked_n5yr_aqi m_asth_shocked_5yr_aqi m_asth_shocked_10yr_aqi m_asth_shocked_11yr_aqi m_asth_shocked_n11yr_beds m_asth_shocked_n10yr_beds m_asth_shocked_n5yr_beds m_asth_shocked_5yr_beds m_asth_shocked_10yr_beds m_asth_shocked_11yr_beds fem_aqi head_edu_num_aqi hispanic_aqi black_aqi pregnant_aqi pregnant_beds black_beds fem_beds head_edu_num_beds hispanic_beds old_aqi old_beds lincome m1 m2 m3 aqi beds, group(newid)

matrix start=e(b),0,0,0,0,0

eststo ml_m_asthma_indi: mixlogit choice m_asth_shocked_n11yr_aqi m_asth_shocked_n10yr_aqi m_asth_shocked_n5yr_aqi m_asth_shocked_5yr_aqi m_asth_shocked_10yr_aqi m_asth_shocked_11yr_aqi m_asth_shocked_n11yr_beds m_asth_shocked_n10yr_beds m_asth_shocked_n5yr_beds m_asth_shocked_5yr_beds m_asth_shocked_10yr_beds m_asth_shocked_11yr_beds fem_aqi head_edu_num_aqi hispanic_aqi black_aqi pregnant_aqi pregnant_beds black_beds fem_beds head_edu_num_beds hispanic_beds old_aqi old_beds lincome m1 m2 m3 delta, group(newid) id(ID79) rand(aqi beds) from(start, copy) constraint(1)

clogit choice m_asth_shocked_n11yr_aqi m_asth_shocked_n10yr_aqi m_asth_shocked_n5yr_aqi m_asth_shocked_5yr_aqi m_asth_shocked_10yr_aqi m_asth_shocked_11yr_aqi fem_aqi head_edu_num_aqi hispanic_aqi black_aqi pregnant_aqi pregnant_beds black_beds fem_beds head_edu_num_beds hispanic_beds old_aqi old_beds lincome m1 m2 m3 aqi beds delta, group(newid) constraint(1)

********************************************************************************
*                         Full Clogit Regression                               * 
********************************************************************************
*Shocked defined as all respiratory diagnosis

eststo cl_all_shocks_indi: clogit choice shocked_aqi shocked_beds fem_aqi edu_aqi hispanic_aqi black_aqi old_aqi pregnant_aqi pregnant_beds black_beds fem_beds edu_beds hispanic_beds old_beds lincome m1 m2 m3  hpi aqi beds delta, group(newid) constraint(1)

*Shocked defined as adults' asthma diagnosis

eststo cl_m_asthma_indi: clogit choice m_asth_shocked_aqi m_asth_shocked_beds edu_aqi fem_aqi hispanic_aqi black_aqi old_aqi pregnant_aqi pregnant_beds black_beds fem_beds edu_beds hispanic_beds old_beds lincome m1 m2 m3 hpi aqi beds delta, group(newid) constraint(1)

*Shocked defined as adults' asthma diagnosis as adults

eststo cl_m_adultasthma_indi: clogit choice m_adultasth_shocked_aqi m_adultasth_shocked_beds edu_aqi fem_aqi hispanic_aqi black_aqi old_aqi pregnant_aqi pregnant_beds black_beds fem_beds edu_beds hispanic_beds old_beds lincome m1 m2 m3 hpi aqi beds delta, group(newid) constraint(1)


*export all results together
esttab * using "${tables}cl_ml_indie.tex", starlevels(* 0.10 ** 0.05 *** 0.01) replace

log close



********************************************************************************
*				OUTDATED
*                              Mloglit coef plot                               * 
*                      By years before and after shock                         * 

********************************************************************************
******** -10+, -10, -5, 5, 10, 10+ ********
clear
set scheme s1color
set obs 6
gen id=_n
replace id=-12.5 if id==1
replace id=-7.5 if id==2
replace id=-2.5 if id==3
replace id=2.5 if id==4
replace id=7.5 if id==5
replace id=12.5 if id==6

*coefs and se are COPIED from results in paper, mixlogit coefficients
gen coef=.2699771 if id==-12.5
replace coef=.1251463 if id==-7.5 
replace coef=.5740778 if id==-2.5
replace coef=-.4483002 if id==2.5
replace coef=-.5619152 if id==7.5
replace coef=-.0549912 if id==12.5

gen se=.1801082 if id==-12.5
replace se=.2591984 if id==-7.5 
replace se=.3058952 if id==-2.5
replace se=.3094453 if id==2.5
replace se=.2419522 if id==7.5
replace se=.2360286 if id==12.5

gen ul=coef+1.64*se
gen ll=coef-1.64*se

twoway rcap ul ll id, color(gs4) yline(0, lpattern(dash) lcolor(gs12)) xline(0, lsty(dot)) xlabel(-12.5 "10+ yrs before" -7.5 "6-10 yrs before" -2.5 "0-5 yrs before" 2.5 "1-5 yrs after" 7.5 "6-10 yrs after" 12.5 "10+ yrs after") xtitle("") xscale(r(-14.5 14.5)) yscale(r(-1.2 1.2)) scale(0.8) legend(label(1 "90% CI")) || scatter coef id, color(gs4) legend(label(2 "Coefficients at Years Before/After Adult Asthma") region(lstyle(none)))

graph export "${figures}EScoefs5.png", replace
