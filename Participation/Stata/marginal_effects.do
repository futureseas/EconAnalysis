global path "C:\GitHub\EconAnalysis\Participation\" 
global results "${path}Results\"
cd $path
clear all

estimates use ${results}nlogit_FULL_BTNA_2.ster
estimates store nlogit_estimation
estimates describe nlogit_estimation
