
***** Agricultural Structural Change Dataset ***********************************
clear all
set more off,permanently

use ".\agro_struc.dta"

***** Preparing Data *****
gsort +cod +year


forvalues i = 1/6{
    bysort cod: replace women_labor_share = women_labor_share[_n+`i'] if women_labor_share==.
}

bysort cod: replace women_labor_share = women_labor_share[_n-1] if women_labor_share==.

keep if year == 2017 | year == 2006 | year == 1995

gen codreg     = 1 if codstate < 20
replace codreg = 2 if codstate < 30 & codstate >= 20
replace codreg = 3 if codstate < 40 & codstate >= 30
replace codreg = 4 if codstate < 50 & codstate >= 40
replace codreg = 5 if codstate >= 50



gen log_workers = log(n_workers)




***** Mediation analysis ******
gen log_windust = log(w_indust)
gen log_wagro = log(w_agro)
gen log_wserv = log(w_serv)

gen urbshare = pesourb/pesotot

***** Measure differences *****

gen teste = log(n_workers + 1)

gen arapp2 = log(arapp*total_area)

gen napp2 = log(napp*total_num)


replace arapp2 = 0 if arapp2 == .
replace napp2 = 0 if napp2 == .


* Shares
gen arapp3 = log(arapp + 0.0000000000001)
gen napp3 = log(napp + 0.0000000000000001)


* Share of >1000ha of landowners of all land owners
gen propnapp_sh = propnapp/totnum_prop
gen proparapp_sh = proparapp/totarea_prop

* Share of >1000ha of landowners of all land in agriculture
gen propnapp_shtot = propnapp/total_num
gen proparapp_shtot = proparapp/total_area


*** Difference between 2010-2000 exposure - BASELINE ***
gen dfao_baseline  = sum_fao[_n-1] - sum_fao[_n-2] if year == 2017 & cod == cod[_n-1]
gen dfao_baselinecat95  = sum_fao_cattle_1995[_n-1] - sum_fao_cattle_1995[_n-2] if year == 2017 & cod == cod[_n-1]
gen dfao_baselinecatact  = sum_fao_cattle_actual[_n-1] - sum_fao_cattle_actual[_n-2] if year == 2017 & cod == cod[_n-1]
gen dfaochigh = sum_fao_cattle_high_1995[_n-1] - sum_fao_cattle_high_1995[_n-2] if year == 2017 & cod == cod[_n-1]
gen dfao_diff = dfaochigh - dfao_baselinecat95
gen dfao_placebo = sum_fao_placebo_1985 - sum_fao_placebo_1985[_n-1] if year == 2017 & cod == cod[_n-1]

*** Land Inequality ***
gen dgini_land_baseline = gini_land - gini_land[_n-1] if year == 2017 & cod == cod[_n-1]
gen dgini_land_short = gini_land[_n-1] - gini_land[_n-2] if year == 2017 & cod == cod[_n-1]
gen dgini_land_long = gini_land - gini_land[_n-2] if year == 2017 & cod == cod[_n-1]

gen dnapp_baseline = napp - napp[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dnapp_short = napp[_n-1 ] - napp[_n-2] if year == 2017 & cod == cod[_n-1]
gen dnapp_long = napp - napp[_n-2] if year == 2017 & cod == cod[_n-1]

gen dnapp2_baseline = napp2 - napp2[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dnapp2_short = napp2[_n-1 ] - napp2[_n-2] if year == 2017 & cod == cod[_n-1]
gen dnapp2_long = napp2 - napp2[_n-2] if year == 2017 & cod == cod[_n-1]

gen dnapp3_baseline = napp3 - napp3[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dnapp3_short = napp3[_n-1 ] - napp3[_n-2] if year == 2017 & cod == cod[_n-1]
gen dnapp3_long = napp3 - napp3[_n-2] if year == 2017 & cod == cod[_n-1]

gen darapp_baseline = arapp - arapp[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen darapp_short = arapp[_n-1 ] - arapp[_n-2] if year == 2017 & cod == cod[_n-1]
gen darapp_long = arapp - arapp[_n-2] if year == 2017 & cod == cod[_n-1]

gen darapp2_baseline = arapp2 - arapp2[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen darapp2_short = arapp2[_n-1 ] - arapp2[_n-2] if year == 2017 & cod == cod[_n-1]
gen darapp2_long = arapp2 - arapp2[_n-2] if year == 2017 & cod == cod[_n-1]

gen darapp3_baseline = arapp3 - arapp3[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen darapp3_short = arapp3[_n-1 ] - arapp3[_n-2] if year == 2017 & cod == cod[_n-1]
gen darapp3_long = arapp3 - arapp3[_n-2] if year == 2017 & cod == cod[_n-1]

gen dpropnapp_sh_baseline =  propnapp_sh -  propnapp_sh[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dpropnapp_sh_short =  propnapp_sh[_n-1 ] -  propnapp_sh[_n-2] if year == 2017 & cod == cod[_n-1]
gen dpropnapp_sh_long =  propnapp_sh -  propnapp_sh[_n-2] if year == 2017 & cod == cod[_n-1]

gen dproparapp_sh_baseline = proparapp_sh - proparapp_sh[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dproparapp_sh_short = proparapp_sh[_n-1 ] - proparapp_sh[_n-2] if year == 2017 & cod == cod[_n-1]
gen dproparapp_sh_long = proparapp_sh - proparapp_sh[_n-2] if year == 2017 & cod == cod[_n-1]

gen dpropnapp_shtot_baseline = propnapp_shtot - propnapp_shtot[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dpropnapp_shtot_short = propnapp_shtot[_n-1 ] - propnapp_shtot[_n-2] if year == 2017 & cod == cod[_n-1]
gen dpropnapp_shtot_long = propnapp_shtot -propnapp_shtot[_n-2] if year == 2017 & cod == cod[_n-1]

gen dproparapp_shtot_baseline = proparapp_shtot - proparapp_shtot[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dproparapp_shtot_short = proparapp_shtot[_n-1 ] - proparapp_shtot[_n-2] if year == 2017 & cod == cod[_n-1]
gen dproparapp_shtot_long = proparapp_shtot - proparapp_shtot[_n-2] if year == 2017 & cod == cod[_n-1]




gen dlog_workers_baseline 	= log_workers - log_workers[_n-1] 	if year == 2017 & cod == cod[_n-1]
gen dlog_workers_long 		= log_workers - log_workers[_n-2] 	if year == 2017 & cod == cod[_n-1]


gen dl_inten_baseline 	= l_inten - l_inten[_n-1] 	if year == 2017 & cod == cod[_n-1]
gen dl_inten_long 		= l_inten - l_inten[_n-2] 	if year == 2017 & cod == cod[_n-1]


* Openness
gen dopen_exp   = open_exp[_n-1] - open_exp[_n-2] 		if year == 2017 & cod == cod[_n-1]
gen dopen_imp   = open_imp[_n-1] - open_imp[_n-2] 		if year == 2017 & cod == cod[_n-1]
gen dopen_total = open_total[_n-1] - open_total[_n-2] 	if year == 2017 & cod == cod[_n-1]


** Residence
gen dresidence   = share_resid_not_farm - share_resid_not_farm[_n-1] 		if year == 2017 & cod == cod[_n-1]



* Difference between 2015-2010 exposure
gen dfao_short  = sum_fao - sum_fao[_n-1] if year == 2017 & cod == cod[_n-1]
gen dfao_shortcat95 = sum_fao_cattle_1995 - sum_fao_cattle_1995[_n-1] if year == 2017 & cod == cod[_n-1]
gen dfao_shortcatact = sum_fao_cattle_actual - sum_fao_cattle_actual[_n-1] if year == 2017 & cod == cod[_n-1]


*** Difference between 2015-2000 exposure ***
gen dfao_long  = sum_fao - sum_fao[_n-2] if year == 2017 & cod == cod[_n-1]
gen dfao_longcat95  = sum_fao_cattle_1995 - sum_fao_cattle_1995[_n-2] if year == 2017 & cod == cod[_n-1]
gen dfao_longcatact  = sum_fao_cattle_actual - sum_fao_cattle_actual[_n-2] if year == 2017 & cod == cod[_n-1]


*** Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991
gen log_valpw_1995 = log(val_outpw)
replace log_valpw_1995 = log_valpw_1995[_n-2] if year == 2017 & cod == cod[_n-1]

*** Others
gen log_area = log(geo_area_2010)


* Outcomes
gen log_linten = log(l_inten)
gen log_valpw = log(val_outpw)
gen log_landinten = log(((total_area/100)/geo_area_2010))
gen log_valpa = log(totval/total_area)

*gen teste = ((total_area/100)/geo_area_2010)
*replace teste = 1 if teste > 1
*replace teste = log(teste)

gen log_farmland = log(total_area + 1)

gen log_seeds = log(totval_seeds)
gen seedspa = log(totval_seeds/total_area)
gen log_trans = log(trans_value)
gen transpa = log(trans_value/total_area)
gen seedsarea = totarea_seeds/total_area
replace trans_area = 0 if trans_area == .
gen transarea = trans_area/total_area
gen propnshare = prop_num/tot_num
gen propashare = prop_area/total_area
replace totval_agroind = 0 if totval_agroind == .
gen log_agroind = log(totval_agroind + 1)
gen log_agroindpa = log(totval_agroind + 1/total_area)
gen log_agroindpw = log(totval_agroind + 1/n_workers)

replace n_maq = 0 if n_maq == .
gen lmaqteste = log(n_maq+1)
gen lmaq_inten2 = log(((n_maq + 1)/total_area))

drop if year == 1995


*** Outcomes ***

* Shares - PNUD
gen dagro_sh = P_AGRO - P_AGRO[_n-1] if year == 2017 & cod == cod[_n-1]
gen dserv_sh = P_SERV - P_SERV[_n-1] if year == 2017 & cod == cod[_n-1]
gen dindust_sh = P_INDUST - P_INDUST[_n-1] if year == 2017 & cod == cod[_n-1]
gen durbsh = urbsh - urbsh[_n-1] if year == 2017 & cod == cod[_n-1]


gen dlog_wagro = log_wagro- log_wagro[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_windust = log_windust- log_windust[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_wserv = log_wagro- log_wserv[_n-1] if year == 2017 & cod == cod[_n-1]



gen dlog_linten = log_linten - log_linten[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_valpw = log_valpw - log_valpw[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_valpa = log_valpa - log_valpa[_n-1] if year == 2017 & cod == cod[_n-1]


gen dlog_landinten = log_landinten - log_landinten[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dadubo = shares_adubo - shares_adubo[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dagrotox = share_agrotox - share_agrotox[_n-1 ] if year == 2017 & cod == cod[_n-1]

gen dtract = ltract_inten - ltract_inten[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dmaq = lmaq_inten - lmaq_inten[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dlmaq = lmaq_inten2 - lmaq_inten2[_n-1 ] if year == 2017 & cod == cod[_n-1]

gen dmaqteste = lmaqteste - lmaqteste[_n-1 ] if year == 2017 & cod == cod[_n-1]

gen dfarmland = log_farmland - log_farmland[_n-1] if year == 2017 & cod == cod[_n-1]

gen dteste = teste - teste[_n-1] if year == 2017 & cod == cod[_n-1]

gen dlog_seeds = log_seeds - log_seeds[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dseedspa = seedspa - seedspa[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dlog_trans = log_trans - log_trans[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dtranspa = transpa - transpa[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dseedsarea = seedsarea - seedsarea[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dtransarea = transarea - transarea[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dpropnshare = propnshare - propnshare[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dpropashare = propashare - propashare[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dlog_agroind = log_agroind - log_agroind[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dlog_agroindpa = log_agroindpa - log_agroindpa[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dlog_agroindpw = log_agroindpw - log_agroindpw[_n-1 ] if year == 2017 & cod == cod[_n-1]



drop if missing(log_income_1991)
drop if missing(log_popdens_1991)
drop if missing(agr_sh_1991)
drop if missing(analf_1991)
drop if missing(lat)
drop if missing(longit)
drop if missing(rain_daniel)
drop if missing(temp_daniel)
drop if missing(sum_fao_cattle_1995)
drop if missing(gini_land)
drop if missing(transarea)
drop if missing(log_farmland)
drop if missing(share_agrotox)
drop if missing(propnapp)
drop if missing(proparapp)


foreach v of varlist banana_1995-wheat_1995{

	gen d30_`v' = 1 if `v' > 0.3
	replace d30_`v' = 0 if missing(d30_`v')
}

foreach v of varlist banana_1995-wheat_1995{

	gen d50_`v' = 1 if `v' > 0.5
	replace d50_`v' = 0 if missing(d50_`v')
}


gen group50 = cattle_1995 + coffee_1995 + maize_1995 + soybean_1995 +  /// 
			   sugarcane_1995
			   
gen dgroup50 = 1 if group50 > 0.5
replace dgroup50 = 0 if missing(dgroup50)



by cod (year), sort: keep if _N == 2 & year[1] == 2006 & year[_N] == 2017





************************ Summary Statistics ************************************
sort  year
by year: summarize log_farmland  lmaq_inten2 transarea share_agrotox gini_land arapp2 napp2 log_agroind log_valpa log_valpw

by year: summarize sum_fao_cattle_1995



summarize  dfarmland dlmaq dtransarea dagrotox dgini_land_baseline dgini_land_long darapp2_baseline dnapp2_baseline dlog_agroind dlog_valpa dlog_valpw


summarize darapp2_baseline dnapp2_baseline darapp2_long dnapp2_long

summarize dreside

********************************************************************************


****************************** Ferman (2021) ***********************************


/***************************************************

Bruno Ferman

This code presents a simple example to run the inference assessment proposed by Ferman (2019), "A simple way to assess inference methods".

First version: August 10th, 2020

Update: November 7th, 2020
- As shown in a revised version of the paper, there is no need to estimate the model under the null to construct the assessment. The assessment can be constructed by simple replacing the vector of outcomes with iid standard normal varibables. 


***************************************************/


/***************************************************
You should include this part of the code with the program "assessment" at the beginning of your code.

The program has 3 inputs, and should run like:

assessment "command" "D" "other".

"command": this input includes the first part of the estimation command. For example, "reg", "xtreg", "xi: reg", and so on.

"D": covariate of interest (we want to assess inference about this variable)

"other": rest of the command (for example, "X , robust" or "X , cluster(W)") 

This command runs 1000 simulations with iid standard normal distribution for the errors. It is easy to modify the command to consider other parameters for the assessment.

you cannot have any variable in your dataset labeled "random".

This code can be easily adjusted to considered other commands that are not OLS regressions. If the command does not have the form "something" "outcome variable" "covariate of interest" "something", then the code must be modified.

This code consider a 5% test based on a t-statistic using the determined estimator for the variance. You can easily adjust the code to consider alternative inference methods.

***************************************************/

* clear all
set matsize 1000


cap program drop assessment
program assessment, eclass
args command D other  

qui: mat R=J(1000,1,.)

_dots 0, title(Assessment running) reps(1000)
forvalues r=1(1)1000 {

qui: gen random = rnormal()

qui: `command' random `D' `other'
qui: mat R[`r',1]=abs(_b[`D']/_se[`D'])>1.96

* 5% 1.96
* 10% 1.645

qui: drop random


_dots `r' 0

}


qui {
preserve
clear 
svmat R
summ R
local summ = r(mean)
restore
}

di "Inference assessment = `summ'"

end	













**************************


eststo clear
foreach v in dlog_workers_baseline dlog_workers_baseline dl_inten_baseline dl_inten_long{
eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress





*********** Remainig tables

eststo clear
foreach v in dresidence{
eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dresidence{
eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dresidence{
eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dresidence{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress









* Baseline
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Placebo
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_placebo log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_placebo) star(* 0.10 ** 0.05 *** 0.01) compress




* Difference
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_diff log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_diff) star(* 0.10 ** 0.05 *** 0.01) compress







eststo clear
foreach v in dresidence{
eststo: qui reg `v' dfao_diff log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_diff) star(* 0.10 ** 0.05 *** 0.01) compress














* Baseline
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
assessment reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}


* Baseline
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_diff log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_diff) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfaochigh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaochigh) star(* 0.10 ** 0.05 *** 0.01) compress



*****************************************************************



eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long dresidence{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



replace share_resid_other_mun = share_resid_other_mun[_n-1] if missing(share_resid_other_mun)




********************* State years fixed effects? *******************************


* Baseline
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress










********************************************************************************

* Farmland
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_baselinecat95 dfarmland log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95 dfarmland) star(* 0.10 ** 0.05 *** 0.01) compress

* Machine Intensity
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_baselinecat95 dlmaq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95 dlmaq) star(* 0.10 ** 0.05 *** 0.01) compress


* GE Seeds
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_baselinecat95 dtransarea log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95 dtransarea) star(* 0.10 ** 0.05 *** 0.01) compress


* Land Inequality
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_baselinecat95 dgini_land_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95 dgini_land_long) star(* 0.10 ** 0.05 *** 0.01) compress



* Other?
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dlmaq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dlmaq) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dgini_land_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dgini_land_long) star(* 0.10 ** 0.05 *** 0.01) compress


************ Playing with controls

* Baseline
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Distances
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state  i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Agri 
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state log_valpw_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 val_outpa_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


women_labor_share
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state altitude val_outpa_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 ///
 analf_1991 capital_dummy dist_federal dist_state altitude val_outpa_1995 d1_banana_1995-d1_wheat_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 ///
 analf_1991 capital_dummy dist_federal dist_state altitude val_outpa_1995 d2_banana_1995-d2_wheat_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
 capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995 ///
 d30_banana_1995-d30_wheat_1995 dgroup50 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
 capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995 ///
 d50_banana_1995-d50_wheat_1995 dgroup50 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



***************************************************************


* Baseline
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_placeb log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_placebo) star(* 0.10 ** 0.05 *** 0.01) compress





****************************** Openness ****************************************
* Baseline
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



gen fao_open_exp = dfao_baselinecat95 * open_exp_mean
gen fao_open_tot = dfao_baselinecat95 * open_total_mean

gen dfao_open_exp = dfao_baselinecat95 * dopen_exp
gen dfao_open_tot = dfao_baselinecat95 * dopen_total



* Interactions
eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 dopen_exp dfao_open_exp log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95 dopen_exp dfao_open_exp) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dfarmland dlmaq dtransarea dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 dopen_total dfao_open_tot log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95 dopen_total dfao_open_tot) star(* 0.10 ** 0.05 *** 0.01) compress





* Full controls














********************************************************************************
* Aggregation 



collapse (firstnm) codreg (mean) dgini_land_baseline dgini_land_long dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 dfarmland dlmaq dtransarea, by(codmicro)



eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 , vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 , vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

********************************************************************************

drop if year == 2006
*** For the Differences

* Top 10%
xtile pct_fao_10 = dfao_baselinecat95, n(9)
gen faodt10 = 1 if pct_fao_10==9
replace faodt10=0 if faodt10==.


* Top 25%
xtile pct_fao_25 = dfao_baselinecat95, n(4)
gen faodt25 = 1 if pct_fao_25==4
replace faodt25=0 if faodt25==.

* Bottom 10%
gen faodb10 = 1 if pct_fao_10==1
replace faodb10=0 if faodb10==.

* Bottom 25%
gen faodb25 = 1 if pct_fao_25==1
replace faodb25=0 if faodb25==.

* Above and below the median
egen fao_median = median(dfao_baselinecat95)
gen faotmed = 1 if dfao_baselinecat95 > fao_median
replace faotmed=0 if faotmed==.
gen faobmed = 1 if dfao_baselinecat95 < fao_median
replace faobmed=0 if faobmed==.



eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodt10) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' faodt25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodt25) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' faotmed log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faotmed) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' faodb10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodb10) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' faodb25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodb25) star(* 0.10 ** 0.05 *** 0.01) compress



*****************************


eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodt10) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' faodt25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodt25) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' faotmed log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faotmed) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' faodb10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodb10) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' faodb25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodb25) star(* 0.10 ** 0.05 *** 0.01) compress

********************************************************************************


eststo clear
eststo: qui reg dlog_agroind dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dlog_agroind dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlog_agroind dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlog_agroind dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/raissh_a.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Heterogeneous Effect of the Commodity Shock on the Type of Industry) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\begin{table}[h]" "\centering" "\begin{adjustbox}{max width=\textwidth}" "\begin{threeparttable}" "\caption{@title}" "label{tab::empshares}" "\begin{tabular}{l* {5}S[table-format = 1.6]}" ///
"\hline \hline" "\noalign{\vskip 0.2cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel A.} & \multicolumn{4}{c}{$\Delta$ Log Output in Agroindustry}\\" "\noalign{\vskip 0.1cm}")



********************************************************************************



eststo clear
eststo: qui reg dfarmland dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dfarmland dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dfarmland dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dfarmland dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/input_a.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels( "Adj. $ R^{2} $") fmt(3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Heterogeneous Effect of the Commodity Shock on the Type of Industry) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\begin{table}[h]" "\centering" "\begin{adjustbox}{max width=\textwidth}" "\begin{threeparttable}" "\caption{@title}" "label{tab::empshares}" "\begin{tabular}{l* {5}S[table-format = 1.6]}" ///
"\hline \hline" "\noalign{\vskip 0.2cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel A.} & \multicolumn{4}{c}{$\Delta$ Log Total Farmland}\\" "\noalign{\vskip 0.1cm}")





eststo clear
eststo: qui reg dlmaq dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dlmaq dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlmaq dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlmaq dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/input_b.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel B.} & \multicolumn{4}{c}{$\Delta$  Log Machine Intensity}\\" "\noalign{\vskip 0.1cm}") ///


eststo clear
eststo: qui reg dtransarea dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dtransarea dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dtransarea dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dtransarea dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/input_c.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel C.} & \multicolumn{4}{c}{$\Delta$  Share of Land with GE Seeds}\\" "\noalign{\vskip 0.1cm}") ///




eststo clear
eststo: qui reg dagrotox dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dagrotox dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dagrotox dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dagrotox dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/input_d.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect of the Commodity Shock on Sectoral GDP) collabels(none) eqlabels(none) mlabels(none) mgroups(none) ///
prehead("\noalign{\vskip 0.25cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel D.} & \multicolumn{4}{c}{$\Delta$  Share of Farms Using Pesticides}\\" "\noalign{\vskip 0.1cm}") ///
prefoot("\noalign{\vskip 0.1cm}" "\noalign{\vskip 0.3cm}" "\hline" "\noalign{\vskip 0.1cm}" "Rural Share in 1991 & \multicolumn{1}{c}{} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Region FE  & & & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Baseline Controls & & & & \multicolumn{1}{c}{\checkmark}\\") ///
postfoot("\hline" "\end{tabular}" "\begin{tablenotes}[flushleft]" "\setlength{\itemindent}{-2.49997pt}" "\item \textit{Notes:} Robust standard errors in parentheses. *** Significant at the 1\% level; ** Significant at the 5\% level; * Significant at the 10\% level." "\end{tablenotes}" "\end{threeparttable}" "\end{adjustbox}" "\end{table}")





********************************************************************************

eststo clear
eststo: qui reg dgini_land_baseline dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dgini_land_baseline dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dgini_land_baseline dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dgini_land_baseline dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/landapp_a.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Heterogeneous Effect of the Commodity Shock on the Type of Industry) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\begin{table}[h]" "\centering" "\begin{adjustbox}{max width=\textwidth}" "\begin{threeparttable}" "\caption{@title}" "label{tab::empshares}" "\begin{tabular}{l* {5}S[table-format = 1.6]}" ///
"\hline \hline" "\noalign{\vskip 0.2cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel A.} & \multicolumn{4}{c}{$\Delta$ Log Area Farms $>1.000$ ha (2006-2017)}\\" "\noalign{\vskip 0.1cm}")




eststo clear
eststo: qui reg darapp2_long dfao_baselinecat95, vce (cluster cod)
eststo: qui reg darapp2_long dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg darapp2_long dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg darapp2_long dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/landapp_b.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel B.} & \multicolumn{4}{c}{$\Delta$ Log Area Farms $>1.000$ ha (1995-2017)}\\" "\noalign{\vskip 0.1cm}") ///



eststo clear
eststo: qui reg dnapp2_baseline dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dnapp2_baseline dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dnapp2_baseline dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dnapp2_baseline dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/landapp_c.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel C.} & \multicolumn{4}{c}{$\Delta$ Log Number Farms $>1.000$ ha (2006-2017)}\\" "\noalign{\vskip 0.1cm}") ///




eststo clear
eststo: qui reg dnapp2_long dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dnapp2_long dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dnapp2_long dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dnapp2_long dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/landapp_d.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect ofthe Commodity Shock on Sectoral GDP) collabels(none) eqlabels(none) mlabels(none) mgroups(none) ///
prehead("\noalign{\vskip 0.25cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel D.} & \multicolumn{4}{c}{$\Delta$ Log Number Farms $>1.000$ ha (1995-2017)}\\" "\noalign{\vskip 0.1cm}") ///
prefoot("\noalign{\vskip 0.1cm}" "\noalign{\vskip 0.3cm}" "\hline" "\noalign{\vskip 0.1cm}" "Rural Share in 1991 & \multicolumn{1}{c}{} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Region FE  & & & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Baseline Controls & & & & \multicolumn{1}{c}{\checkmark}\\") ///
postfoot("\hline" "\end{tabular}" "\begin{tablenotes}[flushleft]" "\setlength{\itemindent}{-2.49997pt}" "\item \textit{Notes:} Robust standard errors in parentheses. *** Significant at the 1\% level; ** Significant at the 5\% level; * Significant at the 10\% level." "\end{tablenotes}" "\end{threeparttable}" "\end{adjustbox}" "\end{table}")



********************************************************************************

eststo clear
eststo: qui reg darapp2_baseline, vce (cluster cod)
eststo: qui reg darapp2_baseline dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg darapp2_baseline dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg darapp2_baseline dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/landgini_a.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Heterogeneous Effect of the Commodity Shock on the Type of Industry) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\begin{table}[h]" "\centering" "\begin{adjustbox}{max width=\textwidth}" "\begin{threeparttable}" "\caption{@title}" "label{tab::empshares}" "\begin{tabular}{l* {5}S[table-format = 1.6]}" ///
"\hline \hline" "\noalign{\vskip 0.2cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel A.} & \multicolumn{4}{c}{$\Delta$ Land Gini (2006-2017)}\\" "\noalign{\vskip 0.1cm}")






eststo clear
eststo: qui reg dgini_land_long dfao_baselinecat95, vce (cluster cod)
eststo: qui reg dgini_land_long dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dgini_land_long dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dgini_land_long dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/landgini_b.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfao_baselinecat95) replace noabbrev varlabels (dfao_baselinecat95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect of the Commodity Shock on Sectoral GDP) collabels(none) eqlabels(none) mlabels(none) mgroups(none) ///
prehead("\noalign{\vskip 0.25cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel B.} & \multicolumn{4}{c}{$\Delta$  Land Gini (1995-2017)}\\" "\noalign{\vskip 0.1cm}") ///
prefoot("\noalign{\vskip 0.1cm}" "\noalign{\vskip 0.3cm}" "\hline" "\noalign{\vskip 0.1cm}" "Rural Share in 1991 & \multicolumn{1}{c}{} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Region FE  & & & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Baseline Controls & & & & \multicolumn{1}{c}{\checkmark}\\") ///
postfoot("\hline" "\end{tabular}" "\begin{tablenotes}[flushleft]" "\setlength{\itemindent}{-2.49997pt}" "\item \textit{Notes:} Robust standard errors in parentheses. *** Significant at the 1\% level; ** Significant at the 5\% level; * Significant at the 10\% level." "\end{tablenotes}" "\end{threeparttable}" "\end{adjustbox}" "\end{table}")



*************************** Alternative Measures *******************************
drop if year == 2006


* Baseline Inputs
eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Actual
eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' dfao_baselinecatact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecatact) star(* 0.10 ** 0.05 *** 0.01) compress


* No Catlle
eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baseline) star(* 0.10 ** 0.05 *** 0.01) compress



* High inputs
eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' dfaochig log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaochigh) star(* 0.10 ** 0.05 *** 0.01) compress

* Different periods
eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' dfao_longcat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_longcat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dfarmland dlmaq dtransarea{
eststo: qui reg `v' dfao_shortcat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_shortcat95) star(* 0.10 ** 0.05 *** 0.01) compress



* Baseline
eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Actual
eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecatact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecatact) star(* 0.10 ** 0.05 *** 0.01) compress



* No Catlle
eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baseline) star(* 0.10 ** 0.05 *** 0.01) compress



* High inputs
eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfaochig log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaochigh) star(* 0.10 ** 0.05 *** 0.01) compress

* Different periods
eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_longcat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_longcat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_shortcat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_shortcat95) star(* 0.10 ** 0.05 *** 0.01) compress



***************************** Robustness ***************************************
drop if year == 2006

eststo clear
foreach v in dteste dlog_linten dlog_valpw{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress





eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


*** Spatial Correlation ***
* Cluster microreg
eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



* Cluster mesoreg
eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmeso)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmeso)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress




** Conley
*Inputs

eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (50)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (100)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (200)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (400)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

* Land Inequality

eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (50)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (100)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (200)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (400)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

*** AKM ***
drop pr_barley_1995


eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_long{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


reg_ss dfarmland, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dfarmland, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


reg_ss dlmaq, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlmaq, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


reg_ss dtransarea, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dtransarea, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


reg_ss dagrotox, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dagrotox, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


reg_ss dgini_land_baseline, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dgini_land_baseline, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

drop if missing(dgini_land_long)
reg_ss dgini_land_long, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dgini_land_long, shiftshare_var(dfao_baselinecat95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

********************************************************************************



************************* Baseline Regressions *********************************
drop if year == 2006
***** Land Inequality *****
*** Gini Land


eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress





eststo clear
foreach v in dlog_landinten dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dlog_landinten dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_landinten dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dfarmland dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress







eststo clear
foreach v in dlog_valpa dlog_valpw{ 
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dlog_valpa dlog_valpw{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dlog_valpa dlog_valpw{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dlog_valpa dlog_valpw{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg , vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg , vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_baseline) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dteste dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dteste dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dteste dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dteste dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress





eststo clear
foreach v in dteste2 dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dteste2 dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dteste2 dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dteste2 dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress





eststo clear
foreach v in darapp2_baseline darapp2_long darapp2_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in darapp2_baseline darapp2_long darapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in darapp2_baseline darapp2_long darapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in darapp2_baseline darapp2_long darapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dnapp2_baseline dnapp2_long dnapp2_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dnapp2_baseline dnapp2_long dnapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp2_baseline dnapp2_long dnapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dnapp2_baseline dnapp2_long dnapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dnapp_baseline dnapp_long dnapp_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dnapp_baseline dnapp_long dnapp_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp_baseline dnapp_long dnapp_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dnapp_baseline dnapp_long dnapp_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dnapp3_baseline dnapp3_long dnapp3_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dnapp3_baseline dnapp3_long dnapp3_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp3_baseline dnapp3_long dnapp3_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dnapp3_baseline dnapp3_long dnapp3_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress





eststo clear
foreach v in darapp3_baseline darapp3_long darapp3_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in darapp3_baseline darapp3_long darapp3_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in darapp3_baseline darapp3_long darapp3_short{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in darapp3_baseline darapp3_long darapp3_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress




* No Controls
eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Baseline Controls
eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Baseline and State FE
eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Full Controls
eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

*** Appropriation
*>1000ha
* No Controls
eststo clear
foreach v in darapp2_baseline darapp2_long darapp2_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in darapp_baseline darapp_long darapp_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in darapp3_baseline darapp3_long darapp3_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp_baseline dnapp_long dnapp_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp2_baseline dnapp2_long dnapp2_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dnapp3_baseline dnapp3_long dnapp3_short{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

* Baseline Controls
eststo clear
foreach v in darapp_baseline darapp_long darapp_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in darapp2_baseline darapp2_long darapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp_baseline dnapp_long dnapp_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp2_baseline dnapp2_long dnapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

* Baseline and State FE
eststo clear
foreach v in darapp_baseline darapp_long darapp_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in darapp2_baseline darapp2_long darapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp_baseline dnapp_long dnapp_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp2_baseline dnapp2_long dnapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Full Controls
eststo clear
foreach v in darapp_baseline darapp_long darapp_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area  i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in darapp2_baseline darapp2_long darapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area  i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp_baseline dnapp_long dnapp_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area  i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dnapp2_baseline dnapp2_long dnapp2_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area  i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress




* Owners only
eststo clear
foreach v in dproparapp_sh_baseline dproparapp_sh_long dproparapp_sh_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area  i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dproparapp_shtot_baseline dproparapp_shtot_long dproparapp_shtot_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area  i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dpropnapp_sh_baseline dpropnapp_sh_long dpropnapp_sh_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area  i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dpropnapp_shtot_baseline dpropnapp_shtot_long dpropnapp_shtot_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area  i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress









********************************************************************************
***** Input Usage *****

*** No Controls ***

eststo clear
foreach v in dlog_landinten dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

* Baseline Controls
eststo clear
foreach v in dlog_landinten dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Baseline and State FE
eststo clear
foreach v in dlog_landinten dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

* Full Controls
eststo clear
foreach v in dlog_landinten dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

*****



***** Input Usage *****

*** No Controls ***

eststo clear
foreach v in dlandteste dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

* Baseline Controls
eststo clear
foreach v in dlandteste dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


* Baseline and State FE
eststo clear
foreach v in dlandteste dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

* Full Controls
eststo clear
foreach v in dlandteste dlmaq dtransarea dagrotox{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress





















*** Agro Industry ***

eststo clear
foreach v in dlog_agroind dlog_agroindpa dlog_agroindpw{
eststo: qui reg `v' dfao_baselinecat95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_agroind dlog_agroindpa dlog_agroindpw{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_agroind dlog_agroindpa dlog_agroindpw{
eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_agroind dlog_agroindpa dlog_agroindpw{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress




********************************************************************************

******************************* Dummies ****************************************

* Top 10%
xtile pct_dfao_10 = dfao_baselinecat95, n(9)
gen dfaodt10 = 1 if pct_dfao_10==9
replace dfaodt10=0 if dfaodt10==.

* Top 25%
xtile pct_dfao_25 = dfao_baselinecat95, n(4)
gen dfaodt25 = 1 if pct_dfao_25==4
replace dfaodt25=0 if dfaodt25==.

* Bottom 10%
gen dfaodb10 = 1 if pct_dfao_10==1
replace dfaodb10=0 if dfaodb10==.

* Bottom 25%
gen dfaodb25 = 1 if pct_dfao_25==1
replace dfaodb25=0 if dfaodb25==.

* Above and below the median
egen dfao_median = median(dfao_baselinecat95)
gen dfaotmed = 1 if dfao_baselinecat95 > dfao_median
replace dfaotmed=0 if dfaotmed==.


* Top 10%
xtile pct_dfao_10_2 = dfao_baseline, n(9)
gen dfaodt10_base = 1 if pct_dfao_10_2==9
replace dfaodt10_base=0 if dfaodt10_base==.

* Top 25%
xtile pct_dfao_25_2 = dfao_baselinecat95, n(4)
gen dfaodt25_base = 1 if pct_dfao_25_2==4
replace dfaodt25_base=0 if dfaodt25_base==.


********************************************************************************

eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress



eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster codmicro)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
	spatial latitude(lat) longitude (longit) dist (50)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
	spatial latitude(lat) longitude (longit) dist (100)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
	spatial latitude(lat) longitude (longit) dist (200)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



* Land Gini

eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster codmicro)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
	spatial latitude(lat) longitude (longit) dist (50)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
	spatial latitude(lat) longitude (longit) dist (100)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_long dgini_land_short{
    eststo: qui acreg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
	spatial latitude(lat) longitude (longit) dist (200)
}
esttab, se ar2 stat (r2_a N) keep(dfao_baselinecat95) star(* 0.10 ** 0.05 *** 0.01) compress



*******

drop pr_barley_1995


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dlog_valpw, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlog_valpw, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dlog_linten, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlog_linten, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dlog_landinten, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlog_landinten, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dadubo, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dadubo, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dagrotox, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dagrotox, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)



local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dlmaq, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlmaq, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)



*** Land Gini

drop if missing(dgini_land_baseline)
drop if missing(dgini_land_long)
drop if missing(dgini_land_short)

local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dgini_land_baseline, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dgini_land_baseline, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dgini_land_long, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dgini_land_long, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dgini_land_short, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dgini_land_short, shiftshare_var(dfao_baselinecat95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 log_valpw_1995, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baselinecat95) compress

eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress



eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_shortcat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_shortcat95) compress


eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_longcat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_longcat95) compress



eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baselinecatact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baselinecatact) compress


********************************************************************************

eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress


eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baselinecat95) compress


eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_baselinecatact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baselinecatact) compress


************

eststo clear
foreach v in dgini_land_baseline dgini_land_short dgini_land_long{
    eststo: qui reg `v' dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_short dgini_land_long{
    eststo: qui reg `v' dfao_baselinecat95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baselinecat95) compress

eststo clear
foreach v in dgini_land_baseline dgini_land_short dgini_land_long{
    eststo: qui reg `v' dfao_baselinecatact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_baselinecatact) compress



************
eststo clear
eststo: qui ivreg2 dlog_valpw log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_baseline = dfao_baseline), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_baseline = dfao_baseline), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dmaq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_baseline = dfao_baseline), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dgini_land_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_baseline = dfao_baseline), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares_baseline

esttab, se ar2 stat (widstat r2_a N) keep(dshares_baseline dfao_baseline) compress



eststo clear
eststo: qui ivreg2 dlog_valpw log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short = dfao_short), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short = dfao_short), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dmaq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short = dfao_short), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dgini_land_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short = dfao_short), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares_short

esttab, se ar2 stat (widstat r2_a N) keep(dfao_short dshares_short) compress


********************************************************************************

eststo clear
eststo: qui acreg dlog_valpw dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dlog_linten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dlog_landinten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dadubo dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dagrotox dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dlmaq dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress


eststo clear
eststo: qui acreg dlog_valpw dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dlog_linten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dlog_landinten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dadubo dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dagrotox dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dlmaq dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress

eststo clear
eststo: qui acreg dlog_valpw dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dlog_linten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dlog_landinten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dadubo dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dagrotox dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dlmaq dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress




* Trying some loops

* Ainda não da certo usar macros
*local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991


eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress


eststo clear
eststo: qui reg dlog_valpw dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_linten dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_landinten dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dadubo dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dagrotox dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlmaq dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress



eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
  eststo: qui  acreg `v' dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, spatial latitude(lat) longitude (longit) dist (50)
}

esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress


eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui acreg `v' dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
}

esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress

eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui acreg `v' dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
}

esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress




eststo clear
eststo: qui reg dlog_valpw dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_linten dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_landinten dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dadubo dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dagrotox dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlmaq dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
esttab, se ar2 stat ( r2_a N) keep(dfao_short) compress


********************************************************************************


xtset cod year, delta(11)


replace n_maq = 0 if n_maq ==.
gen lmaq = log(n_maq + 1)

gen dlmaq= lmaq - lmaq[_n-1] if year == 2017 & cod == cod[_n-1]

replace n_tract = 0 if n_tract ==.
gen ltract = log(n_tract + 1)

gen dtract= ltract - ltract[_n-1] if year == 2017 & cod == cod[_n-1]


* No Controls ***** Como conseguir o mesmo com reg???
eststo clear

eststo: qui xtreg log_valpw pq_sum, fe vce (cluster cod)
eststo: qui xtreg log_linten pq_sum, fe vce (cluster cod)
eststo: qui xtreg lmaq pq_sum, fe vce (cluster cod)

esttab, se ar2 stat (r2_a N) keep(pq_sum) compress



eststo clear
eststo: qui ivreg2 dlog_valpw (dshares= dfao), cluster(cod) ffirst savefirst
eststo: qui ivreg2 dlog_linten (dshares= dfao), cluster(cod) ffirst savefirst
eststo: qui ivreg2 dlmaq (dshares= dfao), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress


drop if lat ==.
drop if longit ==.

eststo clear
eststo: qui acreg dlog_valpw (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_linten (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlmaq (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress

eststo clear
eststo: qui acreg dlog_valpw (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_linten (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlmaq (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress


eststo clear
eststo: qui acreg dlog_valpw (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_linten (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlmaq (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress