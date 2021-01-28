
***** RAIS Total Employment and Sector Shares **********************************
clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\rais.dta"

***** Setting up ***************************************************************
gsort +cod +year

* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen rur_sh_1991 = pesorur_1991/pesotot_1991


* Log of Total Employment
foreach v of varlist total_emp-services_complete {
	gen log_`v' = log(`v')
}

* Others
gen log_area = log(geo_area_2010)
gen log_pop = log(pesotot)


******************* Differences ***************************

keep if year==2000 | year==2010

* Measures
gen dfaoc95  = sum_fao_cattle_1995 - sum_fao_cattle_1995[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaocact  = sum_fao_cattle_actual - sum_fao_cattle_actual[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfao  = sum_fao - sum_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dshares  = pq_sum  - pq_sum[_n-1] if year == 2010 & cod == cod[_n-1]

* Employment Shares
gen dagrosh = agriculture_rais - agriculture_rais[_n-1] if year == 2010 & cod == cod[_n-1]
gen dmanush = manufacturing_rais - manufacturing_rais[_n-1] if year == 2010 & cod == cod[_n-1]
gen dmanucsh = manufc_rais - manufc_rais[_n-1] if year == 2010 & cod == cod[_n-1]
gen dservsh = services_rais - services_rais[_n-1] if year == 2010 & cod == cod[_n-1]
gen dservcsh = servicesc_rais - servicesc_rais[_n-1] if year == 2010 & cod == cod[_n-1]

* Total Employment
gen dtotemp = log_total_emp - log_total_emp[_n-1] if year == 2010 & cod == cod[_n-1]
gen dagr = log_agriculture - log_agriculture[_n-1] if year == 2010 & cod == cod[_n-1]
gen dmanu = log_manufacturing - log_manufacturing[_n-1] if year == 2010 & cod == cod[_n-1]
gen dmanuc = log_manufacturing_construc - log_manufacturing_construc[_n-1] if year == 2010 & cod == cod[_n-1]
gen dserv = log_services - log_services[_n-1] if year == 2010 & cod == cod[_n-1]
gen dservc = log_services_complete - log_services_complete[_n-1] if year == 2010 & cod == cod[_n-1]



***** First Differences Regressions ********************************************

drop if year == 2000
drop if missing(dagrosh)
drop if missing(dmanush)
drop if missing(dmanucsh)
drop if missing(dservcsh)
drop if missing(log_income_1991)
drop if missing(log_popdens_1991)
drop if missing(rur_sh_1991)
drop if missing(analf_1991)


*** Baseline Regression ***
eststo clear
foreach v in dagrosh dmanush dservsh{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dtotemp dagr dmanu dmanuc dserv dservc{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


*** FPC High ***

eststo clear
foreach v in dagrosh dmanush dservsh{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991 pc1_high, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dtotemp dagr dmanu dmanuc dserv dservc{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991 pc1_high, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



*** FPC High and State FE ***

eststo clear
foreach v in dagrosh dmanush dservsh{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991 pc1_high i.codstate, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dtotemp dagr dmanu dmanuc dserv dservc{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991 pc1_high i.codstate, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



* Full Controls
eststo clear
foreach v in dagrosh dmanush dservsh{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area pc1_high i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dtotemp dagr dmanu dmanuc dserv dservc{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area pc1_high i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress







*** AKM Correction ***

* Run this all at once
drop pr_barley_1995
local controls log_income_1991 log_popdens_1991 rur_sh_1991 analf_1991

reg_ss dagrosh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dagrosh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dmanush, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dmanush, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dservsh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dservsh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

********************************************************************************



***** Full Panel Regressions ***************************************************

clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\rais.dta"

***** Setting up ***************************************************************
gsort +cod +year

* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen rur_sh_1991 = pesorur_1991/pesotot_1991


* Log of Total Employment
foreach v of varlist total_emp-services_complete {
	gen log_`v' = log(`v')
}

* Others
gen log_area = log(geo_area_2010)
gen log_pop = log(pesotot)


*drop if year > 2010
drop if missing(agriculture_rais)
drop if missing(manufacturing_rais)
drop if missing(services_rais)
drop if missing(log_income_1991)
drop if missing(log_popdens_1991)
drop if missing(rur_sh_1991)
drop if missing(analf_1991)

*** Regressions ***

*** Baseline Controls

* Year <= 2010

************************* Main Regressions *************************************
eststo clear
foreach v in agriculture_rais manufacturing_rais  services_rais{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.rur_sh_1991#i.year c.analf_1991#i.year if year <=2010, absorb (cod year) vce(cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in log_total_emp log_agriculture log_manufacturing  log_services{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.rur_sh_1991#i.year c.analf_1991#i.year if year <=2010, absorb (cod year) vce(cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in agriculture_rais manufacturing_rais manufc_rais services_rais servicesc_rais{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.rur_sh_1991#i.year c.analf_1991#i.year if year <=2010, absorb (cod year) vce(cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in log_total_emp log_agriculture log_manufacturing log_manufacturing_construc log_services log_services_complete{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.rur_sh_1991#i.year c.analf_1991#i.year if year <=2010, absorb (cod year) vce(cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress

* Full Sample
eststo clear
foreach v in agriculture_rais manufacturing_rais manufc_rais services_rais servicesc_rais{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.rur_sh_1991#i.year c.analf_1991#i.year, absorb (cod year) vce(cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in log_total_emp log_agriculture log_manufacturing log_manufacturing_construc log_services log_services_complete{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.rur_sh_1991#i.year c.analf_1991#i.year, absorb (cod year) vce(cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress


* Full Set Controls
eststo clear
foreach v in agriculture_rais manufacturing_rais  services_rais{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.rur_sh_1991#i.year c.analf_1991#i.year c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year ///
c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.pc1_high#i.year i.codstate if year <= 2010, absorb (cod year) vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in log_total_emp log_agriculture log_manufacturing  log_services{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.rur_sh_1991#i.year c.analf_1991#i.year c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year ///
c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.pc1_high#i.year if year <= 2010, absorb (cod year) vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress