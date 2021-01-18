
***** Municipal GDPs ***********************************************************
clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\municip_gdp.dta"

***** Setting up ***************************************************************
gsort +cod +year

* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991


* Outcomes
foreach v of varlist pib_tot-pib_serv {
	gen `v'_pc = `v'/pesotot
	gen log_`v' = log(`v'_pc)
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

* GDP
gen dlog_pib_tot = log_pib_tot - log_pib_tot[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_agro = log_pib_agro - log_pib_agro[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_indust = log_pib_indust - log_pib_indust[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_serv = log_pib_serv - log_pib_serv[_n-1] if year == 2010 & cod == cod[_n-1]




***** First Differences Regressions ********************************************

drop if year == 2000
drop if missing(dlog_pib_to)
drop if missing(dlog_pib_agro)
drop if missing(dlog_pib_indust)
drop if missing(dlog_pib_serv)
drop if missing(log_income_1991)
drop if missing(log_popdens_1991)
drop if missing(agr_sh_1991)
drop if missing(analf_1991)


*** Baseline Regression ***
eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


*** Location Fixed Effects ***

eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



*** Low and High First Principal components Controls ***

eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 pc1_low, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 pc1_high, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


*** AKM Correction ***

* Run this all at once
drop pr_barley_1995
local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991

reg_ss dlog_pib_tot, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dlog_pib_tot, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dlog_pib_agro, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dlog_pib_agro, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dlog_pib_indust, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dlog_pib_indust, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dlog_pib_serv, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dlog_pib_serv, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

********************************************************************************



***** Full Panel Regressions ***************************************************

clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\municip_gdp.dta"

gsort +cod +year

* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991


* Outcomes
foreach v of varlist pib_tot-pib_serv {
	gen `v'_pc = `v'/pesotot
	gen log_`v' = log(`v'_pc)
}



* Others
gen log_area = log(geo_area_2010)
gen log_pop = log(pesotot)

drop if year >2010
drop if missing(log_pib_tot)
drop if missing(log_pib_agro)
drop if missing(log_pib_indust)
drop if missing(log_pib_serv)
drop if missing(log_income_1991)
drop if missing(log_popdens_1991)
drop if missing(agr_sh_1991)
drop if missing(analf_1991)

*** Regressions ***

* Baseline Controls
eststo clear
foreach v in log_pib_tot log_pib_agro log_pib_indust log_pib_serv{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb (cod year) vce(cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress


* Full Set Controls
eststo clear
foreach v in log_pib_tot log_pib_agro log_pib_indust log_pib_serv{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year ///
c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year, absorb (cod year) vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress





