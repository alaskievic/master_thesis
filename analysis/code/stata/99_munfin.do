*** Fao 2000-2010

clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\fao_andrei.dta"

********** Setting up **********

gsort +cod +year

gen log_pop = log(pesotot)
*replace log_pop=0 if log_pop==.

gen pib_pc = municip_pib/pop

gen log_area = log(geo_area_2010)
gen log_rdpc = log(RDPC)
gen log_pib_pc = log(pib_pc)

gen log_fao = log(sum_fao)
gen log_pqshares = log(pq_shares)
gen log_pquant = log(pq_quant)

foreach v of varlist rev_itr-exp_transp {
	replace `v' = `v' + 1
	gen log_`v' = log(`v')

}

***** Regressions

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\fao_andrei.log"
xtset cod year

***************** DIFERENÇAS **************************************************

gen dlog_fao = log_fao - log_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pquant = log_pquant - log_pquant[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pqshares = log_pqshares - log_pqshares[_n-1] if year == 2010 & cod == cod[_n-1]


gen dlog_rdpc = log_rdpc - log_rdpc[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_pc = log_pib_pc - log_pib_pc[_n-1] if year == 2010 & cod == cod[_n-1]


gen dlog_rev_impost_tot = log_rev_impost_tot - log_rev_impost_tot[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_rev_iptu = log_rev_iptu - log_rev_iptu[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_rev_itr = log_rev_itr - log_rev_itr[_n-1] if year == 2010 & cod == cod[_n-1]


gen dlog_exp_func_total = log_exp_func_total - log_exp_func_total[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_exp_educ = log_exp_educ - log_exp_educ[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_exp_habit = log_exp_habit - log_exp_habit[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_exp_saude = log_exp_saude - log_exp_saude[_n-1] if year == 2010 & cod == cod[_n-1]


gen dT_AGUA = T_AGUA - T_AGUA[_n-1] if year == 2010 & cod == cod[_n-1]
gen dT_ANALF18M = T_ANALF18M - T_ANALF18M[_n-1] if year == 2010 & cod == cod[_n-1]
gen dGINI = GINI - GINI[_n-1] if year == 2010 & cod == cod[_n-1]
gen dT_LUZ = T_LUZ - T_LUZ[_n-1] if year == 2010 & cod == cod[_n-1]
gen dgini_land = gini_land - gini_land[_n-1] if year == 2010 & cod == cod[_n-1]


gen destab_total = estab_total - estab_total[_n-1] if year == 2010 & cod == cod[_n-1]
gen destab_tot_pub = estab_tot_pub - estab_tot_pub[_n-1] if year == 2010 & cod == cod[_n-1]
gen destab_munic = estab_munic - estab_munic[_n-1] if year == 2010 & cod == cod[_n-1]


gen datend_total = atend_total - atend_total[_n-1] if year == 2010 & cod == cod[_n-1]
gen datend_total_pub = atend_total_pub - atend_total_pub[_n-1] if year == 2010 & cod == cod[_n-1]
gen datend_total_priv = atend_total_priv - atend_total_priv[_n-1] if year == 2010 & cod == cod[_n-1]


gen dleitos_total = leitos_total - leitos_total[_n-1] if year == 2010 & cod == cod[_n-1]
gen dleitos_munic = leitos_munic - leitos_munic[_n-1] if year == 2010 & cod == cod[_n-1]
gen dleitos_total_priv = estab_munic - estab_munic[_n-1] if year == 2010 & cod == cod[_n-1]



*foreach v of varlist estab_total-leitos_sus {
	*gen d`v' = `v' - `v'[_n-1] if year == 2010 & cod == cod[_n-1]
*}

** Regressions

eststo clear

eststo: qui reg dlog_rdpc dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dlog_rdpc dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dlog_pib_pc dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dlog_pib_pc dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress


*foreach v of varlist log_rdpc log_pib_pc {
*	eststo clear
*	
*	eststo: qui reghdfe `v' log_fao  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year)
*	
*    eststo: qui reghdfe `v' log_pqshares  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year)
	
*	esttab, se ar2 keep(log_fao log_pqshares)
*}



eststo clear

eststo: qui reg dlog_rev_impost_tot dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dlog_rev_impost_tot dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dlog_rev_iptu dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dlog_rev_iptu dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress


eststo clear

eststo: qui reg dlog_rev_itr dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
eststo: qui reg dlog_rev_itr dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress


*foreach v of varlist log_rev_impost_tot log_rev_iptu log_rev_itr{
*	eststo clear
	
*	eststo: qui reghdfe `v' log_fao  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year) vce (cluster cod)
	
*   eststo: qui reghdfe `v' log_pqshares  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year) vce (cluster cod)
	
*	esttab, se ar2 keep(log_fao log_pqshares)
*}




eststo clear
eststo: qui reg dlog_exp_func_total dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dlog_exp_func_total dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dlog_exp_educ dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dlog_exp_educ dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress



eststo clear
eststo: qui reg dlog_exp_habit dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dlog_exp_habit dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dlog_exp_saude dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dlog_exp_saude dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress


*foreach v of varlist log_exp_func_total	log_exp_educ log_exp_habit log_exp_saude  {
*	eststo clear
	
*	eststo: qui reghdfe `v' log_fao  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year) vce (cluster cod)
	
*   eststo: qui reghdfe `v' log_pqshares  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year) vce (cluster cod)
	
*	esttab, se ar2 keep(log_fao log_pqshares)
*}


eststo clear

eststo: qui reg dGINI dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dGINI dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dT_LUZ dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dT_LUZ dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress


eststo clear

eststo: qui reg dT_AGUA dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dT_AGUA dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dT_ANALF18M dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dT_ANALF18M dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress


eststo clear

eststo: qui reg dgini_land dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dgini_land dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dgini_land dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dgini_land dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress


*foreach v of varlist T_AGUA T_ANALF18M GINI T_LUZ gini_land {
*	eststo clear
	
*	eststo: qui reghdfe `v' log_fao  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year)
	
*   eststo: qui reghdfe `v' log_pqshares  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year)
	
*	esttab, se ar2 keep(log_fao log_pqshares)
*}


eststo clear

eststo: qui reg destab_total dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg destab_total dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg destab_tot_pub dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg destab_tot_pub dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg destab_munic dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg destab_munic dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress


*foreach v of varlist estab_total estab_tot_pub estab_munic {
*	eststo clear
	
*	eststo: qui reghdfe `v' log_fao  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year)
*	
 *   eststo: qui reghdfe `v' log_pqshares  c.dist_state#i.year c.dist_federal#i.year ///
	*c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
	*c.capital_dummy#i.year, absorb (cod year)
	*
	*esttab, se ar2 keep(log_fao log_pqshares)
*}



eststo clear

eststo: qui reg datend_total dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg datend_total dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg datend_total_pub dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg datend_total_pub dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg datend_total_priv dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg datend_total_priv dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress



*foreach v of varlist atend_total atend_total_pub atend_total_priv {
*	eststo clear
	
*	eststo: qui reghdfe `v' log_fao  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year)
	
 *   eststo: qui reghdfe `v' log_pqshares  c.dist_state#i.year c.dist_federal#i.year ///
	*c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
	*c.capital_dummy#i.year, absorb (cod year)
	
	*esttab, se ar2 keep(log_fao log_pqshares) compress
*}

eststo clear

eststo: qui reg dleitos_total dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dleitos_total dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dleitos_munic dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dleitos_munic dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

eststo: qui reg dleitos_total_priv dlog_fao dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy
				
eststo: qui reg dleitos_total_priv dlog_pqshares dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy

esttab, se ar2 keep(dlog_fao dlog_pqshares) compress



*foreach v of varlist leitos_total leitos_munic leitos_total_priv {
*eststo clear	
*	eststo: qui reghdfe `v' log_fao  c.dist_state#i.year c.dist_federal#i.year ///
*	c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
*	c.capital_dummy#i.year, absorb (cod year)
*	
 *   eststo: qui reghdfe `v' log_pqshares  c.dist_state#i.year c.dist_federal#i.year ///
	*c.log_area#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
	*c.capital_dummy#i.year, absorb (cod year)
	
*	esttab, se ar2 keep(log_fao log_pqshares)
*}


log close


