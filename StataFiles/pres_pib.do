*** GDP
clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\pres_pib.dta"

********** Setting up **********

keep if year==2000 | year==2010
gsort +cod +year

drop gini_land1_y

rename gini_land1 gini_land2
rename gini_land1_x gini_land1



* controls
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



* Interaction
gen ineq_shares1 = gini_land1 * pq_sum
gen ineq_shares2 = gini_land2 * pq_sum
gen ineq_fao1 = gini_land1 * sum_fao
gen ineq_fao2 = gini_land2 * sum_fao





******************* Differences ***************************

* Measures
gen dfao  = sum_fao - sum_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dshares  = pq_sum  - pq_sum[_n-1] if year == 2010 & cod == cod[_n-1]

* GDP
gen dlog_pib_tot = log_pib_tot - log_pib_tot[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_agro = log_pib_agro - log_pib_agro[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_indust = log_pib_indust - log_pib_indust[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_serv = log_pib_serv - log_pib_serv[_n-1] if year == 2010 & cod == cod[_n-1]




* Land Inequality
gen dgini_land1 = gini_land1 - gini_land1[_n-1] if year == 2010 & cod == cod[_n-1]
gen dgini_land2 = gini_land2 - gini_land2[_n-1] if year == 2010 & cod == cod[_n-1]


* Interactions
gen dineq_shares1 = ineq_shares1 - ineq_shares1[_n-1] if year == 2010 & cod == cod[_n-1]
gen dineq_shares2 = ineq_shares2 - ineq_shares2[_n-1] if year == 2010 & cod == cod[_n-1]
gen dineq_fao1 = ineq_fao1 - ineq_fao1[_n-1] if year == 2010 & cod == cod[_n-1]
gen dineq_fao2 = ineq_fao2 - ineq_fao2[_n-1] if year == 2010 & cod == cod[_n-1]




********************************************************************************
eststo clear
eststo: qui ivreg2 dlog_pib_tot log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress



eststo clear
eststo: qui ivreg2 dlog_pib_tot log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares2 = dfao dineq_fao2), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares2

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares2 dineq_fao2) compress


eststo clear
eststo: qui ivreg2 dlog_pib_agro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress




eststo clear
eststo: qui ivreg2 dlog_pib_agro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares2 = dfao dineq_fao2), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares2

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares2 dineq_fao2) compress






eststo clear
eststo: qui ivreg2 dlog_pib_indust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress


eststo clear
eststo: qui ivreg2 dlog_pib_indust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares2 = dfao dineq_fao2), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares2

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares2 dineq_fao2) compress



eststo clear
eststo: qui ivreg2 dlog_pib_serv log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress


eststo clear
eststo: qui ivreg2 dlog_pib_serv log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares2 = dfao dineq_fao2), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares2

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares2 dineq_fao2) compress


********************************************************************************
eststo clear

eststo: qui ivreg2 dlog_pib_tot (dlog_shares = dlog_fao), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_pib_tot dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: estimates restore _ivreg2_dlog_shares
 
esttab, se ar2 stat (widstat) keep(dlog_shares dlog_fao) compress




eststo clear	
eststo: qui reg dlog_pib_agro dlog_fao, robust

eststo: qui reg dlog_pib_agro dlog_fao  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_pib_agro dlog_shares, robust

eststo: qui reg dlog_pib_agro dlog_shares  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress



eststo clear

eststo: qui ivreg2 dlog_pib_agro (dlog_shares = dlog_fao), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_pib_agro dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: estimates restore _ivreg2_dlog_shares
 
esttab, se ar2 stat (widstat) keep(dlog_shares dlog_fao) compress



eststo clear	
eststo: qui reg dlog_pib_indust dlog_fao, robust

eststo: qui reg dlog_pib_indust dlog_fao  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_pib_indust dlog_shares, robust

eststo: qui reg dlog_pib_indust dlog_shares  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress



eststo clear

eststo: qui ivreg2 dlog_pib_indust (dlog_shares = dlog_fao), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_pib_indust dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: estimates restore _ivreg2_dlog_shares
 
esttab, se ar2 stat (widstat) keep(dlog_shares dlog_fao) compress





eststo clear	
eststo: qui reg dlog_pib_serv dlog_fao, robust

eststo: qui reg dlog_pib_serv dlog_fao  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_pib_serv dlog_shares, robust

eststo: qui reg dlog_pib_serv dlog_shares  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress



eststo clear

eststo: qui ivreg2 dlog_pib_serv (dlog_shares = dlog_fao), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_pib_serv dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: estimates restore _ivreg2_dlog_shares
 
esttab, se ar2 stat (widstat) keep(dlog_shares dlog_fao) compress

log close


********************************************************
reghdfe log_fao, absorb (cod year) residuals(red1_fao)

reghdfe log_pqshares, absorb (cod year) residuals(red1_sh)

reghdfe log_fao c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year ///
				c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year, absorb (cod year) residuals(red2_fao)
	
reghdfe log_pqshares c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year ///
				c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year, absorb (cod year) residuals(red2_sh)	

reghdfe log_fao c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year ///
					  c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.temp_outono#i.year ///
					  c.temp_inverno#i.year c.temp_primavera#i.year c.temp_verao#i.year c.rain_verao#i.year ///
					  c.rain_outono#i.year c.rain_inverno#i.year c.rain_primavera#i.year, ///
					  absorb (cod year) residuals(red3_fao)


reghdfe log_pqshares c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year ///
					  c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.temp_outono#i.year ///
					  c.temp_inverno#i.year c.temp_primavera#i.year c.temp_verao#i.year c.rain_verao#i.year ///
					  c.rain_outono#i.year c.rain_inverno#i.year c.rain_primavera#i.year, ///
					  absorb (cod year) residuals(red3_sh)

					  
*********quantiles
foreach v of varlist red1_fao-red3_sh{
xtile q10`v' = `v', nq(10)
}				  

foreach v of varlist q10red1_fao-q10red3_sh{
gen d`v' = 0
}

foreach v of varlist q10red1_fao-q10red3_sh{
replace d`v' = 1 if `v' == 10
}


foreach v of varlist red1_fao-red3_sh{
xtile q25`v' = `v', nq(5)
}						  

foreach v of varlist q25red1_fao-q25red3_sh{
gen d`v' = 0
}

foreach v of varlist q25red1_fao-q25red3_sh{
replace d`v' = 1 if `v' == 5
}



***** Regressions

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\fao_pib.log"


eststo clear

eststo: qui reghdfe log_pib_tot log_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_tot log_pqshares c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_tot dq10red2_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_tot dq10red2_sh c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_tot dq25red2_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_tot dq25red2_sh c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)


esttab, se ar2 keep(log_fao log_pqshares dq10red2_fao dq10red2_sh dq25red2_fao dq25red2_sh) compress



eststo clear

eststo: qui reghdfe log_pib_agro log_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_agro log_pqshares c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_agro dq10red2_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_agro dq10red2_sh c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_agro dq25red2_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_agro dq25red2_sh c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)


esttab, se ar2 keep(log_fao log_pqshares dq10red2_fao dq10red2_sh dq25red2_fao dq25red2_sh) compress





eststo clear

eststo: qui reghdfe log_pib_indust log_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_indust log_pqshares c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_indust dq10red2_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_indust dq10red2_sh c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_indust dq25red2_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_indust dq25red2_sh c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)


esttab, se ar2 keep(log_fao log_pqshares dq10red2_fao dq10red2_sh dq25red2_fao dq25red2_sh) compress





eststo clear

eststo: qui reghdfe log_pib_serv log_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_serv log_pqshares c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_serv dq10red2_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_serv dq10red2_sh c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_serv dq25red2_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_serv dq25red2_sh c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)


esttab, se ar2 keep(log_fao log_pqshares dq10red2_fao dq10red2_sh dq25red2_fao dq25red2_sh) compress


log close


****** comparison


eststo clear

eststo: qui reghdfe log_pib_indust log_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_indust log_pqshares c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)


eststo: qui ivreghdfe log_pib_indust c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year (log_pqshares = log_fao), absorb (cod year) cluster (cod)

esttab, se ar2 keep(log_fao log_pqshares) compress



eststo clear

eststo: qui reghdfe log_pib_agro log_fao c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_agro log_pqshares c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)


eststo: qui ivreghdfe log_pib_agro c.dist_state#i.year c.dist_federal#i.year ///
c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year ///
c.capital_dummy#i.year (log_pqshares = log_fao), absorb (cod year) cluster (cod)

esttab, se ar2 keep(log_fao log_pqshares) compress

