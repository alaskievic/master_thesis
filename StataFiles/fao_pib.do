*** Fao GDP

clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\fao_pib.dta"

********** Setting up **********

gsort +cod +year

foreach v of varlist pib_tot-pib_serv {
	gen `v'_pc = `v'/pesotot
	gen log_`v' = log(`v'_pc)
}

gen log_pop = log(pesotot)
*replace log_pop=0 if log_pop==.

gen log_area = log(geo_area_2010)

gen log_fao = log(sum_fao)
gen log_pqshares = log(pq_shares)
gen log_pquant = log(pq_quant)


***** differences

gen dlog_pib_tot = log_pib_tot - log_pib_tot[_n-10] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_agro = log_pib_agro - log_pib_agro[_n-10] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_indust = log_pib_indust - log_pib_indust[_n-10] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_serv = log_pib_serv - log_pib_serv[_n-10] if year == 2010 & cod == cod[_n-1]


gen dlog_fao = log_fao - log_fao[_n-10] if year == 2010 & cod == cod[_n-1]
gen dlog_shares = log_pqshares - log_pqshares[_n-10] if year == 2010 & cod == cod[_n-1]

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\pib_diff.log"

eststo clear	
eststo: qui reg dlog_pib_tot dlog_fao, robust

eststo: qui reg dlog_pib_tot dlog_fao  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_pib_tot dlog_shares, robust

eststo: qui reg dlog_pib_tot dlog_shares  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress



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

