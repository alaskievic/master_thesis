clear all

set more off,permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\municip_pib_final.dta"

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\municip_pib_final.log"

********** Setting up **********

gsort +cod +year

gen log_pop = log(pesotot)
replace log_pop=0 if log_pop==.

gen log_area = log(geo_area_2010)
replace log_area=0 if log_area==.

gen log_pq_shares = log(pq_shares)
replace log_pq_shares=0 if log_pq_shares==.

foreach v of varlist pib_tot-pib_serv {
	replace `v' = `v' + 1
	gen `v'_pc = `v'/pesotot
	
}

foreach v of varlist pib_tot_pc-pib_serv_pc {
	gen log_`v' = log(`v')
	replace log_`v'=0 if log_`v'==.
	
}

*Residuals
reghdfe log_pq_shares, absorb (cod year) residuals(log_pq_shares_resid)

reghdfe log_pq_shares c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.temp_outono#i.year c.temp_inverno#i.year c.temp_primavera#i.year c.temp_verao#i.year c.rain_verao#i.year c.rain_outono#i.year c.rain_inverno#i.year c.rain_primavera#i.year, absorb (cod year) residuals(log_pq_shares_partial)

xtreg log_pq_shares i.year, fe
predict log_pq_shares_resid_2, residuals 


*****************************

**** First using XTREG

eststo clear

eststo: qui xtreg log_pib_tot_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_agro_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_indust_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_serv_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(log_pq_shares)


**** Now with climatic controls and log(area)

eststo clear

eststo: qui xtreg log_pib_tot_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.temp_outono#i.year c.temp_inverno#i.year c.temp_primavera#i.year c.temp_verao#i.year c.rain_verao#i.year c.rain_outono#i.year c.rain_inverno#i.year c.rain_primavera#i.year	i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_agro_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.temp_outono#i.year c.temp_inverno#i.year c.temp_primavera#i.year c.temp_verao#i.year c.rain_verao#i.year c.rain_outono#i.year c.rain_inverno#i.year c.rain_primavera#i.year	i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_indust_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.temp_outono#i.year c.temp_inverno#i.year c.temp_primavera#i.year c.temp_verao#i.year c.rain_verao#i.year c.rain_outono#i.year c.rain_inverno#i.year c.rain_primavera#i.year	i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_serv_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year c.temp_outono#i.year c.temp_inverno#i.year c.temp_primavera#i.year c.temp_verao#i.year c.rain_verao#i.year c.rain_outono#i.year c.rain_inverno#i.year c.rain_primavera#i.year	i.year, fe vce(cluster cod)

esttab, se ar2 keep(log_pq_shares)


**** Now with REGHDFE

eststo clear

eststo: qui reghdfe log_pib_tot_pc log_pq_shares c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_agro_pc log_pq_shares c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_indust_pc log_pq_shares c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, absorb (cod year) vce(cluster cod)

eststo: qui reghdfe log_pib_serv_pc log_pq_shares c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, absorb (cod year) vce(cluster cod)

esttab, se ar2 keep(log_pq_shares)


******** Final


foreach v of varlist log_pib_tot_pc-log_pib_serv_pc {
	eststo clear
	eststo: qui reghdfe `v' log_pq_shares          c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
	eststo: qui reghdfe `v' log_pq_shares_partial  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year, absorb (cod year) vce(cluster cod) 
	esttab, se ar2 keep(log_pq_shares log_pq_shares_partial)
}





