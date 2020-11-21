*** Fao finances

clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\fao_rev.dta"

********** Setting up **********

drop municip_x_x municip2 municip_y municip_y_y municip_x_x_x municip_y_y_y



gsort +cod +year

gen log_pop = log(pesotot)
replace log_pop=0 if log_pop==.

gen log_area = log(geo_area_2010)

gen log_fao = log(sum_fao)
gen log_pqshares = log(pq_shares)
gen log_pquant = log(pq_quant)

*naming problems
rename exp_itr rev_itr
rename trans_estad transf_estad


replace rev_itr = 0 if rev_itr <= 0


*local controls c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   *c.latitude#i.year c.longitude#i.year c.temp_outono#i.year ///
			   *c.temp_inverno#i.year c.temp_primavera#i.year c.temp_verao#i.year ///
			   *c.rain_verao#i.year c.rain_outono#i.year c.rain_inverno#i.year ///
			   *c.rain_primavera#i.year c.capital_dummy#i.year

			   
*local revenues rev_itr rev_impost_tot rev_impost_outros rev_iptu rev_iss rev_taxas rev_tot ///
			   *rev_fpm rev_corrente rev_orcam transf_ipva transf_icms transf_estad
			   

*local expenditures exp_corrente exp_educ exp_func_total exp_habit exp_orcam exp_prev exp_saude ///
				   *exp_seguran exp_transp
				   

foreach v of varlist rev_itr-transf_estad {
	replace `v'=0 if `v'==.
	replace `v' = `v' + 1
	replace `v' = `v'/pesotot
	replace `v' = log(`v')
}



foreach v of varlist exp_corrente-exp_transp {
	replace `v'=0 if `v'==.
	replace `v' = `v' + 1
	replace `v' = `v'/pesotot
	replace `v' = log(`v')

}


*xtset cod year
*são diferentes!!!!
*xtreg log_fao i.year, fe 
*predict xtred1_fao, residuals 

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



*****************************


gen dlog_fao = log_fao - log_fao[_n-10] if year == 2010 & cod == cod[_n-1]
gen dlog_shares = log_pqshares - log_pqshares[_n-10] if year == 2010 & cod == cod[_n-1]
gen drev_itr = rev_itr - rev_itr[_n-10] if year == 2010 & cod == cod[_n-1]




eststo clear	
eststo: qui reg drev_itr dlog_fao, robust

eststo: qui reg drev_itr dlog_fao  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg drev_itr dlog_shares, robust

eststo: qui reg drev_itr dlog_shares  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress


 
eststo clear
eststo: qui ivreg2 drev_itr (dlog_shares = dlog_fao), robust ffirst savefirst 
 
	
eststo: qui ivreg2 drev_itr dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares = dlog_fao), robust ffirst savefirst 


eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep( dlog_shares dlog_fao)


















****** Regressions

*não funciona com locals!!!!!!!!!!

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\fao_fin.log"


foreach v of varlist rev_itr-transf_estad{

eststo clear

eststo: qui reghdfe `v' log_fao c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' log_pqshares c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' dq10red2_fao c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' dq10red2_sh c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' dq25red2_fao c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' dq25red2_sh c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

esttab, se ar2 keep(log_fao log_pqshares dq10red2_fao dq10red2_sh dq25red2_fao dq25red2_sh) compress

}


foreach v of varlist exp_corrente-exp_transp{

eststo clear

eststo clear

eststo: qui reghdfe `v' log_fao c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' log_pqshares c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' dq10red2_fao c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' dq10red2_sh c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' dq25red2_fao c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)
			   
eststo: qui reghdfe `v' dq25red2_sh c.altitude#i.year c.dist_federal#i.year c.dist_state#i.year c.log_area#i.year ///
			   c.latitude#i.year c.longitude#i.year ///
			   c.capital_dummy#i.year, absorb (cod year) vce(cluster cod)

esttab, se ar2 keep(log_fao log_pqshares dq10red2_fao dq10red2_sh dq25red2_fao dq25red2_sh) compress

}

log close 
