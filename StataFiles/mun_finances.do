clear all

set more off,permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\exp_bartik.dta"

********** Setting up **********

gsort +cod +year

gen log_pop = log(pop)
replace log_pop=0 if log_pop==.

gen pib_pc = municip_pib/pop

gen log_pib_pc = log(pib_pc)
replace log_pib_pc=0 if log_pop==.


replace rev_itr = 0 if rev_itr <= 0

foreach v of varlist rev_itr-exp_transp {
	replace `v' = `v' + 1
	gen log_`v' = log(`v')

}



*****************************



********* Panel regression for municipalities finances using the whole 2000-2010 period *********************


******* especificar year quando coloca
xtset cod year


log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\mun_finances.log

eststo clear


***** Log Pib per Capita in 2010 values
eststo: qui xtreg log_pib_pc log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_pc log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_pc shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_pc log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_pc pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_pib_pc log_shares_dummy_25  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)


esttab, se ar2 keep(log_shares_dummy_10 log_shares_dummy_10 shares_dummy_10 log_pq_shares pq_shares log_shares_dummy_25)



***** Total Revenues
eststo clear

eststo: qui xtreg log_rev_tot log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_tot log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_tot log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_tot pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_tot log_shares_dummy_25 c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_tot log_quant_dummy_10 c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_tot log_pq_quant c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_tot pq_quant c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)


esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares pq_shares log_shares_dummy_25 log_quant_dummy_10 log_pq_quant pq_quant)


***** Total Taxes Revenues
eststo clear

eststo: qui xtreg log_rev_impost_tot log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_impost_tot log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_impost_tot log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_impost_tot pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_impost_tot log_shares_dummy_25 c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_impost_tot log_quant_dummy_10 c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_impost_tot log_pq_quant c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_impost_tot pq_quant c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)


esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares pq_shares log_shares_dummy_25 log_quant_dummy_10 log_pq_quant pq_quant)



***** Property Taxes
eststo clear

eststo: qui xtreg log_rev_iptu log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_iptu log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_iptu log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)


esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)



***** Services Taxes
eststo clear

eststo: qui xtreg log_rev_iss log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_iss log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_iss log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)


esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)

***** Rural Property Taxes
eststo clear

eststo: qui xtreg log_rev_itr log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_itr log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_itr log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_itr log_quant_dummy_10 c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_itr log_pq_quant c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)


esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares log_quant_dummy_10 log_pq_quant)


***** Transfers
eststo clear
eststo: qui xtreg log_rev_fpm log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_fpm log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_rev_fpm log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)


eststo clear
eststo: qui xtreg log_transf_icms log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_transf_icms log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_transf_icms log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)


eststo clear
eststo: qui xtreg log_transf_ipva log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_transf_ipva log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_transf_ipva log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)


eststo clear
eststo: qui xtreg log_transf_estad log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_transf_estad log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_transf_estad log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)


***** Total functional Expenditures
eststo clear

eststo: qui xtreg log_exp_func_total log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_func_total log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_func_total shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_func_total log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_func_total pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares shares_dummy_10 log_pq_shares pq_shares)


***** Education Expenditures with shares measure
eststo clear


eststo: qui xtreg log_exp_educ log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_educ log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_educ shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_educ log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_educ pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares shares_dummy_10 pq_shares)

***** Health Expenditures with shares measure
eststo clear

eststo: qui xtreg log_exp_educ log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_saude log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_saude shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_saude log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_saude pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)


esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares shares_dummy_10 pq_shares)


***** Housing Expenditures with shares measure
eststo clear

eststo: qui xtreg log_exp_habit log_shares_dummy_10 i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_habit log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_habit shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_habit log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

eststo: qui xtreg log_exp_habit pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)


esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares shares_dummy_10 pq_shares)




