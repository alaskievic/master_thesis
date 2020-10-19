clear all

set more off,permanently


*use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\exp_bartik.dta"

*use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\exp_bartik.dta"

*log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\exp_bartik.log"

*use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\andrei_data_1.dta"

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\andrei_tmp.dta"


gsort +cod +year

gen log_pop = log(pop)
replace log_pop=0 if log_pop==.

gen pib_pc = municip_pib/pop

gen log_rdpc = log(RDPC)
replace log_rdpc = 0 if log_rdpc==.

gen log_pib_pc = log(pib_pc)
replace log_pib_pc=0 if log_pop==.


replace rev_itr = 0 if rev_itr <= 0

foreach v of varlist rev_itr-exp_transp {
	replace `v' = `v' + 1
	gen log_`v' = log(`v')

}


* generates first differences
** shares
gen dlog_pq_quant = log_pq_quant - log_pq_quant[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pq_shares = log_pq_shares - log_pq_shares[_n-1] if year == 2010 & cod == cod[_n-1]



gen dlog_rdpc = log_rdpc - log_rdpc[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_pc = log_pib_pc - log_pib_pc[_n-1] if year == 2010 & cod == cod[_n-1]


gen dlog_rev_impost_tot = log_rev_impost_tot - log_rev_impost_tot[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_rev_iptu = log_rev_iptu - log_rev_iptu[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_rev_itr = log_rev_itr - log_rev_itr[_n-1] if year == 2010 & cod == cod[_n-1]

gen dlog_exp_func_total = log_exp_func_total - log_exp_func_total[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_exp_educ = log_exp_educ - log_exp_educ[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_exp_habit = log_exp_habit - log_exp_habit[_n-1] if year == 2010 & cod == cod[_n-1]

gen dT_AGUA = T_AGUA - T_AGUA[_n-1] if year == 2010 & cod == cod[_n-1]
gen dT_ANALF18M = T_ANALF18M - T_ANALF18M[_n-1] if year == 2010 & cod == cod[_n-1]
gen dGINI = GINI - GINI[_n-1] if year == 2010 & cod == cod[_n-1]
gen dT_LUZ = T_LUZ - T_LUZ[_n-1] if year == 2010 & cod == cod[_n-1]
gen dgini_land = gini_land - gini_land[_n-1] if year == 2010 & cod == cod[_n-1]


*AMS outcomes
foreach v of varlist estab_total-leitos_sus {
	gen d`v' = `v' - `v'[_n-1] if year == 2010 & cod == cod[_n-1]
}






*********
**dummy se especificar year???????????
xtset cod year

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\andrei_tmp.log"

eststo clear

foreach v of varlist estab_total-leitos_sus {
	eststo clear
	eststo: qui xtreg `v' log_shares_dummy_10 i.year, fe 
	eststo: qui xtreg `v' log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe 
	eststo: qui xtreg `v' log_pq_shares i.year, fe 
	eststo: qui xtreg `v' log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe
	esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)
}

eststo clear


foreach v of varlist estab_total-leitos_sus {
	eststo clear
	eststo: qui xtreg `v' log_shares_dummy_10 i.year, fe 
	eststo: qui xtreg `v' log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe 
	eststo: qui xtreg `v' log_pq_shares i.year, fe 
	eststo: qui xtreg `v' log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe
	esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)
}


foreach v of varlist gini_land-GINI {
	eststo clear
	eststo: qui xtreg `v' log_shares_dummy_10 i.year, fe 
	eststo: qui xtreg `v' log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe 
	eststo: qui xtreg `v' log_pq_shares i.year, fe 
	eststo: qui xtreg `v' log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe
	esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)
}

foreach v of varlist T_AGUA-T_SLUZ {
	eststo clear
	eststo: qui xtreg `v' log_shares_dummy_10 i.year, fe 
	eststo: qui xtreg `v' log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe 
	eststo: qui xtreg `v' log_pq_shares i.year, fe 
	eststo: qui xtreg `v' log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe
	esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)
}


foreach v of varlist log_rdpc-log_exp_transp {
	eststo clear
	eststo: qui xtreg `v' log_shares_dummy_10 i.year, fe 
	eststo: qui xtreg `v' log_shares_dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe 
	eststo: qui xtreg `v' log_pq_shares i.year, fe 
	eststo: qui xtreg `v' log_pq_shares  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe
	esttab, se ar2 keep(log_shares_dummy_10 log_pq_shares)
}



/*
*eststo clear

*log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\andrei_data_1.log

eststo: qui reg dlog_rdpc log_shares_dummy_10
eststo: qui reg dlog_rdpc dlog_pq_quant
eststo: qui reg dlog_rdpc dlog_pq_shares

eststo: qui reg dlog_pib_pc log_shares_dummy_10
eststo: qui reg dlog_pib_pc dlog_pq_quant
eststo: qui reg dlog_pib_pc dlog_pq_shares



esttab, se ar2 compress

eststo clear

eststo: qui reg dlog_rev_impost_tot dummy_10
eststo: qui reg dlog_rev_impost_tot dummy_25
eststo: qui reg dlog_rev_impost_tot dpq_log

eststo: qui reg dlog_rev_iptu dummy_10
eststo: qui reg dlog_rev_iptu dummy_25
eststo: qui reg dlog_rev_iptu dpq_log

esttab, se ar2 compress

eststo clear

eststo: qui reg dlog_rev_itr dummy_10
eststo: qui reg dlog_rev_itr dummy_25
eststo: qui reg dlog_rev_itr dpq_log

esttab, se ar2 compress


*
eststo clear

eststo: qui reg dlog_exp_func_total dummy_10
eststo: qui reg dlog_exp_func_total dummy_25
eststo: qui reg dlog_exp_func_total dpq_log

eststo: qui reg dlog_exp_educ dummy_10
eststo: qui reg dlog_exp_educ dummy_25
eststo: qui reg dlog_exp_educ dpq_log

esttab, se ar2 compress

eststo clear

eststo: qui reg dlog_exp_habit dummy_10
eststo: qui reg dlog_exp_habit dummy_25
eststo: qui reg dlog_exp_habit dpq_log

esttab, se ar2 compress

*
eststo clear

eststo: qui reg dGINI dummy_10
eststo: qui reg dGINI dummy_25
eststo: qui reg dGINI dpq_log

eststo: qui reg dT_LUZ dummy_10
eststo: qui reg dT_LUZ dummy_25
eststo: qui reg dT_LUZ dpq_log

esttab, se ar2 compress

eststo clear

eststo: qui reg dT_ANALF18M dummy_10
eststo: qui reg dT_ANALF18M dummy_25
eststo: qui reg dT_ANALF18M dpq_log

eststo: qui reg dT_AGUA dummy_10
eststo: qui reg dT_AGUA dummy_25
eststo: qui reg dT_AGUA dpq_log

esttab, se ar2 compress

eststo clear
*/


*Summary Statistics

local varlist rev_tot rev_impost_tot rev_iptu rev_iss rev_itr rev_taxas rev_fpm transf_ipva transf_icms transf_estad

* Level
eststo: summarize rev_tot rev_impost_tot rev_iptu rev_iss rev_itr rev_impost_outros rev_taxas rev_fpm transf_ipva transf_icms transf_estad if year==2000
esttab, cells("mean(fmt(2)) sd(fmt(2))") nomtitle nonumber

estpost summarize rev_tot rev_impost_tot rev_iptu rev_iss rev_itr rev_impost_outros rev_taxas rev_fpm transf_ipva transf_icms transf_estad if year==2010
esttab, cells("mean(fmt(2)) sd(fmt(2))") nomtitle nonumber



estpost summarize exp_func_total exp_educ exp_saude exp_habit exp_transp exp_prev exp_seguran if year==2000
esttab, cells("mean(fmt(2)) sd(fmt(2))") nomtitle nonumber


estpost summarize exp_func_total exp_educ exp_saude exp_habit exp_transp exp_prev exp_seguran if year==2010
esttab, cells("mean(fmt(2)) sd(fmt(2))") nomtitle nonumber


* Log
estpost summarize log_rev_tot log_rev_impost_tot log_rev_iptu log_rev_iss log_rev_itr log_rev_impost_outros log_rev_taxas log_rev_fpm log_transf_ipva log_transf_icms log_transf_estad if year==2000
esttab, cells("mean(fmt(2)) sd(fmt(2))") nomtitle nonumber

estpost summarize log_rev_tot log_rev_impost_tot log_rev_iptu log_rev_iss log_rev_itr log_rev_impost_outros log_rev_taxas log_rev_fpm log_transf_ipva log_transf_icms log_transf_estad if year==2010
esttab, cells("mean(fmt(2)) sd(fmt(2))") nomtitle nonumber



estpost summarize log_exp_func_total log_exp_educ log_exp_saude log_exp_habit log_exp_transp log_exp_prev log_exp_seguran if year==2000
esttab, cells("mean(fmt(2)) sd(fmt(2))") nomtitle nonumber


estpost summarize log_exp_func_total log_exp_educ log_exp_saude log_exp_habit log_exp_transp log_exp_prev log_exp_seguran if year==2010
esttab, cells("mean(fmt(2)) sd(fmt(2))") nomtitle nonumber




foreach v of local varlist {
	estpost summarize `v' if year==2000
	esttab, cells("mean sd") nomtitle nonumber
	estpost summarize `v' if year==2010
	esttab, cells("mean sd") nomtitle nonumber
}






xtset cod year

xtreg log_exp_educ dummy_10 altitude dist_federal dist_state geo_area_2010 latitude longitude capital_dummy i.year, fe vce(cluster cod)


xtreg log_rev_impost_tot dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)




eststo clear

eststo: qui xtreg log_rev_impost_tot dummy_10  i.year, fe vce(cluster cod)
eststo: qui xtreg log_rev_impost_tot dummy_25  i.year, fe vce(cluster cod)
eststo: qui xtreg log_rev_impost_tot pq_log  i.year, fe vce(cluster cod)

esttab, se ar2 keep(dummy_10 dummy_25 pq_log)


eststo clear

eststo: qui xtreg log_exp_func_total dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)
eststo: qui xtreg log_exp_func_total dummy_25  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)
eststo: qui xtreg log_exp_func_total pq_log  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(dummy_10 dummy_25 pq_log)


eststo clear

eststo: qui xtreg log_exp_educ dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)
eststo: qui xtreg log_exp_educ dummy_25  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)
eststo: qui xtreg log_exp_educ pq_log  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(dummy_10 dummy_25 pq_log)


eststo clear

eststo: qui xtreg log_rev_itr dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)
eststo: qui xtreg log_rev_itr dummy_25  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)
eststo: qui xtreg log_rev_itr pq_log  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)

esttab, se ar2 keep(dummy_10 dummy_25 pq_log)

log close



