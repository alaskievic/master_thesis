clear all

set more off,permanently

*expenditure
use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\exp_bartik.dta"

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\exp_bartik.log"


gen log_pop = log(pop)
replace log_pop=0 if log_pop==.

replace rev_itr = 0 if rev_itr <= 0

foreach v of varlist rev_itr-exp_transp {
	replace `v' = `v' + 1
	gen log_`v' = log(`v')

}



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

xtreg log_exp_educ dummy_10 altitude dist_federal dist_state geo_area_2010 latitude longitude capital_dummy i.year, fe vce(cluster id)


xtreg log_rev_impost_tot dummy_10  c.dist_state#i.year c.dist_federal#i.year c.geo_area_2010#i.year  c.latitude#i.year c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year i.year, fe vce(cluster cod)



log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\exp_bartik.log"

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



