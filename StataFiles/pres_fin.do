*** GDP
clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\pres_fin.dta"

********** Setting up **********
drop municip_y_y_y_y_y_y
keep if year==2000 | year==2010
gsort +cod +year

drop gini_land1_y

rename gini_land1 gini_land2
rename gini_land1_x gini_land1

rename exp_itr rev_itr
rename trans_estad transf_estad

replace rev_itr = 0 if rev_itr <= 0



* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991

* Others
gen log_area = log(geo_area_2010)
gen log_pop = log(pesotot)



* Interaction
gen ineq_shares1 = gini_land1 * pq_sum
gen ineq_shares2 = gini_land2 * pq_sum
gen ineq_fao1 = gini_land1 * sum_fao
gen ineq_fao2 = gini_land2 * sum_fao
				   
* Outcomes
foreach v of varlist rev_itr-transf_estad {
	replace `v'=0 if `v'==.
	replace `v' = `v' + 1
	replace `v' = `v'/pesotot
	replace `v' = log(`v')
	gen d`v' = `v' - `v'[_n-1] if year == 2010 & cod == cod[_n-1]
}



foreach v of varlist exp_corrente-exp_transp {
	replace `v'=0 if `v'==.
	replace `v' = `v' + 1
	replace `v' = `v'/pesotot
	replace `v' = log(`v')
	gen d`v' = `v' - `v'[_n-1] if year == 2010 & cod == cod[_n-1]
}


******************* Differences ***************************

* Measures
gen dfao  = sum_fao - sum_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dshares  = pq_sum  - pq_sum[_n-1] if year == 2010 & cod == cod[_n-1]


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
eststo: qui ivreg2 drev_itr log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress




eststo clear
eststo: qui ivreg2 drev_tot log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress



eststo clear
eststo: qui ivreg2 drev_iptu log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress



eststo clear
eststo: qui ivreg2 drev_iss log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress






eststo clear
eststo: qui ivreg2 dtransf_icms log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress







eststo clear
eststo: qui ivreg2 dtransf_ipva log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress






eststo clear
eststo: qui ivreg2 dtransf_estad log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress

********************************************************************************
eststo clear
eststo: qui ivreg2 dexp_func_total log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress




eststo clear
eststo: qui ivreg2 dexp_educ log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress



eststo clear
eststo: qui ivreg2 dexp_saude log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress



eststo clear
eststo: qui ivreg2 dexp_habit log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress


********************************************************************************
