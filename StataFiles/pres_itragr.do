clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\itr_pres.dta"


********** Setting up **********
gsort +cod +year



* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991

* Others
gen log_area = log(geo_area_2010)
gen log_pop = log(pesotot)


* Interactions
gen ineq_shares2000 = landgini_1995 * pq_sum_2000
gen ineq_shares2010 = landgini_2006 * pq_sum_2010
gen ineq_shares2015 = landgini_2017 * pq_sum_2015

gen ineq_fao2000 = landgini_1995 * sum_fao_2000
gen ineq_fao2010 = landgini_2006 * sum_fao_2010
gen ineq_fao2015 = landgini_2017 * sum_fao_2015


***********  Differences  ************************

* Measures
gen dshare2010  = pq_sum_2010 - pq_sum_2000
gen dshare2015  = pq_sum_2015 - pq_sum_2000

gen dfao2010  = sum_fao_2010 - sum_fao_2000
gen dfao2015  = sum_fao_2015 - sum_fao_2000


* Interactions
gen dineq_shares2010 = ineq_shares2010 - ineq_shares2000
gen dineq_shares2015 = ineq_shares2015 - ineq_shares2000

gen dineq_fao2010 = ineq_fao2010 - ineq_fao2000
gen dineq_fao2015 = ineq_fao2015 - ineq_fao2000

********************************************************************************
reg dummy_op_2010  log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster cod

ivprobit dummy_op_2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 dineq_shares2010 = dfao2010 dineq_fao2010), twostep first

ivprobit dummy_conv_op_2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 dineq_shares2010 = dfao2010 dineq_fao2010), twostep first


ivprobit dummy_op_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2015 dineq_shares2015 = dfao2015 dineq_fao2015), twostep first

ivprobit dummy_conv_op_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2015 dineq_shares2015 = dfao2015 dineq_fao2015), twostep first




********************************************************************************





********************************************************************************

