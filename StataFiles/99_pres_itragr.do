clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\itr_pres.dta"


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


*Land Inequality
gen dineq_2010 = landgini_2006 - landgini_1995
gen dineq_2015 = landgini_2017 - landgini_1995

* Interactions
gen dineq_shares2010 = ineq_shares2010 - ineq_shares2000
gen dineq_shares2015 = ineq_shares2015 - ineq_shares2000

gen dineq_fao2010 = ineq_fao2010 - ineq_fao2000
gen dineq_fao2015 = ineq_fao2015 - ineq_fao2000

********************************************************************************
*** OLS

** No Land Inequality
eststo clear
foreach v in dummy_op_2010 dummy_conv_op_2010 dummy_op_2015 dummy_conv_op_2015{
    eststo: qui reg `v' dfao2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao2010) compress


eststo clear
foreach v in dummy_op_2010 dummy_conv_op_2010 dummy_op_2015 dummy_conv_op_2015{
    eststo: qui reg `v' dfao2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao2015) compress


*** Probit
eststo clear
foreach v in dummy_op_2010 dummy_conv_op_2010 dummy_op_2015 dummy_conv_op_2015{
    eststo: qui probit `v' dfao2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_p N) keep(dfao2010) compress


eststo clear
foreach v in dummy_op_2010 dummy_conv_op_2010 dummy_op_2015 dummy_conv_op_2015{
    eststo: qui probit `v' dfao2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_p N) keep(dfao2015) compress



*** Logit
eststo clear
foreach v in dummy_op_2010 dummy_conv_op_2010 dummy_op_2015 dummy_conv_op_2015{
    eststo: qui logit `v' dfao2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao2010) compress


eststo clear
foreach v in dummy_op_2010 dummy_conv_op_2010 dummy_op_2015 dummy_conv_op_2015{
    eststo: qui logit `v' dfao2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao2015) compress



******** With Land Inequality ***********

eststo clear
foreach v in dummy_op_2010 dummy_conv_op_2010 dummy_op_2015 dummy_conv_op_2015{
    eststo: qui reg `v' dfao2010 dineq_2010 dineq_fao2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao2010 dineq_2010 dineq_fao2010) compress


eststo clear
foreach v in dummy_op_2010 dummy_conv_op_2010 dummy_op_2015 dummy_conv_op_2015{
    eststo: qui reg `v' dfao2015 dineq_2015 dineq_fao2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao2015 dineq_2015 dineq_fao2015) compress

ivprobit dummy_op_2010 dineq_2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 dineq_shares2010 = dfao2010 dineq_fao2010), twostep first

ivprobit dummy_conv_op_2010 dineq_2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 dineq_shares2010 = dfao2010 dineq_fao2010), twostep first

ivprobit dummy_op_2015 dineq_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2015 dineq_shares2015 = dfao2015 dineq_fao2015), twostep first

ivprobit dummy_conv_op_2015 dineq_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2015 dineq_shares2015 = dfao2015 dineq_fao2015), twostep first


********************************************************************************


ivprobit dummy_op_2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 = dfao2010), twostep first

ivprobit dummy_conv_op_2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 = dfao2010), twostep first

ivprobit dummy_op_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2015 = dfao2015), twostep first

ivprobit dummy_conv_op_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2015 = dfao2015), twostep first


********************************************************************************


******** With Land Inequality **************************************************
ivprobit dummy_op_2010 dineq_2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 dineq_shares2010 = dfao2010 dineq_fao2010), twostep first

ivprobit dummy_conv_op_2010 dineq_2010 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 dineq_shares2010 = dfao2010 dineq_fao2010), twostep first

ivprobit dummy_op_2015 dineq_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2015 dineq_shares2015 = dfao2015 dineq_fao2015), twostep first

ivprobit dummy_conv_op_2015 dineq_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2015 dineq_shares2015 = dfao2015 dineq_fao2015), twostep first


********************************************************************************

* Sign changes?
ivprobit dummy_op_2015 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshare2010 dineq_shares2010 = dfao2010 dineq_fao2010), twostep first