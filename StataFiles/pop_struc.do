clear all

set more off,permanently


*use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\pop_struc_controls.dta"

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\popstruc_pres.dta"


*log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\pop_struc.log"


**** Preparing Data

rename municip_x_x_x_x_x_x municip
rename gini_land1 gini_land2
rename gini_land1_x gini_land1


gsort +cod +year

* controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991

* anothers
gen log_area = log(geo_area_2010)

* Measures *DO NOT PASS LOGS

* Outcomes
gen log_wtrans = log(w_trans)
gen log_wext = log(indust_ex)
gen log_wagro = log(w_agro)
gen log_windust = log(w_indust)


* Interaction
gen ineq_shares1 = gini_land1 * pq_sum
gen ineq_shares2 = gini_land2 * pq_sum
gen ineq_fao1 = gini_land1 * sum_fao
gen ineq_fao2 = gini_land2 * sum_fao


******************* Differences ***************************

* Measures
gen dfao  = sum_fao - sum_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dshares  = pq_sum  - pq_sum[_n-1] if year == 2010 & cod == cod[_n-1]

* Shares
gen dagro_sh = P_AGRO - P_AGRO[_n-1] if year == 2010 & cod == cod[_n-1]
gen dserv_sh = P_SERV - P_SERV[_n-1] if year == 2010 & cod == cod[_n-1]
gen dcom_sh = P_COM - P_COM[_n-1] if year == 2010 & cod == cod[_n-1]
gen dconstr_sh = P_CONSTR - P_CONSTR[_n-1] if year == 2010 & cod == cod[_n-1]
gen dextr_sh = P_EXTR - P_EXTR[_n-1] if year == 2010 & cod == cod[_n-1]
gen dtransf_sh = P_TRANSF - P_TRANSF[_n-1] if year == 2010 & cod == cod[_n-1]
gen dindust_sh = P_INDUST - P_INDUST[_n-1] if year == 2010 & cod == cod[_n-1]

* Wages
gen dlog_wagro = log_wagro- log_wagro[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_windust = log_windust- log_windust[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_wtrans = log_wtrans- log_wtrans[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_wext = log_wext- log_wext[_n-1] if year == 2010 & cod == cod[_n-1]

* Land Inequality
gen dgini_land1 = gini_land1 - gini_land1[_n-1] if year == 2010 & cod == cod[_n-1]
gen dgini_land2 = gini_land2 - gini_land2[_n-1] if year == 2010 & cod == cod[_n-1]


* Interactions
gen dineq_shares1 = ineq_shares1 - ineq_shares1[_n-1] if year == 2010 & cod == cod[_n-1]
gen dineq_shares2 = ineq_shares2 - ineq_shares2[_n-1] if year == 2010 & cod == cod[_n-1]
gen dineq_fao1 = ineq_fao1 - ineq_fao1[_n-1] if year == 2010 & cod == cod[_n-1]
gen dineq_fao2 = ineq_fao2 - ineq_fao2[_n-1] if year == 2010 & cod == cod[_n-1]




*********** Regressions *******************************************************
eststo clear
eststo: qui ivreg2 dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress





eststo clear
eststo: qui ivreg2 dlog_wagro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress




eststo clear
eststo: qui ivreg2 dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress




eststo clear
eststo: qui ivreg2 dtransf_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress



eststo clear
eststo: qui ivreg2 dextr_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress





eststo clear
eststo: qui ivreg2 dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress



eststo clear
eststo: qui ivreg2 dlog_wtrans log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress



eststo clear
eststo: qui ivreg2 dlog_wext log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dineq_fao1) compress




ivreg2 dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

ivreg2 dlog_windust dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


ivreg2 dagro_sh  log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst
ivreg2 dagro_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst



***********
	
reg dagro_sh dshares dineq_shares1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)


ivreg2 dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares = dfao), cluster(cod) ffirst savefirst

ivreg2 dagro_sh (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

ivreg2 dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

*eststo: estimates restore _ivreg2_dlog_shares_dineq_shares1

*esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares dineq_shares1 dlog_fao dineq_fao1) compress


***************************
reg dindust_sh dshares dineq_shares1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

ivreg2 dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares = dfao), cluster(cod) ffirst savefirst

ivreg2 dindust_sh (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

ivreg2 dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst
*****************************

reg dlog_wagro dshares dineq_shares1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

ivreg2 dlog_wagro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares = dfao), cluster(cod) ffirst savefirst

ivreg2 dlog_wagro (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

ivreg2 dlog_wagro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst




reg dlog_windust dshares dineq_shares1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

ivreg2 dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares = dfao), cluster(cod) ffirst savefirst

ivreg2 dlog_windust (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

ivreg2 dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


reg dlog_wtrans dshares dineq_shares1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

ivreg2 dlog_wtrans log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.uf (dshares = dfao), cluster(cod) ffirst savefirst

ivreg2 dlog_wtrans (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst


**** TEST
eststo clear

eststo: qui reg dlog_windust dshares dineq_shares1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

eststo: qui ivreg2 dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares = dfao), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_windust (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat) keep(dfao dshares dineq_shares1 dineq_fao1) compress
*******


eststo clear
eststo: qui ivreg2 dlog_wtrans log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.cod (dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares_dineq_shares1

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares dineq_shares1 dlog_fao dineq_fao1) compress

*****************************
eststo clear

eststo: qui ivreg2 dlog_rdpc (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dlog_rdpc rural_sh_c pop_dens_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dlog_rdpc dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dlog_rdpc  rural_sh_c pop_dens_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress




******************************************************************
eststo clear	
eststo: qui reg dlog_rdpct dlog_fao, robust

eststo: qui reg dlog_rdpct dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_rdpct dlog_shares, robust

eststo: qui reg dlog_rdpct dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 dlog_rdpct (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dlog_rdpct rural_sh_c pop_dens_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dlog_rdpct dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dlog_rdpct  rural_sh_c pop_dens_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress





******************************************************************
eststo clear	
eststo: qui reg dlog_popd2 dlog_fao, robust

eststo: qui reg dlog_popd2 dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_popd2 dlog_shares, robust

eststo: qui reg dlog_popd2 dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 dlog_popd2 (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dlog_popd2 rural_sh_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dlog_popd2 dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dlog_popd2  rural_sh_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress


















******************************************************************
eststo clear	
eststo: qui reg drur_share dlog_fao, robust

eststo: qui reg drur_share dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg drur_share dlog_shares, robust

eststo: qui reg drur_share dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 drur_share (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 drur_share pop_dens_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 drur_share dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 drur_share pop_dens_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress












******************************************************************
eststo clear	
eststo: qui reg durb_share dlog_fao, robust

eststo: qui reg durb_share dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg durb_share dlog_shares, robust

eststo: qui reg durb_share dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 durb_share (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 durb_share pop_dens_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 durb_share dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 durb_share pop_dens_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress









* Agro share
******************************************************************
eststo clear	
eststo: qui reg dagro_sh dlog_fao, robust

eststo: qui reg dagro_sh dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dagro_sh dlog_shares, robust

eststo: qui reg dagro_sh dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 dagro_sh (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dagro_sh rural_sh_c pop_dens_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dagro_sh dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dagro_sh  rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress








******************************************************************
eststo clear	
eststo: qui reg dserv_sh dlog_fao, robust

eststo: qui reg dserv_sh dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dserv_sh dlog_shares, robust

eststo: qui reg dserv_sh dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 dserv_sh (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dserv_sh rural_sh_c pop_dens_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dserv_sh dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dserv_sh  rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress










******************************************************************
eststo clear	
eststo: qui reg dcom_sh dlog_fao, robust

eststo: qui reg dcom_sh dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dcom_sh dlog_shares, robust

eststo: qui reg dcom_sh dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 dcom_sh (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dcom_sh rural_sh_c pop_dens_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dcom_sh dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dcom_sh  rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress









******************************************************************
eststo clear	
eststo: qui reg dconstr_sh dlog_fao, robust

eststo: qui reg dconstr_sh dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dconstr_sh dlog_shares, robust

eststo: qui reg dconstr_sh dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 dconstr_sh (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dconstr_sh rural_sh_c pop_dens_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dconstr_sh dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dconstr_sh  rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress









******************************************************************
eststo clear	
eststo: qui reg dextr_sh dlog_fao, robust

eststo: qui reg dextr_sh dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dextr_sh dlog_shares, robust

eststo: qui reg dextr_sh dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 dextr_sh (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dextr_sh rural_sh_c pop_dens_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dextr_sh dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dextr_sh  rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress






******************************************************************
eststo clear	
eststo: qui reg dtransf_sh dlog_fao, robust

eststo: qui reg dtransf_sh dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dtransf_sh dlog_shares, robust

eststo: qui reg dtransf_sh dlog_shares dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
eststo clear

eststo: qui ivreg2 dtransf_sh (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dtransf_sh rural_sh_c pop_dens_c rdpc_c  (dlog_shares = dlog_fao), robust ffirst savefirst


eststo: qui ivreg2 dtransf_sh dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst
 
eststo: qui ivreg2 dtransf_sh  rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares = dlog_fao), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares

esttab, se ar2 stat (widstat) keep(dlog_fao dlog_shares) compress



log close





