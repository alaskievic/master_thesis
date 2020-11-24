clear all

set more off,permanently


use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\agrostruc_pres2.dta"

*log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\agro_struc4.log"


**** Preparing Data
rename municip_x_x_x_x_x_x municip
rename gini_land_x gini_land
drop if year ==1995
gsort +cod +year



* controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991

* anothers
gen log_area = log(geo_area_2010)

* Measures *DO NOT PASS LOGS


* Outcomes
gen log_linten = log(l_inten)
gen log_valpw = log(val_outpw)


gen ineq_shares = gini_land * pq_sum
gen ineq_fao = gini_land * sum_fao


******************* Differences ***************************

* Measures
gen dfao  = sum_fao - sum_fao[_n-1] if year == 2017 & cod == cod[_n-1]
gen dshares  = pq_sum  - pq_sum[_n-1] if year == 2017 & cod == cod[_n-1]

* Outcomes
gen dlog_linten = log_linten - log_linten[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_valpw = log_valpw - log_valpw[_n-1] if year == 2017 & cod == cod[_n-1]

gen dnf_tract = nf_tract - nf_tract[_n-1] if year == 2017 & cod == cod[_n-1]
gen dn_tract = n_tract - n_tract[_n-1] if year == 2017 & cod == cod[_n-1]
gen dn_maq = n_maq - n_maq[_n-1] if year == 2017 & cod == cod[_n-1]



* Land Inequality
gen dgini_land = gini_land - gini_land[_n-1] if year == 2017 & cod == cod[_n-1]


* Interactions
gen dineq_shares = ineq_shares - ineq_shares[_n-1] if year == 2017 & cod == cod[_n-1]
gen dineq_fao = ineq_fao - ineq_fao[_n-1] if year == 2017 & cod == cod[_n-1]




**********************************************************************************************


eststo clear
eststo: qui ivreg2 dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares = dfao dineq_fao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares dineq_fao) compress





eststo clear
eststo: qui ivreg2 dlog_valpw log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares = dfao dineq_fao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares dineq_fao) compress




eststo clear
eststo: qui ivreg2 dnf_tract log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares = dfao dineq_fao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares dineq_fao) compress




eststo clear
eststo: qui ivreg2 dn_tract log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares = dfao dineq_fao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares dineq_fao) compress



eststo clear
eststo: qui ivreg2 dn_maq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares = dfao dineq_fao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares dineq_fao) compress






***********************************************************************************************
eststo clear	
eststo: qui reg dlog_agrival dlog_fao_1, robust

eststo: qui reg dlog_agrival dlog_fao_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_agrival dlog_shares_1, robust

eststo: qui reg dlog_agrival dlog_shares_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao_1 dlog_shares_1) compress


 
eststo clear
eststo: qui ivreg2 dlog_agrival (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 
 
eststo: qui ivreg2 dlog_agrival rural_sh_c pop_dens_c rdpc_c (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_agrival dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 

eststo: qui ivreg2 dlog_agrival rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst  

 
eststo: estimates restore _ivreg2_dlog_shares_2

esttab, se ar2 stat (widstat) keep( dlog_shares_2 dlog_fao_2)
*******************************************************************************************

**********************************************************************************************
eststo clear	
eststo: qui reg dlog_linten dlog_fao_1, robust

eststo: qui reg dlog_linten dlog_fao_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_linten dlog_shares_1, robust

eststo: qui reg dlog_linten dlog_shares_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao_1 dlog_shares_1) compress


 
eststo clear
eststo: qui ivreg2 dlog_linten (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 
 
eststo: qui ivreg2 dlog_linten rural_sh_c pop_dens_c rdpc_c (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_linten dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 

eststo: qui ivreg2 dlog_linten rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst  

 
eststo: estimates restore _ivreg2_dlog_shares_2

esttab, se ar2 stat (widstat) keep( dlog_shares_2 dlog_fao_2)
*******************************************************************************************



**********************************************************************************************
eststo clear	
eststo: qui reg dnf_tract dlog_fao_1, robust

eststo: qui reg dnf_tract dlog_fao_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dnf_tract dlog_shares_1, robust

eststo: qui reg dnf_tract dlog_shares_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao_1 dlog_shares_1) compress


 
eststo clear
eststo: qui ivreg2 dnf_tract (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 
 
eststo: qui ivreg2 dnf_tract rural_sh_c pop_dens_c rdpc_c (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst
	
eststo: qui ivreg2 dnf_tract dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 

eststo: qui ivreg2 dnf_tract rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst  

 
eststo: estimates restore _ivreg2_dlog_shares_2

esttab, se ar2 stat (widstat) keep( dlog_shares_2 dlog_fao_2)
*******************************************************************************************




**********************************************************************************************
eststo clear	
eststo: qui reg dn_tract dlog_fao_1, robust

eststo: qui reg dn_tract dlog_fao_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dn_tract dlog_shares_1, robust

eststo: qui reg dn_tract dlog_shares_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao_1 dlog_shares_1) compress


 
eststo clear
eststo: qui ivreg2 dn_tract (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 
 
eststo: qui ivreg2 dn_tract rural_sh_c pop_dens_c rdpc_c (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst
	
eststo: qui ivreg2 dn_tract dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 

eststo: qui ivreg2 dn_tract rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst  

 
eststo: estimates restore _ivreg2_dlog_shares_2

esttab, se ar2 stat (widstat) keep( dlog_shares_2 dlog_fao_2)
*******************************************************************************************


**********************************************************************************************
eststo clear	
eststo: qui reg dn_maq  dlog_fao_1, robust

eststo: qui reg dn_maq  dlog_fao_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dn_maq  dlog_shares_1, robust

eststo: qui reg dn_maq  dlog_shares_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao_1 dlog_shares_1) compress


 
eststo clear
eststo: qui ivreg2 dn_maq  (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 
 
eststo: qui ivreg2 dn_maq  rural_sh_c pop_dens_c rdpc_c (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst
	
eststo: qui ivreg2 dn_maq  dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 

eststo: qui ivreg2 dn_maq rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst  

 
eststo: estimates restore _ivreg2_dlog_shares_2

esttab, se ar2 stat (widstat) keep( dlog_shares_2 dlog_fao_2)
*******************************************************************************************


log close
