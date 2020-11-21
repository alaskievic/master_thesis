clear all

set more off,permanently


use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\agro_struc3.dta"

*log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\agro_struc.log"


**** Preparing Data

drop year_y
rename year_x year
drop pop_dens
drop pesotot_y
rename pesotot_x pesotot
gsort +cod +year

gen pop_dens = pesotot/geo_area_2010

gen log_area = log(geo_area_2010)
gen log_linten = log(l_inten)
gen log_dens = log(pop_dens)
gen log_agrival = log(val_outpw)

gen log_rdpc = log(RDPC)
gen rur_share = pesoRUR/POP

gen log_fao = log(sum_fao)
gen log_shares = log(pq_shares)


gen rural_sh_c = rur_share[_n-2] if year == 2017

gen pop_dens_c = log_dens[_n-2] if year == 2017

gen rdpc_c = log_rdpc[_n-2] if year == 2017

*xtset cod year

* Generating differences
gen dlog_linten = log_linten - log_linten[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_dens = log_dens - log_dens[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_agrival = log_agrival - log_agrival[_n-1] if year == 2017 & cod == cod[_n-1]


gen dnf_tract = nf_tract - nf_tract[_n-1] if year == 2017 & cod == cod[_n-1]
gen dn_tract = n_tract - n_tract[_n-1] if year == 2017 & cod == cod[_n-1]
gen dn_maq = n_maq - n_maq[_n-1] if year == 2017 & cod == cod[_n-1]


/* Generating first differences: attention 1995(census) == 2000(bartik)
2010 (bartik) == 2006 (census)
2015 (bartik) == 2017 (census)
*/

* _1 == 2006 (2010 in bartik) - 1995 (2000 in bartik) in measures
gen dlog_fao_1 = log_fao[_n-1] - log_fao[_n-2] if year == 2017 & cod == cod[_n-1]
gen dlog_shares_1 = log_shares[_n-1] - log_shares[_n-2] if year == 2017 & cod == cod[_n-1]

* _1 == 2017(2015 in bartik) - 2006 (2010 in bartik) in measures
gen dlog_fao_2 = log_fao - log_fao[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_shares_2 = log_shares - log_shares[_n-1] if year == 2017 & cod == cod[_n-1]



* First with simple reg

******

eststo clear	
eststo: qui reg dlog_dens dlog_fao_1, robust

eststo: qui reg dlog_dens dlog_fao_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_dens dlog_shares_1, robust

eststo: qui reg dlog_dens dlog_shares_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao_1 dlog_shares_1) compress
	
	
*************



*************

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



*************

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

	
	
	
*************

eststo clear	
eststo: qui reg dn_maq dlog_fao_1, robust

eststo: qui reg dn_maq dlog_fao_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dn_maq dlog_shares_1, robust

eststo: qui reg dn_maq dlog_shares_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao_1 dlog_shares_1) compress

	
rural_sh_c pop_dens_c rdpc_c
**************** Now trying with ivreg2
eststo clear
eststo: qui reg dlog_linten dlog_fao_1, robust


eststo: qui reg dlog_linten dlog_fao_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_linten dlog_shares_1  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao_1 dlog_shares_1) compress


eststo clear

eststo: qui ivreg2 dlog_linten (dlog_shares_1 = dlog_fao_1), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_linten dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares_1 = dlog_fao_1), robust ffirst savefirst
 
 
eststo: qui ivreg2 dlog_linten (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_linten dist_state dist_federal latitude ///
 longitude altitude capital_dummy  (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst

eststo: estimates restore _ivreg2_dlog_shares_1
eststo: estimates restore _ivreg2_dlog_shares_2

esttab, se ar2 stat (widstat) keep(dlog_shares_1 dlog_shares_2 dlog_fao_1 dlog_fao_2) compress


eststo clear

eststo: qui ivreg2 dlog_dens (dlog_shares_1 = dlog_fao_1), robust ffirst
	
eststo: qui ivreg2 dlog_dens dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_1 = dlog_fao_1), robust ffirst
 
eststo: estimates restore _ivreg2_dlog_shares_1
 
esttab, se ar2 stat (widstat) keep(dlog_shares_1 dlog_fao_1) compress
 

 
 
 
 
******************************************
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

*****************************************************************************
eststo clear

eststo: qui ivreg2 dlog_agrival (dlog_shares_1 = dlog_fao_1), robust ffirst savefirst

eststo: qui ivreg2 dlog_agrival rural_sh_c pop_dens_c rdpc_c (dlog_shares_1 = dlog_fao_1), robust ffirst savefirst
	
	
eststo: qui ivreg2 dlog_agrival dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_1 = dlog_fao_1), robust ffirst savefirst

eststo: qui ivreg2 dlog_agrival rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_1 = dlog_fao_1), robust ffirst savefirst
eststo: estimates restore _ivreg2_dlog_shares_1
esttab, se ar2 stat (widstat) keep(dlog_shares_1 dlog_fao_1)
 
 
eststo clear
eststo: qui ivreg2 dlog_agrival (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 
 
eststo: qui ivreg2 dlog_agrival rural_sh_c pop_dens_c rdpc_c (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst
	
eststo: qui ivreg2 dlog_agrival dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst 

eststo: qui ivreg2 dlog_agrival rural_sh_c pop_dens_c rdpc_c dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_2 = dlog_fao_2), robust ffirst savefirst  

 
eststo: estimates restore _ivreg2_dlog_shares_2

esttab, se ar2 stat (widstat) keep( dlog_shares_2 dlog_fao_2)
******************************************************************* 

 

eststo clear

eststo: qui ivreg2 dnf_tract (dlog_shares_1 = dlog_fao_1), robust ffirst
	
eststo: qui ivreg2 dnf_tract dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_1 = dlog_fao_1), robust ffirst
 
eststo: estimates restore _ivreg2_dlog_shares_1
 
esttab, se ar2 stat (widstat) keep(dlog_shares_1 dlog_fao_1) compress
 
 
eststo clear

eststo: qui ivreg2 dn_tract (dlog_shares_1 = dlog_fao_1), robust ffirst
	
eststo: qui ivreg2 dn_tract dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_1 = dlog_fao_1), robust ffirst

eststo: estimates restore _ivreg2_dlog_shares_1
 
esttab, se ar2 stat (widstat) keep(dlog_shares_1 dlog_fao_1) compress
 
 
eststo clear
eststo: qui ivreg2 dn_maq (dlog_shares_1 = dlog_fao_1), robust savefirst

eststo: qui ivreg2 dn_maq dist_state dist_federal latitude log_area ///
 longitude altitude capital_dummy (dlog_shares_1 = dlog_fao_1), robust savefirst

 
eststo: estimates restore _ivreg2_dlog_shares_1
 
esttab, se ar2 stat (widstat) keep(dlog_shares_1 dlog_fao_1) compress
