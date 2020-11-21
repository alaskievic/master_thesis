clear all

set more off,permanently


use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\pop_struc_controls.dta"

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\pop_struc.log"


**** Preparing Data

gsort +cod +year


gen log_area = log(geo_area_2010)

gen log_rdpc = log(RDPC)
gen log_rdpct = log(RDPCT)

gen pop_desn1 = pesotot/geo_area_2010
gen pop_desn2 = POP/geo_area_2010
gen log_popd1 = log(pop_desn1)
gen log_popd2 = log(pop_desn2)

gen rur_share = pesoRUR/POP
gen urb_share = pesourb/POP

gen log_fao = log(sum_fao)
gen log_shares = log(pq_shares)


* Differences
gen dlog_rdpc = log_rdpc - log_rdpc[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_rdpct = log_rdpct - log_rdpct[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_popd2 = log_popd2 - log_popd2[_n-1] if year == 2010 & cod == cod[_n-1]
gen drur_share = rur_share - rur_share[_n-1] if year == 2010 & cod == cod[_n-1]
gen durb_share = urb_share - urb_share[_n-1] if year == 2010 & cod == cod[_n-1]

gen dlog_fao  = log_fao - log_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_shares  = log_shares  - log_shares[_n-1] if year == 2010 & cod == cod[_n-1]


gen dagro_sh = P_AGRO - P_AGRO[_n-1] if year == 2010 & cod == cod[_n-1]
gen dserv_sh = P_SERV - P_SERV[_n-1] if year == 2010 & cod == cod[_n-1]
gen dcom_sh = P_COM - P_COM[_n-1] if year == 2010 & cod == cod[_n-1]
gen dconstr_sh = P_CONSTR - P_CONSTR[_n-1] if year == 2010 & cod == cod[_n-1]
gen dextr_sh = P_EXTR - P_EXTR[_n-1] if year == 2010 & cod == cod[_n-1]
gen dtransf_sh = P_TRANSF - P_TRANSF[_n-1] if year == 2010 & cod == cod[_n-1]

gen rural_sh_c = rur_share[_n-2] if year == 2010

gen pop_dens_c = log_popd2[_n-2] if year == 2010

gen rdpc_c = log_rdpc[_n-2] if year == 2010
***** Regressions

***********
eststo clear	
eststo: qui reg dlog_rdpc dlog_fao, robust

eststo: qui reg dlog_rdpc dlog_fao dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

eststo: qui reg dlog_rdpc dlog_shares, robust

eststo: qui reg dlog_rdpc dlog_shares  dist_state dist_federal ///
	log_area  latitude longitude altitude ///
	capital_dummy, robust

esttab, se ar2 keep(dlog_fao dlog_shares) compress

*******
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





