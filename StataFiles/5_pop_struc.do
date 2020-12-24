clear all

set more off,permanently

*set maxvar 50000

*use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\pop_struc_controls.dta"

use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\pop_struc.dta"
*use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\pop_struc_2.dta"


*log using "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\pop_struc.log"


**** Preparing Data



gsort +cod +year

replace P_AGRO = P_AGRO/100
replace P_SERV = P_SERV/100
replace P_COM = P_COM/100
replace P_CONSTR = P_CONSTR/100
replace P_EXTR = P_EXTR/100
replace P_TRANSF = P_TRANSF/100
replace P_INDUST = P_INDUST/100


* controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991
gen val_outpa_1995 = log(val_outpa)

* anothers
gen log_area = log(geo_area_2010)

* Measures *DO NOT PASS LOGS

* Outcomes
gen log_wtrans = log(w_trans)
gen log_wext = log(indust_ex)
gen log_wagro = log(w_agro)
gen log_windust = log(w_indust)


* Interaction
*gen ineq_shares1 = gini_land1 * pq_sum
*gen ineq_shares2 = gini_land2 * pq_sum
*gen ineq_fao1 = gini_land1 * sum_fao
*gen ineq_fao2 = gini_land2 * sum_fao


*** Alternative measures
** First considering the whole sample (both years) to calcualte the distribution

* Top 10%
xtile pct_fao_10 = sum_fao, n(9)
gen faodt10 = 1 if pct_fao_10==9
replace faodt10=0 if faodt10==.


* Top 25%
xtile pct_fao_25 = sum_fao, n(4)
gen faodt25 = 1 if pct_fao_25==4
replace faodt25=0 if faodt25==.

* Bottom 10%
gen faodb10 = 1 if pct_fao_10==1
replace faodb10=0 if faodb10==.

* Bottom 25%
gen faodb25 = 1 if pct_fao_25==1
replace faodb25=0 if faodb25==.

* Above and below the median
egen fao_median = median(sum_fao)
gen faotmed = 1 if sum_fao > fao_median
replace faotmed=0 if faotmed==.
gen faobmed = 1 if sum_fao < fao_median
replace faobmed=0 if faobmed==.


*** Now the same by year
* Top 10%
egen pct_fao_10_y = xtile(sum_fao), by(year) n(9)
gen faodt10y = 1 if pct_fao_10_y==9
replace faodt10y=0 if faodt10y==.

* Top 25%
egen pct_fao_25_y = xtile(sum_fao), by(year) n(4)
gen faodt25y = 1 if pct_fao_25_y==4
replace faodt25y=0 if faodt25y==.

* Bottom 10%
gen faodb10y = 1 if pct_fao_10_y==1
replace faodb10y=0 if faodb10y==.

* Bottom 25%
gen faodb25y = 1 if pct_fao_25_y==1
replace faodb25y=0 if faodb25y==.


* Above and below the median
bysort year : egen fao_median_y = median(sum_fao)
bysort year : gen faotmedy = 1 if sum_fao > fao_median_y
replace faotmedy=0 if faotmedy==.
bysort year : gen faobmedy = 1 if sum_fao < fao_median_y
replace faobmedy=0 if faobmedy==.



******************* Differences ***************************

* Measures
gen dfao  = sum_fao - sum_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dshares  = pq_sum  - pq_sum[_n-1] if year == 2010 & cod == cod[_n-1]
*gen dactual = sum_actual  - sum_actual[_n-1] if year == 2010 & cod == cod[_n-1]
*gen dakm = sum_fao_akm - sum_fao_akm[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaoc95 = sum_faoc95 - sum_faoc95[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaocact = sum_faocact - sum_faocact[_n-1] if year == 2010 & cod == cod[_n-1]


drop if year == 2000 

* Top 10%
xtile pct_dfao_10 = dfaoc95, n(9)
gen dfaodt10 = 1 if pct_dfao_10==9
replace dfaodt10=0 if dfaodt10==.

* Top 25%
xtile pct_dfao_25 = dfaoc95, n(4)
gen dfaodt25 = 1 if pct_dfao_25==4
replace dfaodt25=0 if dfaodt25==.

* Bottom 10%
gen dfaodb10 = 1 if pct_dfao_10==1
replace dfaodb10=0 if dfaodb10==.

* Bottom 25%
gen dfaodb25 = 1 if pct_dfao_25==1
replace dfaodb25=0 if dfaodb25==.

* Above and below the median
egen dfao_median = median(dfaoc95)
gen dfaotmed = 1 if dfaoc95 > dfao_median
replace dfaotmed=0 if dfaotmed==.



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
*gen dineq_shares1 = ineq_shares1 - ineq_shares1[_n-1] if year == 2010 & cod == cod[_n-1]
*gen dineq_shares2 = ineq_shares2 - ineq_shares2[_n-1] if year == 2010 & cod == cod[_n-1]
*gen dineq_fao1 = ineq_fao1 - ineq_fao1[_n-1] if year == 2010 & cod == cod[_n-1]
*gen dineq_fao2 = ineq_fao2 - ineq_fao2[_n-1] if year == 2010 & cod == cod[_n-1]



********************************************************************************

*Summary Statistics

drop if log_windust==.
drop if lat == .
drop if longit ==.

sort  year


by year: summarize P_AGRO P_INDUST P_SERV log_wagro log_windust sum_faoc95


drop if dlog_windust==.
summarize dagro_sh dindust_sh  dserv_sh dlog_wagro dlog_windust dfaoc95

********************************************************************************
* Using only FAO
drop if lat == .
drop if longit ==.
drop if dlog_windust==.


eststo clear
eststo: qui reg dagro_sh dfao, cluster(cod)
eststo: qui reg dindust_sh dfao, cluster(cod)
eststo: qui reg dserv_sh dfao, cluster(cod)
eststo: qui reg dlog_wagro dfao, cluster(cod)
eststo: qui reg dlog_windust dfao, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfao) compress

eststo clear
eststo: qui reg dagro_sh dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dshares) compress



eststo clear
eststo: qui reg dagro_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfao) compress



********************************************************************************
*Standardized
*********************** Alternative Measures ***********************************

egen zfao = std(dfaoc95)

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 val_outpa_1995, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 pc1_high, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 pc1_int, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 pc1_low, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 pc1_int val_outpa_1995, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v'  zfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep( zfao) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dcom_sh dconstr_sh dextr_sh dtransf_sh {
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, beta robust
}




eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit rain_daniel temp_daniel capital_dummy dist_federal dist_state altitude, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit rain_daniel temp_daniel capital_dummy dist_federal dist_state altitude i.codstate, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit rain_daniel temp_daniel capital_dummy dist_federal dist_state altitude i.codmeso, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit rain_daniel temp_daniel capital_dummy dist_federal dist_state altitude i.codmicro, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress




reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit rain_daniel temp_daniel capital_dummy dist_federal dist_state altitude i.codstate, cluster(cod)
*********************** Alternative Measures ***********************************
eststo clear
eststo: qui reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Dummies?? 
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress









eststo clear
eststo: qui reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dindust_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dserv_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dlog_wagro dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dlog_windust dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)

esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
}

esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (200)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (700)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

************************* Dummies **********************************************
xtset cod year, delta(10)


reghdfe P_AGRO sum_faoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, absorb(cod) vce (cluster cod)


reghdfe P_AGRO sum_faoc95 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb(cod) vce (cluster cod)

***********
eststo clear
eststo: qui reg dagro_sh dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfaodt10) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
eststo: qui reg dagro_sh dfaodt25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfaodt25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfaodt25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfaodt25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfaodt25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfaodt25) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
eststo: qui reg dagro_sh dfaotmed log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfaotmed log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfaotmed log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfaotmed log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfaotmed log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfaotmed) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
eststo: qui reg dagro_sh dfaodb10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfaodb10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfaodb10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfaodb10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfaodb10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfaodb10) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
eststo: qui reg dagro_sh dfaodb25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfaodb25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfaodb25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfaodb25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfaodb25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfaodb25) star(* 0.10 ** 0.05 *** 0.01) compress





************
eststo clear
eststo: qui reg dagro_sh dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfaodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfaodt10) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
eststo: qui reg dagro_sh faodt10y log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(faodt10) star(* 0.10 ** 0.05 *** 0.01) compress


* IGUAL!!!!!
eststo clear
foreach v in P_AGRO P_INDUST P_SERV log_wagro log_windust{
eststo: qui reghdfe `v' sum_faoc95 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb(cod year) vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(sum_faoc95) compress


eststo clear
foreach v in P_AGRO P_INDUST P_SERV log_wagro log_windust{
eststo: qui reghdfe `v' faodt10 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb(cod year) vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(faodt10) compress


eststo clear
foreach v in P_AGRO P_INDUST P_SERV log_wagro log_windust{
eststo: qui reghdfe `v' faodt25 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb(cod year) vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(faodt25) compress


eststo clear
foreach v in P_AGRO P_INDUST P_SERV log_wagro log_windust{
eststo: qui reghdfe `v' faotmed c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb(cod year) vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(faotmed) compress

***********



reghdfe P_AGRO sum_faoc95 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb(cod year) vce (cluster cod)

reghdfe P_INDUST sum_faoc95 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb(cod) vce (cluster cod)

reghdfe P_AGRO sum_faoc95 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb(cod) vce (cluster cod)


xtreg P_AGRO sum_faoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, fe cluster(cod)

eststo clear
foreach v in P_AGRO P_INDUST P_SERV log_wagro log_windust{
	eststo: qui xtreg `v' sum_faoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, fe cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(sum_faoc95) compress


eststo clear
foreach v in P_AGRO P_INDUST P_SERV log_wagro log_windust{
	eststo: qui xtreg `v' faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(faodt10) compress



eststo clear
eststo: qui reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dindust_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dserv_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dlog_wagro dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dlog_windust dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)

esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


********************************************************************************

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
}

esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (200)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (700)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
eststo: qui reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dindust_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dserv_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dlog_wagro dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
eststo: qui reg dlog_windust dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)

esttab, se ar2 stat (r2_a N) keep(dfaoc95) compress



********* AKM ******************************************************************

drop pr_barley_1995

local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dagro_sh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dagro_sh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dindust_sh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dindust_sh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dserv_sh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dserv_sh, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dlog_wagro, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlog_wagro, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991
reg_ss dlog_windust, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlog_windust, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


********************************************************************************
eststo clear
eststo: qui reg dagro_sh dfaocact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfaocact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfaocact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfaocact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_windust dfaocact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfaocact) star(*0.10 **0.05 ***0.01) compress

********* Looping **************************************************************

* Baseline with controls
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
	eststo: qui reg `v' dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(dfao) compress

* Baseline clustering at microregion
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
	eststo: qui reg `v' dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmicro)
}
esttab, se ar2 stat (r2_a N) keep(dfao) compress


* Baseline clustering at mesoregion
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
	eststo: qui reg `v' dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codmeso)
}
esttab, se ar2 stat (r2_a N) keep(dfao) compress


* Baseline clustering at state
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
	eststo: qui reg `v' dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(codstate)
}
esttab, se ar2 stat (r2_a N) keep(dfao) compress


*** Alternative Measures **************
* We know must run a xtreg
xtset cod year, delta(10)




* First only baseline controls
eststo clear
eststo clear

eststo: qui xtreg P_AGRO pq_sum, fe vce (cluster cod)
eststo: qui xtreg P_INDUST pq_sum, fe vce (cluster cod)
eststo: qui xtreg P_SERV pq_sum, fe vce (cluster cod)
eststo: qui xtreg log_wagro pq_sum, fe vce (cluster cod)
eststo: qui xtreg log_windust pq_sum, fe vce (cluster cod)

esttab, se ar2 stat (r2_a N) keep(pq_sum) compress



eststo clear
foreach v in P_AGRO P_INDUST P_SERV log_wagro log_windust{
	eststo: qui xtreg `v' faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(faodt10) compress

eststo clear
foreach v in P_AGRO P_INDUST P_SERV log_wagro log_windust{
	eststo: qui xtreg `v' faodt10y log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
}
esttab, se ar2 stat (r2_a N) keep(faodt10y) compress

* Now full controls


* For median they have only the inverted sign
xtreg P_AGRO faodt10, fe vce (cluster cod)
xtreg P_AGRO faodt25, fe vce (cluster cod)
xtreg P_AGRO faotmed, fe vce (cluster cod)



xtreg P_AGRO faodb10, fe vce (cluster cod)
xtreg P_AGRO faodb25, fe vce (cluster cod)
xtreg P_AGRO faobmed, fe vce (cluster cod)



* By year does not seems to work
xtreg P_AGRO faodt10y, fe vce (cluster cod)
xtreg P_AGRO faodt25y, fe vce (cluster cod)
xtreg P_AGRO faotmedy, fe vce (cluster cod)



xtreg P_AGRO faodb10y, fe vce (cluster cod)
xtreg P_AGRO faodb25y, fe vce (cluster cod)
xtreg P_AGRO faobmedy, fe vce (cluster cod)


** Problem with the definition of the treatment?
xtreg P_AGRO faodt10 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)


xtreg P_AGRO faodt10y c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

xtreg P_AGRO faodt10 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.altitude#i.year c.dist_federal#i.year ///
c.log_area#i.year c.lat#i.year c.longit#i.year i.capital_dummy#i.year ///
c.temp_daniel#i.year c.rain_daniel#i.year i.year, fe vce (cluster cod)


xtreg P_AGRO faodt10y c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.altitude#i.year c.dist_federal#i.year ///
c.log_area#i.year c.lat#i.year c.longit#i.year i.capital_dummy#i.year ///
c.temp_daniel#i.year c.rain_daniel#i.year i.year, fe vce (cluster cod)



eststo clear
eststo: qui acreg dagro_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dindust_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dserv_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_wagro dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_windust dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)

esttab, se ar2 stat ( r2_a N) keep(dfao) compress


eststo clear
eststo: qui acreg dagro_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dindust_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dserv_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_wagro dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_windust dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)

esttab, se ar2 stat ( r2_a N) keep(dfao) compress


eststo clear
eststo: qui acreg dagro_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)

eststo: qui acreg dindust_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)

eststo: qui acreg dserv_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)

eststo: qui acreg dlog_wagro dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)

eststo: qui acreg dlog_windust dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)

esttab, se ar2 stat ( r2_a N) keep(dfao) compress


eststo clear
eststo: qui acreg dagro_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dindust_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dserv_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_wagro dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_windust dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (200)

esttab, se ar2 stat ( r2_a N) keep(dfao) compress




eststo clear
eststo: qui acreg dagro_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (300)

eststo: qui acreg dindust_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (300)

eststo: qui acreg dserv_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (300)

eststo: qui acreg dlog_wagro dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (300)

eststo: qui acreg dlog_windust dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (300)

esttab, se ar2 stat ( r2_a N) keep(dfao) compress


********************************************************************************


*** TESTS ************

******Difference between xtreg and reg without differences
xtset cod year, delta(10)
tsset cod year, delta(10)



reg dagro_sh dshares, vce (cluster cod)


reg P_AGRO pq_sum i.year i.cod, vce (cluster cod)

* No Controls ***** Como conseguir o mesmo com reg???
eststo clear

eststo: qui xtreg P_AGRO pq_sum, fe vce (cluster cod)
eststo: qui xtreg P_INDUST pq_sum, fe vce (cluster cod)
eststo: qui xtreg P_SERV pq_sum, fe vce (cluster cod)
eststo: qui xtreg log_wagro pq_sum, fe vce (cluster cod)
eststo: qui xtreg log_windust pq_sum, fe vce (cluster cod)

esttab, se ar2 stat (r2_a N) keep(pq_sum) compress


eststo clear
eststo: qui ivreg2 dagro_sh (dshares= dfao), cluster(cod) ffirst savefirst
eststo: qui ivreg2 dindust_sh (dshares= dfao), cluster(cod) ffirst savefirst
eststo: qui ivreg2 dserv_sh (dshares= dfao), cluster(cod) ffirst savefirst
eststo: qui ivreg2 dlog_wagro (dshares= dfao), cluster(cod) ffirst savefirst
eststo: qui ivreg2 dlog_windust (dshares= dfao), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress


eststo clear
eststo: qui acreg dagro_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dindust_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dserv_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_wagro (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_windust (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress



eststo clear
eststo: qui acreg dagro_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dindust_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dserv_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_wagro (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_windust (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress




eststo clear
eststo: qui acreg dagro_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dindust_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dserv_sh (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_wagro (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_windust (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress


* Missing values
eststo clear
eststo: qui acreg dagro_sh (dshares= dfao), ///
spatial latitude(latitude) longitude (longitude) dist (200)

eststo: qui acreg dindust_sh (dshares= dfao), ///
spatial latitude(latitude) longitude (longitude) dist (200)

eststo: qui acreg dserv_sh (dshares= dfao), ///
spatial latitude(latitude) longitude(longitude) dist (200)

eststo: qui acreg dlog_wagro (dshares= dfao), ///
spatial latitude(latitude) longitude (longitude) dist (200)

eststo: qui acreg dlog_windust (dshares= dfao), ///
spatial latitude(latitude) longitude (longitude) dist (200)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress


********************************************************************************



reg dagro_sh dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)


reg dagro_sh dfao, cluster(cod)
reg dagro_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)


eststo clear
eststo: qui reg dagro_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dindust_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dserv_sh dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg dlog_wagro dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
eststo: qui reg  dlog_windust dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

esttab, se ar2 stat (r2_a N) keep(dfao) compress


* Baseline Controls
eststo clear

eststo: qui xtreg P_AGRO pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg P_INDUST pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg P_SERV pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg log_wagro pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg log_windust pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

esttab, se ar2 stat (r2_a N) keep(pq_sum) compress





eststo clear
eststo: qui ivreg2 dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares= dfao), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares= dfao), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dserv_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares= dfao), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_wagro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares= dfao), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares= dfao), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress


eststo clear
eststo: qui acreg dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dserv_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_wagro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress



eststo clear
eststo: qui acreg dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dserv_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_wagro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress




eststo clear
eststo: qui acreg dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dserv_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_wagro log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_windust log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress









* All Controls
eststo clear

eststo: qui xtreg P_AGRO pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.altitude#i.year c.dist_federal#i.year ///
c.dist_state#i.year i.capital_dummy#i.year c.rain_daniel#i.year c.temp_daniel#i.year ///
c.lat#i.year c.longit#i.year c.log_area#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg P_INDUST pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.altitude#i.year c.dist_federal#i.year ///
c.dist_state#i.year i.capital_dummy#i.year c.rain_daniel#i.year c.temp_daniel#i.year ///
c.lat#i.year c.longit#i.year c.log_area#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg P_SERV pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.altitude#i.year c.dist_federal#i.year ///
c.dist_state#i.year i.capital_dummy#i.year c.rain_daniel#i.year c.temp_daniel#i.year ///
c.lat#i.year c.longit#i.year c.log_area#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg log_wagro pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.altitude#i.year c.dist_federal#i.year ///
c.dist_state#i.year i.capital_dummy#i.year c.rain_daniel#i.year c.temp_daniel#i.year ///
c.lat#i.year c.longit#i.year c.log_area#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg log_windust pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.altitude#i.year c.dist_federal#i.year ///
c.dist_state#i.year i.capital_dummy#i.year c.rain_daniel#i.year c.temp_daniel#i.year ///
c.lat#i.year c.longit#i.year c.log_area#i.year i.year, fe vce (cluster cod)


esttab, se ar2 stat (r2_a N) keep(pq_sum) compress




********* With Land Inequality *************************************************

eststo clear
eststo: qui ivreg2 dagro_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares dineq_shares1= dfao dineq_fao1), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dindust_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares dineq_shares1= dfao dineq_fao1), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dserv_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares dineq_shares1= dfao dineq_fao1), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_wagro dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares dineq_shares1= dfao dineq_fao1), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_windust dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares dineq_shares1 = dfao dineq_fao1), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares1

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares dineq_shares1 dgini_land1) compress


eststo clear
eststo: qui acreg dagro_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dindust_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dserv_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_wagro dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_windust dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (50)

esttab, se ar2 stat ( r2_a N) keep(dshares dineq_shares1) compress



eststo clear
eststo: qui acreg dagro_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dindust_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dserv_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_wagro dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_windust dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (100)

esttab, se ar2 stat ( r2_a N) keep(dshares dineq_shares1) compress




eststo clear
eststo: qui acreg dagro_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dindust_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dserv_sh dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_wagro dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_windust dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares1= dfao dineq_fao1), ///
spatial latitude(lat) longitude (longit) dist (200)

esttab, se ar2 stat ( r2_a N) keep(dshares dineq_shares1) compress




********************************************************************************

reg dagro_sh dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
ivreg2 dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), cluster(cod) ffirst savefirst

ivreg2 dagro_sh (dshares= dfao), cluster(cod) ffirst savefirst

*Are the same when using full panel data?
tsset cod year, delta(10)

ivreg2 dagro_sh (dshares= dfao), cluster(cod) ffirst savefirst
ivreg2 d.P_AGRO log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (d.pq_sum=d.sum_fao), cluster(cod) ffirst savefirst


*egen zsum = std(pq_sum)
*egen zfao = std(sum_fao)
*egen zagsh = std(P_AGRO)

bysort cod: egen zagsh = std(P_AGRO)
bysort cod: egen zfao = std(sum_fao)
bysort cod: egen zsum = std(pq_sum)


sum zsum zfao zagsh

xtivreg2 P_AGRO (pq_sum = sum_fao), fe small first
xtivreg2 zagsh (zsum = zfao), fe small first

xtreg P_AGRO pq_sum, fe vce (cluster cod)
xtreg zagsh zsum, fe vce (cluster cod)

*De fato, são iguais
xtivreg2 P_AGRO (pq_sum = sum_fao), fd small first

ivreg2 d.P_AGRO log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (d.pq_sum=d.sum_fao), cluster(cod) small first


* Standardized
egen zdshares = std(dshares)
egen zdfao = std(dfao)

ivreg2 d.P_AGRO log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (zdshares=zdfao), cluster(cod) small first



****** Using acreg
* net install acreg, from(https://acregstata.weebly.com/uploads/2/9/1/6/29167217) replace

acreg dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

acreg dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), id(cod) time(year) ///
spatial latitude(lat) longitude (longit) dist (100)


acreg dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares=dfao), id(cod) time(year) ///
spatial latitude(lat) longitude (longit) dist (200)


acreg dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), id(cod) time(year) ///
spatial latitude(lat) longitude (longit) dist (300)


acreg dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), id(cod) time(year) ///
spatial latitude(lat) longitude (longit) dist (500)


*********** AKM Correction *****************************************************
local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991

reg_ss dagro_sh, shiftshare_var(dshares) control_varlist(`controls') share_varlist(banana-tea) akmtype(0)

reg_ss dagro_sh, shiftshare_var(dakm) control_varlist(`controls') share_varlist(akm_banana-akm_wheat) akmtype(1)

ivreg_ss dagro_sh, endogenous_var(dshares) shiftshare_iv(dakm) control_varlist(`controls') share_varlist(akm_banana-akm_wheat) akmtype(1)



ivreg2 dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares= dakm), cluster(cod) ffirst savefirst


********************************************************************************


*********************** ssaggregate ********************************************

clear all

set more off,permanently

cd "C:\Users\Andrei\Desktop\ADH\Data"

use location_level

merge 1:1 czone year using Lshares_wide, assert(3) nogen

* Example
ssaggregate y x z l_sh_routine33 [aw=wei], n(aaaa) t(year) s(ind_share) controls("t2 Lsh_manuf")


* Second Step
merge 1:1 sic87dd year using shocks, assert(1 3) nogen
merge m:1 sic87dd using industries, assert(1 3) nogen





* Now trying
clear all

set more off,permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\popstruc_pres.dta"


rename pr_banana pr_1
rename pr_barley pr_2
rename pq_orange pr_3
rename pr_cocoa pr_4
rename pr_coffee pr_5
rename pr_cotton pr_6
rename pr_maize pr_7
rename pr_rice pr_8
rename pr_sorghum pr_9
rename pr_soybean pr_10
rename pr_sugarcane pr_11
rename pr_tea pr_12
rename pr_tobacco pr_13
rename pr_wheat pr_14



* It Works!!!!
ssaggregate P_AGRO pq_sum sum_fao, n(crops) t(year) s(pr_)


controls("log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991")
*************************************************

ivreg_ss dagro_sh, endogenous_var(dshares) shiftshare_iv(dfao) control_varlist(`controls') ///
share_varlist(pr_banana-pr_wheat) akmtype(1) firststage(1)

ivreg_ss dagro_sh, endogenous_var(dshares) shiftshare_iv(dfao) control_varlist(`controls') ///
share_varlist(banana-tea) akmtype(1) firststage(1)

eststo clear
eststo: qui reg dagro_sh dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)

eststo: qui ivreg2 dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares


esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress




eststo clear
eststo: qui ivreg2 dagro_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dactual= dfao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares


esttab, se ar2 stat (widstat r2_a N) keep(dfao dactual) compress


reg dagro_sh dactual log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)





eststo clear
eststo: qui ivreg2 dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares


esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress



eststo clear
eststo: qui ivreg2 dindust_sh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dactual= dfao), cluster(cod) ffirst savefirst


eststo: estimates restore _ivreg2_dshares


esttab, se ar2 stat (widstat r2_a N) keep(dfao dactual) compress












***     ***************

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





