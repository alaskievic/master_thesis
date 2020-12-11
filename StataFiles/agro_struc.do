clear all

set more off,permanently


use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\agrostruc_baseline.dta"

*log using "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\agro_struc_baseline.log"


**** Preparing Data
rename municip_x_x_x_x_x_x municip
rename gini_land_x gini_land
gsort +cod +year



*** Measure differences, then drop
* Difference between 2015-2010 exposure
gen dfao_short  = sum_fao - sum_fao[_n-1] if year == 2017 & cod == cod[_n-1]
gen dshares_short = pq_sum  - pq_sum[_n-1] if year == 2017 & cod == cod[_n-1]

* Difference between 2015-2000 exposure
gen dfao_long  = sum_fao - sum_fao[_n-2] if year == 2017 & cod == cod[_n-1]
gen dshares_long  = pq_sum  - pq_sum[_n-2] if year == 2017 & cod == cod[_n-1]


* Difference between 2010-2000 exposure - BASELINE
gen dfao_baseline  = sum_fao[_n-1] - sum_fao[_n-2] if year == 2017 & cod == cod[_n-1]
gen dshares_baseline  = pq_sum[_n-1]  - pq_sum[_n-2] if year == 2017 & cod == cod[_n-1]



* Interactions
*gen ineq_shares = gini_land * pq_sum
*gen ineq_fao = gini_land * sum_fao

* Land Inequality
gen dgini_land_baseline = gini_land - gini_land[_n-1] if year == 2017 & cod == cod[_n-1]
gen dgini_land_short = gini_land[_n-1] - gini_land[_n-2] if year == 2017 & cod == cod[_n-1]
gen dgini_land_long = gini_land - gini_land[_n-2] if year == 2017 & cod == cod[_n-1]


* Interactions Differences
*gen dineq_shares = ineq_shares - ineq_shares[_n-1] if year == 2017 & cod == cod[_n-1]
*gen dineq_fao = ineq_fao - ineq_fao[_n-1] if year == 2017 & cod == cod[_n-1]

* Now drop
drop if year == 1995

*** Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991

*** Others
gen log_area = log(geo_area_2010)

* Measures *DO NOT PASS LOGS

* Outcomes
gen log_linten = log(l_inten)
gen log_valpw = log(val_outpw)
gen log_landinten = log(((total_area/100)/geo_area_2010))

replace n_maq = 0 if n_maq == .
gen lmaq_inten2 = log(((n_maq + 1)/total_area))

******************* Differences ************************************************

* Outcomes
gen dlog_linten = log_linten - log_linten[_n-1] if year == 2017 & cod == cod[_n-1]
gen dlog_valpw = log_valpw - log_valpw[_n-1] if year == 2017 & cod == cod[_n-1]

gen dlog_landinten = log_landinten - log_landinten[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dadubo = shares_adubo - shares_adubo[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dagrotox = share_agrotox - share_agrotox[_n-1 ] if year == 2017 & cod == cod[_n-1]

gen dtract = ltract_inten - ltract_inten[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dmaq = lmaq_inten - lmaq_inten[_n-1 ] if year == 2017 & cod == cod[_n-1]
gen dlmaq = lmaq_inten2 - lmaq_inten2[_n-1 ] if year == 2017 & cod == cod[_n-1]

********************************************************************************
*** Using only FAO

drop if missing(dlog_valpw)
drop if missing(dlog_linten)
drop if missing(dlog_landinten)

eststo clear
eststo: qui reg dlog_valpw dfao_baseline, vce (cluster cod)
eststo: qui reg dlog_linten dfao_baseline, vce (cluster cod)
eststo: qui reg dlog_landinten dfao_baseline, vce (cluster cod)
eststo: qui reg dadubo dfao_baseline, vce (cluster cod)
eststo: qui reg dagrotox dfao_baseline, vce (cluster cod)
eststo: qui reg dmaq dfao_baseline, vce (cluster cod)

esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress


eststo clear
eststo: qui reg dlog_valpw dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_linten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_landinten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dadubo dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dagrotox dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlmaq dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress



eststo clear
eststo: qui acreg dlog_valpw dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dlog_linten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dlog_landinten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dadubo dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dagrotox dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
eststo: qui acreg dlmaq dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress


eststo clear
eststo: qui acreg dlog_valpw dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dlog_linten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dlog_landinten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dadubo dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dagrotox dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
eststo: qui acreg dlmaq dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress

eststo clear
eststo: qui acreg dlog_valpw dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dlog_linten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dlog_landinten dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dadubo dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dagrotox dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
eststo: qui acreg dlmaq dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress




* Trying some loops

* Ainda não da certo usar macros
*local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991


eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui reg `v' dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress


eststo clear
eststo: qui reg dlog_valpw dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_linten dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_landinten dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dadubo dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dagrotox dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlmaq dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress



eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
  eststo: qui  acreg `v' dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, spatial latitude(lat) longitude (longit) dist (50)
}

esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress


eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui acreg `v' dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
}

esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress

eststo clear
foreach v in dlog_valpw dlog_linten dlog_landinten dadubo dagrotox dlmaq{
    eststo: qui acreg `v' dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
}

esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress




eststo clear
eststo: qui reg dlog_valpw dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_linten dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlog_landinten dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dadubo dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dagrotox dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dlmaq dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
esttab, se ar2 stat ( r2_a N) keep(dfao_short) compress



*** Land Inequality


eststo clear
eststo: qui reg dgini_land_baseline dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dgini_land_short dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dgini_land_long dfao_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
esttab, se ar2 stat ( r2_a N) keep(dfao_baseline) compress

eststo clear
eststo: qui reg dgini_land_baseline dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dgini_land_short dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dgini_land_long dfao_long log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
esttab, se ar2 stat ( r2_a N) keep(dfao_long) compress

eststo clear
eststo: qui reg dgini_land_baseline dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dgini_land_short dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
eststo: qui reg dgini_land_long dfao_short log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
esttab, se ar2 stat ( r2_a N) keep(dfao_short) compress



**** Some Tests

ivreg2 dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_baseline= dfao_baseline), cluster(cod) ffirst savefirst

ivreg2 dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short= dfao_short), cluster(cod) ffirst savefirst

ivreg2 dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_long = dfao_long), cluster(cod) ffirst savefirst

********************************************************************************


xtset cod year, delta(11)


replace n_maq = 0 if n_maq ==.
gen lmaq = log(n_maq + 1)

gen dlmaq= lmaq - lmaq[_n-1] if year == 2017 & cod == cod[_n-1]

replace n_tract = 0 if n_tract ==.
gen ltract = log(n_tract + 1)

gen dtract= ltract - ltract[_n-1] if year == 2017 & cod == cod[_n-1]


* No Controls ***** Como conseguir o mesmo com reg???
eststo clear

eststo: qui xtreg log_valpw pq_sum, fe vce (cluster cod)
eststo: qui xtreg log_linten pq_sum, fe vce (cluster cod)
eststo: qui xtreg lmaq pq_sum, fe vce (cluster cod)

esttab, se ar2 stat (r2_a N) keep(pq_sum) compress



eststo clear
eststo: qui ivreg2 dlog_valpw (dshares= dfao), cluster(cod) ffirst savefirst
eststo: qui ivreg2 dlog_linten (dshares= dfao), cluster(cod) ffirst savefirst
eststo: qui ivreg2 dlmaq (dshares= dfao), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress



drop if lat ==.
drop if longit ==.

eststo clear
eststo: qui acreg dlog_valpw (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_linten (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlmaq (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress



eststo clear
eststo: qui acreg dlog_valpw (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_linten (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlmaq (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress




eststo clear
eststo: qui acreg dlog_valpw (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_linten (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlmaq (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress




* Baseline Controls
eststo clear

eststo: qui xtreg log_valpw pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg log_linten pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg lmaq pq_sum c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)


esttab, se ar2 stat (r2_a N) keep(pq_sum) compress

eststo clear

eststo: qui xtreg log_valpw sum_fao c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg log_linten sum_fao c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)

eststo: qui xtreg lmaq sum_fao c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year i.year, fe vce (cluster cod)


esttab, se ar2 stat (r2_a N) keep(sum_fao) compress


eststo clear
eststo: qui ivreg2 dlog_valpw log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short = dfao_short), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short = dfao_short), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dmaq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short = dfao_short), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dgini_land_baseline log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares_short = dfao_short), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares_short

esttab, se ar2 stat (widstat r2_a N) keep(dfao_short dshares_short) compress





ivreg2 dtract log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares= dfao), cluster(cod) ffirst savefirst

eststo clear
eststo: qui acreg dlog_valpw log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlmaq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (50)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress



eststo clear
eststo: qui acreg dlog_valpw log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlmaq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (100)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress




eststo clear
eststo: qui acreg dlog_valpw log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlmaq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares= dfao), ///
spatial latitude(lat) longitude (longit) dist (200)

esttab, se ar2 stat ( r2_a N) keep(dshares) compress



********* With Land Inequality *************************************************

eststo clear
eststo: qui ivreg2 dlog_valpw dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares dineq_shares= dfao dineq_fao), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlog_linten dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares dineq_shares= dfao dineq_fao), cluster(cod) ffirst savefirst

eststo: qui ivreg2 dlmaq dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
(dshares dineq_shares= dfao dineq_fao), cluster(cod) ffirst savefirst

eststo: estimates restore _ivreg2_dshares
eststo: estimates restore _ivreg2_dineq_shares

esttab, se ar2 stat (widstat r2_a N) keep(dfao dineq_fao dshares dineq_shares dgini_land) compress


eststo clear
eststo: qui acreg dlog_valpw dgini_land1 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlog_linten dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (50)

eststo: qui acreg dlmaq dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (50)


esttab, se ar2 stat ( r2_a N) keep(dshares dineq_shares) compress



eststo clear
eststo: qui acreg dlog_valpw dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlog_linten dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (100)

eststo: qui acreg dlmaq dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (100)


esttab, se ar2 stat ( r2_a N) keep(dshares dineq_shares) compress




eststo clear
eststo: qui acreg dlog_valpw dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlog_linten dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (200)

eststo: qui acreg dlmaq dgini_land log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares dineq_shares= dfao dineq_fao), ///
spatial latitude(lat) longitude (longit) dist (200)


esttab, se ar2 stat ( r2_a N) keep(dshares dineq_shares) compress



**************************** GROWTH ********************************************
eststo clear
eststo: qui reg dlog_valpw dshares, cluster(cod)
eststo: qui reg dlog_valpw dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
esttab, se ar2 keep(dshares) compress



eststo clear
eststo: qui ivreg2 dlog_valpw (dshares  = dfao), cluster(cod) ffirst savefirst
eststo: estimates restore _ivreg2_dshares
eststo: qui ivreg2 dlog_valpw log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares = dfao), cluster(cod) ffirst savefirst
eststo: estimates restore _ivreg2_dshares
esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress



eststo clear
eststo: qui reg dlog_linten dshares, cluster(cod)
eststo: qui reg dlog_linten dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, cluster(cod)
esttab, se ar2 keep(dshares) compress


eststo clear
eststo: qui ivreg2 dlog_linten (dshares = dfao), cluster(cod) ffirst savefirst
eststo: estimates restore _ivreg2_dshares
eststo: qui ivreg2 dlog_linten log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares = dfao), cluster(cod) ffirst savefirst
eststo: estimates restore _ivreg2_dshares
esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress


gen log_maq = log(dn_maq)

eststo clear
eststo: qui reg log_maq dshares if year ==2017, cluster(cod)
eststo: qui reg log_maq dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 if year ==2017, cluster(cod)
esttab, se ar2 keep(dshares) compress



eststo clear
eststo: qui ivreg2 log_maq (dshares = dfao), cluster(cod) ffirst savefirst
eststo: estimates restore _ivreg2_dshares
eststo: qui ivreg2 log_maq log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 (dshares = dfao), cluster(cod) ffirst savefirst
eststo: estimates restore _ivreg2_dshares
esttab, se ar2 stat (widstat r2_a N) keep(dfao dshares) compress


********************************************************************************
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
