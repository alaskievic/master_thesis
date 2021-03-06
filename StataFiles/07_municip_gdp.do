
***** Municipal GDPs ***********************************************************
clear all

set more off, permanently

cd "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles"
use "./municip_gdp.dta"

***** Setting up ***************************************************************
gsort +cod +year

* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991


* Outcomes
foreach v of varlist pib_tot-pib_serv {
	gen `v'_pc = `v'/pesotot
	gen log_`v' = log(`v'_pc)
}



* Others
gen log_area = log(geo_area_2010)
gen log_pop = log(pesotot)



******************* Differences ***************************

keep if year==2000 | year==2010

*Standardized
*egen zfaoc95 = std(sum_fao_cattle_1995)

* Measures
gen dfaoc95  = sum_fao_cattle_1995 - sum_fao_cattle_1995[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaocact  = sum_fao_cattle_actual - sum_fao_cattle_actual[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfao  = sum_fao - sum_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dshares  = pq_sum  - pq_sum[_n-1] if year == 2010 & cod == cod[_n-1]

* GDP
gen dlog_pib_tot = log_pib_tot - log_pib_tot[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_agro = log_pib_agro - log_pib_agro[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_indust = log_pib_indust - log_pib_indust[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_pib_serv = log_pib_serv - log_pib_serv[_n-1] if year == 2010 & cod == cod[_n-1]




***************************** Summary Statistics *******************************
drop if missing(log_income_1991)
drop if missing(log_popdens_1991)
drop if missing(agr_sh_1991)
drop if missing(analf_1991)
drop if missing(temp_daniel)
drop if missing(rain_daniel)
drop if missing(lat) 
drop if missing(longit)
drop if missing(log_pib_agro)


by cod (year), sort: keep if _N == 2 & year[1] == 2000 & year[_N] == 2010

sort  year
by year: summarize sum_fao_cattle_1995


by year: summarize log_pib_tot log_pib_agro log_pib_indust log_pib_serv

summarize dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv









***** First Differences Regressions ********************************************

drop if year == 2000

* Standardized
egen zdfaoc95 = std(dfaoc95)


gen codreg = 1 if codstate < 20
replace codreg = 2 if codstate < 30 & codstate >= 20
replace codreg = 3 if codstate < 40 & codstate >= 30
replace codreg = 4 if codstate < 50 & codstate >= 40
replace codreg = 5 if codstate >= 50


*** No Controls ***

eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 agr_sh_1991, vce (cluster cod)
}



foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
}

eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress






********** TESTE ***************


eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95, vce (cluster cod)
}



eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 agr_sh_1991, vce (cluster cod)
}



foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
}


foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}




********************************************************************************
eststo clear
eststo: qui reg dlog_pib_tot dfaoc95, vce (cluster cod)
eststo: qui reg dlog_pib_tot dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlog_pib_tot dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlog_pib_tot dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/gdp_a.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect of the Commodity Shock on Sectoral GDP) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\begin{table}[h]" "\centering" "\begin{adjustbox}{max width=\textwidth}" "\begin{threeparttable}" "\caption{@title}" "\label{tab::gdp}" "\begin{tabular}{l* {5}S[table-format = 1.6]}" ///
"\hline \hline" "\noalign{\vskip 0.2cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel A.} & \multicolumn{4}{c}{$\Delta$ Log Total GDP Per Capita}\\" "\noalign{\vskip 0.1cm}")



eststo clear
eststo: qui reg dlog_pib_agro dfaoc95, vce (cluster cod)
eststo: qui reg dlog_pib_agro dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlog_pib_agro dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlog_pib_agro dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/gdp_b.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel B.} & \multicolumn{4}{c}{$\Delta$ Log Agricultural GDP Per Capita}\\" "\noalign{\vskip 0.1cm}") ///



eststo clear
eststo: qui reg dlog_pib_indust dfaoc95, vce (cluster cod)
eststo: qui reg dlog_pib_indust dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlog_pib_indust dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlog_pib_indust dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/gdp_c.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel C.} & \multicolumn{4}{c}{$\Delta$  Log Manufacturing GDP Per Capita}\\" "\noalign{\vskip 0.1cm}") ///


eststo clear
eststo: qui reg dlog_pib_serv dfaoc95, vce (cluster cod)
eststo: qui reg dlog_pib_serv dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlog_pib_serv dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlog_pib_serv dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/gdp_d.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect of the Commodity Shock on Sectoral GDP) collabels(none) eqlabels(none) mlabels(none) mgroups(none) ///
prehead("\noalign{\vskip 0.25cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel D.} & \multicolumn{4}{c}{$\Delta$ Log Services GDP Per Capita}\\" "\noalign{\vskip 0.1cm}") ///
prefoot("\noalign{\vskip 0.1cm}" "\noalign{\vskip 0.3cm}" "\hline" "\noalign{\vskip 0.1cm}" "Rural Share in 1991 & \multicolumn{1}{c}{} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Region FE  & & & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Baseline Controls & & & & \multicolumn{1}{c}{\checkmark}\\") ///
postfoot("\hline" "\end{tabular}" "\begin{tablenotes}[flushleft]" "\setlength{\itemindent}{-2.49997pt}" "\item \textit{Notes:} Robust standard errors in parentheses. *** Significant at the 1\% level; ** Significant at the 5\% level; * Significant at the 10\% level." "\end{tablenotes}" "\end{threeparttable}" "\end{adjustbox}" "\end{table}")




********************************************************************************





*** Baseline and State FE ***
eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


*** Full Controls
eststo clear
foreach v in dlog_pib_tot dlog_pib_agro dlog_pib_indust dlog_pib_serv{
    eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


*** AKM Correction ***

* Run this all at once
drop pr_barley_1995
local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991

reg_ss dlog_pib_tot, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dlog_pib_tot, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dlog_pib_agro, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dlog_pib_agro, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dlog_pib_indust, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dlog_pib_indust, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dlog_pib_serv, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)

reg_ss dlog_pib_serv, shiftshare_var(dfaoc95) control_varlist(`controls') share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

********************************************************************************



***** Full Panel Regressions ***************************************************

clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\municip_gdp.dta"

gsort +cod +year

* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991


* Outcomes
foreach v of varlist pib_tot-pib_serv {
	gen `v'_pc = `v'/pesotot
	gen log_`v' = log(`v'_pc)
}



* Others
gen log_area = log(geo_area_2010)
gen log_pop = log(pesotot)

drop if year >2010
drop if missing(log_pib_tot)
drop if missing(log_pib_agro)
drop if missing(log_pib_indust)
drop if missing(log_pib_serv)
drop if missing(log_income_1991)
drop if missing(log_popdens_1991)
drop if missing(agr_sh_1991)
drop if missing(analf_1991)

*** Regressions ***

* Baseline Controls
eststo clear
foreach v in log_pib_tot log_pib_agro log_pib_indust log_pib_serv{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year, absorb (cod year) vce(cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress


* Full Set Controls
eststo clear
foreach v in log_pib_tot log_pib_agro log_pib_indust log_pib_serv{
eststo: qui reghdfe `v' sum_fao_cattle_1995 c.log_income_1991#i.year c.log_popdens_1991#i.year ///
c.agr_sh_1991#i.year c.analf_1991#i.year c.dist_state#i.year c.dist_federal#i.year c.log_area#i.year c.latitude#i.year ///
c.longitude#i.year c.altitude#i.year c.capital_dummy#i.year, absorb (cod year) vce (cluster cod)
}
esttab, se ar2 stat ( r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress





