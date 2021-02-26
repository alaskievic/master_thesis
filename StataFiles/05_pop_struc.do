
***** Population Structural Change Dataset *************************************

clear all

set more off,permanently

use "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\pop_struc.dta"

**** Preparing Data
gsort +cod +year


gen codreg = 1 if codstate < 20
replace codreg = 2 if codstate < 30 & codstate >= 20
replace codreg = 3 if codstate < 40 & codstate >= 30
replace codreg = 4 if codstate < 50 & codstate >= 40
replace codreg = 5 if codstate >= 50


replace P_AGRO = P_AGRO/100
replace P_SERV = P_SERV/100
replace P_COM = P_COM/100
replace P_CONSTR = P_CONSTR/100
replace P_EXTR = P_EXTR/100
replace P_TRANSF = P_TRANSF/100
replace P_INDUST = P_INDUST/100

* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991
gen log_val_outpa_1995 = log(val_outpa_1995)


egen totsum = sum(tot_trab) if year ==2000

gen totsh = tot_trab/totsum




* Others
gen log_area = log(geo_area_2010)


** Outcomes
gen log_wtrans = log(w_trans)
gen log_wagro = log(w_agro)
gen log_windust = log(w_indust)
gen log_wserv = log(w_serv)
gen sharepop = tot_trab/pesotot
gen urbshare = pesourb/pesotot

* Sidra
gen log_tot_trab = log(tot_trab)
gen log_agro_gn = log(agro_gn)
gen log_indust_gn = log(indust_gn)
gen log_agro_ns = log(agro_ns)
gen log_indust_ns = log(indust_ns)
gen log_agro_os = log(agro_os)
gen log_indust_os = log(indust_os)
gen log_agro_gold = log(agro_gold)
gen log_indust_gold = log(indust_gold)

gen sagro_gn = agro_gn/tot_trab 
gen sindust_gn = indust_gn/tot_trab
gen sagro_ns = agro_ns/tot_trab
gen sindust_ns = indust_ns/tot_trab
gen sagro_os = agro_os/tot_trab
gen sindust_os = indust_os/tot_trab
gen sagro_gold = agro_gold/tot_trab
gen sindust_gold = indust_gold/tot_trab





******************* Differences ************************************************

*Standardized
*egen zsum_fao_cattle_1995 = std(sum_fao_cattle_1995)

* Measures
gen dfao  = sum_fao - sum_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dshares  = pq_sum  - pq_sum[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaoc95 = sum_fao_cattle_1995 - sum_fao_cattle_1995[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaocact = sum_fao_cattle_actual - sum_fao_cattle_actual[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaochigh = sum_fao_cattle_high_1995 - sum_fao_cattle_high_1995[_n-1] if year == 2010 & cod == cod[_n-1]

*gen dzfaoc95 = zsum_fao_cattle_1995 - zsum_fao_cattle_1995[_n-1] if year == 2010 & cod == cod[_n-1]




* Shares - PNUD
gen dagro_sh = P_AGRO - P_AGRO[_n-1] if year == 2010 & cod == cod[_n-1]
gen dserv_sh = P_SERV - P_SERV[_n-1] if year == 2010 & cod == cod[_n-1]
gen dcom_sh = P_COM - P_COM[_n-1] if year == 2010 & cod == cod[_n-1]
gen dconstr_sh = P_CONSTR - P_CONSTR[_n-1] if year == 2010 & cod == cod[_n-1]
gen dextr_sh = P_EXTR - P_EXTR[_n-1] if year == 2010 & cod == cod[_n-1]
gen dtransf_sh = P_TRANSF - P_TRANSF[_n-1] if year == 2010 & cod == cod[_n-1]
gen dindust_sh = P_INDUST - P_INDUST[_n-1] if year == 2010 & cod == cod[_n-1]
gen dtrab_sh = sharepop - sharepop[_n-1] if year == 2010 & cod == cod[_n-1]
gen durbsh = urbsh - urbsh[_n-1] if year == 2010 & cod == cod[_n-1]

gen wtot_sh = totsh[_n-1] if year == 2010 & cod == cod[_n-1]



* Shares - Sidra
gen dlog_tot_trab = log_tot_trab - log_tot_trab[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_gn = log_agro_gn - log_agro_gn[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_gn = log_indust_gn - log_indust_gn[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_ns = log_agro_ns - log_agro_ns[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_ns = log_indust_ns - log_indust_ns[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_os = log_agro_os - log_agro_os[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_os = log_indust_os  - log_indust_os[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_gold = log_agro_gold  - log_agro_gold[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_gold = log_indust_gold - log_indust_gold[_n-1] if year == 2010 & cod == cod[_n-1]

gen dsagro_gn = sagro_gn - sagro_gn[_n-1] if year == 2010 & cod == cod[_n-1]
gen dsindust_gn = sindust_gn - sindust_gn[_n-1] if year == 2010 & cod == cod[_n-1]
gen dsagro_ns = sagro_ns - sagro_ns[_n-1] if year == 2010 & cod == cod[_n-1]
gen dsindust_ns = sindust_ns - sindust_ns[_n-1] if year == 2010 & cod == cod[_n-1]
gen dsagro_os = sagro_os - sagro_os[_n-1] if year == 2010 & cod == cod[_n-1]
gen dsindust_os = sindust_os - sindust_os[_n-1] if year == 2010 & cod == cod[_n-1]
gen dsagro_gold = sagro_gold  - sagro_gold[_n-1] if year == 2010 & cod == cod[_n-1]
gen dsindust_gold = sindust_gold  - sindust_gold[_n-1] if year == 2010 & cod == cod[_n-1]

* Wages
gen dlog_wagro = log_wagro- log_wagro[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_windust = log_windust- log_windust[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_wtrans = log_wagro- log_wtrans[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_wserv = log_wagro- log_wserv[_n-1] if year == 2010 & cod == cod[_n-1]



drop if missing(log_income_1991)
drop if missing(log_popdens_1991)
drop if missing(agr_sh_1991)
drop if missing(analf_1991)
drop if missing(temp_daniel)
drop if missing(rain_daniel)
drop if missing(lat) 
drop if missing(longit)
drop if missing(log_windust)
drop if missing(log_wagro)
drop if missing(sum_fao_cattle_1995)

by cod (year), sort: keep if _N == 2 & year[1] == 2000 & year[_N] == 2010


********************************************************************************


*Summary Statistics
sort  year
by year: summarize sum_fao_cattle_1995

summarize dfaoc95

by year: summarize P_AGRO P_INDUST P_SERV log_wagro log_windust log_wserv urbshare

summarize dagro_sh dindust_sh  dserv_sh dlog_wagro dlog_windust dlog_wserv durbsh


 
summarize dlog_tot_trab dlog_agro_ns dlog_indust_ns dlog_agro_gn dlog_indust_gn dlog_agro_os dlog_indust_os dlog_agro_gold dlog_indust_gold



gen dlog_tot_trab = log_tot_trab - log_tot_trab[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_gn = log_agro_gn - log_agro_gn[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_gn = log_indust_gn - log_indust_gn[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_ns = log_agro_ns - log_agro_ns[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_ns = log_indust_ns - log_indust_ns[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_os = log_agro_os - log_agro_os[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_os = log_indust_os  - log_indust_os[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_gold = log_agro_gold  - log_agro_gold[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_gold = log_indust_gold - log_indust_gold[_n-1] if year == 2010 & cod == cod[_n-1]



egen dfaoc_median = median(dfaoc95)

gen dfaoc95_p50 = 1 if dfaoc95 > dfaoc_median
replace dfaoc95_p50 = 0 if dfaoc95_p50 != 1

ttest log_income_1991, by(dfaoc95_p50)           
ttest log_popdens_1991 , by(dfaoc95_p50)           
ttest agr_sh_1991, by(dfaoc95_p50)
ttest analf_1991 , by(dfaoc95_p50)



ttest altitude , by(dfaoc95_p50)
ttest longit , by(dfaoc95_p50)
ttest lat , by(dfaoc95_p50)
ttest dist_federal , by(dfaoc95_p50)
ttest dist_state , by(dfaoc95_p50)
ttest rain_daniel , by(dfaoc95_p50)
ttest temp_daniel , by(dfaoc95_p50)
ttest capital_dummy , by(dfaoc95_p50)
ttest log_area , by(dfaoc95_p50)







********************************************************************************


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg [weight=wtot_sh], vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



* Aggregation at microregion


drop if year == 2000


collapse (firstnm) codreg (mean) dfaoc95 dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wser log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, by(codmicro)

*collapse (firstnm) codreg (mean) dfaoc95 dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wser log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 [weight = pesotot], by(codmicro)


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress








***** Baseline Regressions *****************************************************

drop if year == 2000

********************************************************************************

eststo clear
eststo: qui reg durbsh dfaoc95, vce (cluster cod)
eststo: qui reg durbsh dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg durbsh dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg durbsh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
esttab, se ar2 stat(r2_a N, fmt(3 0))  keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
eststo: qui reg dagro_sh dfaoc95, vce (cluster cod)
eststo: qui reg dagro_sh dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dagro_sh dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/empsh_a.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect of the Commodity Shock on Employment Shares and Wages) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\begin{table}[h]" "\centering" "\begin{adjustbox}{max width=\textwidth}" "\begin{threeparttable}" "\caption{@title}" "label{tab::empshares}" "\begin{tabular}{l* {5}S[table-format = 1.6]}" ///
"\hline \hline" "\noalign{\vskip 0.2cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel A.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Agriculture}\\" "\noalign{\vskip 0.1cm}")



eststo clear
eststo: qui reg dindust_sh dfaoc95, vce (cluster cod)
eststo: qui reg dindust_sh dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dindust_sh dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dindust_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/empsh_b.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel B.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Manufacturing}\\" "\noalign{\vskip 0.1cm}") ///



eststo clear
eststo: qui reg dserv_sh dfaoc95, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/empsh_c.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel C.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Services}\\" "\noalign{\vskip 0.1cm}") ///


eststo clear
eststo: qui reg dlog_wagro dfaoc95, vce (cluster cod)
eststo: qui reg dlog_wagro dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlog_wagro dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlog_wagro dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/empsh_d.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel D.} & \multicolumn{4}{c}{$\Delta$ Log Wages in Agriculture}\\" "\noalign{\vskip 0.1cm}") ///


eststo clear
eststo: qui reg dlog_windust dfaoc95, vce (cluster cod)
eststo: qui reg dlog_windust dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlog_windust dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlog_windust dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/empsh_e.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel E.} & \multicolumn{4}{c}{$\Delta$ Log Wages in Manufacturing}\\" "\noalign{\vskip 0.1cm}") ///


eststo clear
eststo: qui reg dlog_wserv dfaoc95, vce (cluster cod)
eststo: qui reg dlog_wserv dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dlog_wserv dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dlog_wserv dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Tables/empsh_f.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfaoc95) replace noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect of the Commodity Shock on Sectoral GDP) collabels(none) eqlabels(none) mlabels(none) mgroups(none) ///
prehead("\noalign{\vskip 0.25cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel F.} & \multicolumn{4}{c}{$\Delta$ Log Wages in Services}\\" "\noalign{\vskip 0.1cm}") ///
prefoot("\noalign{\vskip 0.1cm}" "\noalign{\vskip 0.3cm}" "\hline" "\noalign{\vskip 0.1cm}" "Rural Share in 1991 & \multicolumn{1}{c}{} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Region FE  & & & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Baseline Controls & & & & \multicolumn{1}{c}{\checkmark}\\") ///
postfoot("\hline" "\end{tabular}" "\begin{tablenotes}[flushleft]" "\setlength{\itemindent}{-2.49997pt}" "\item \textit{Notes:} Robust standard errors in parentheses. *** Significant at the 1\% level; ** Significant at the 5\% level; * Significant at the 10\% level." "\end{tablenotes}" "\end{threeparttable}" "\end{adjustbox}" "\end{table}")




********************************************************************************

*************************** Alternative Measures *******************************

* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* Actual measure
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaocact log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaocact) star(* 0.10 ** 0.05 *** 0.01) compress


* No Cattle
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao) star(* 0.10 ** 0.05 *** 0.01) compress

* High Inputs
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaochigh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaochigh) star(* 0.10 ** 0.05 *** 0.01) compress

* Only shares
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dshares log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dshares) star(* 0.10 ** 0.05 *** 0.01) compress


egen zfao2000 =  std(sum_fao_cattle_1995) if year == 2000
egen zfao2010 =  std(sum_fao_cattle_1995) if year == 2010
by year: summarize zfao2000 zfao2010

gsort +cod +year

replace zfao2000 = zfao2010 if year == 2010

gen dzfao = zfao2000 - zfao2000[_n-1] if year == 2010 & cod == cod[_n-1]



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dzfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dzfao) star(* 0.10 ** 0.05 *** 0.01) compress


***************************** Robustness ***************************************

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



*** Spatial Correlation ***
* Cluster microreg
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Cluster mesoreg
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmeso)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* State
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codstate)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Conley
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (50)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (100)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (200)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (400)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



*** AKM ***
drop pr_barley_1995


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


local controls log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 codreg
reg_ss dagro_sh, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dagro_sh, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


reg_ss dindust_sh, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dindust_sh, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


reg_ss dserv_sh, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dserv_sh, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


reg_ss durbsh, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss durbsh, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)


reg_ss dlog_wagro, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlog_wagro, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dlog_windust, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlog_windust, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)

reg_ss dlog_wserv, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(1)
reg_ss dlog_wserv, shiftshare_var(dfaoc95) control_varlist(log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) share_varlist(pr_banana_1995-pr_wheat_1995) akmtype(0)



********************************************************************************

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust dlog_wserv durbsh{
eststo: qui reg `v' dfaoc95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust dlog_wserv durbsh{
eststo: qui reg `v' dfaoc95 agr_sh_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust dlog_wserv durbsh{
eststo: qui reg `v' dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust dlog_wserv durbsh{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust dlog_wserv durbsh{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 linten_1995 val_outpw_1995 i.codreg, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



*** No Controls ***

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh{
eststo: qui reg `v' dfaoc95, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

xtset cod year, delta(10)

egen zfaoc95 = std(sum_fao_cattle_1995)

eststo clear
foreach v in P_AGRO P_INDUST P_SERV{
eststo: qui xtreg `v' sum_fao_cattle_1995 i.year, fe robust
}
esttab, se ar2 stat (r2_a N) keep(sum_fao_cattle_1995) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in P_AGRO P_INDUST P_SERV{
eststo: qui xtreg `v' zfaoc95 i.year, fe robust
}
esttab, se ar2 stat (r2_a N) keep(zfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Baseline Controls
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Baseline and State FE
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Full Set of Controls
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 altitude longit lat dist_federal dist_state rain_daniel temp_daniel capital_dummy log_area i.codstate, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



***** Total Employment *****
*Lets use first ns (afeter use old sectors)

* No Controls
eststo clear
foreach v in dlog_tot_trab dlog_agro_ns dlog_indust_ns{
eststo: qui reg `v' dfaoc95, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* Rural Share
eststo clear
foreach v in dlog_tot_trab dlog_agro_ns dlog_indust_ns{
eststo: qui reg `v' dfaoc95 agr_sh_1991, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* Rural Share and Region FE
eststo clear
foreach v in dlog_tot_trab dlog_agro_ns dlog_indust_ns{
eststo: qui reg `v' dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* Full Controls
eststo clear
foreach v in dlog_tot_trab dlog_agro_ns dlog_indust_ns{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

********************************************************************************


eststo clear
foreach v in dlog_tot_trab dlog_agro_gn dlog_agro_ns dlog_agro_os dlog_agro_gold{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dlog_tot_trab dlog_indust_gn dlog_indust_ns dlog_indust_os dlog_indust_gold{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Sidra Shares 
eststo clear
foreach v in dsagro_gn dsagro_ns dsagro_os dsagro_gold{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dsindust_gn dsindust_ns dsindust_os dsindust_gold{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster cod)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


********************************************************************************
************************** AKM *************************************************

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
************************** Dummies *********************************************

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



*** For the Differences

* Top 10%
xtile pct_fao_10 = dfaoc95, n(9)
gen faodt10 = 1 if pct_fao_10==9
replace faodt10=0 if faodt10==.


* Top 25%
xtile pct_fao_25 = dfaoc95, n(4)
gen faodt25 = 1 if pct_fao_25==4
replace faodt25=0 if faodt25==.

* Bottom 10%
gen faodb10 = 1 if pct_fao_10==1
replace faodb10=0 if faodb10==.

* Bottom 25%
gen faodb25 = 1 if pct_fao_25==1
replace faodb25=0 if faodb25==.

* Above and below the median
egen fao_median = median(dfaoc95)
gen faotmed = 1 if dfaoc95 > fao_median
replace faotmed=0 if faotmed==.
gen faobmed = 1 if dfaoc95 < fao_median
replace faobmed=0 if faobmed==.



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodt10) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' faodt25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodt25) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' faotmed log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faotmed) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' faodb10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodb10) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' faodb25 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodb25) star(* 0.10 ** 0.05 *** 0.01) compress


********************************************************************************
********************** Spatial Correlation *************************************

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, vce (cluster codmicro)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* 50 KM
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (50)
}

esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* 100 KM
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (100)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* 150 KM
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh dlog_wagro dlog_windust{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, ///
spatial latitude(lat) longitude (longit) dist (150)
}
esttab, se ar2 stat (r2_a N) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* 200 KM
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























