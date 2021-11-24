
***** Population Structural Change Dataset *************************************

clear all

set more off, permanently

use "./pop_struc.dta"

**** Preparing Data
gsort +cod +year

drop codstate
gen codstate = int(cod/100000)

gen codreg = 1 if codstate < 20
replace codreg = 2 if codstate < 30 & codstate >= 20
replace codreg = 3 if codstate < 40 & codstate >= 30
replace codreg = 4 if codstate < 50 & codstate >= 40
replace codreg = 5 if codstate >= 50


replace P_AGRO   = agrish_1991 if year == 1991
replace P_INDUST = manush_1991 if year == 1991




replace P_AGRO = P_AGRO/100 if year == 2000 | year == 2010
replace P_SERV = P_SERV/100 if year == 2000 | year == 2010
replace P_COM = P_COM/100 if year == 2000 | year == 2010
replace P_CONSTR = P_CONSTR/100 if year == 2000 | year == 2010
replace P_EXTR = P_EXTR/100 if year == 2000 | year == 2010
replace P_TRANSF = P_TRANSF/100 if year == 2000 | year == 2010 
replace P_INDUST = P_INDUST/100 if year == 2000 | year == 2010 
replace analf_1991 = analf_1991/100 if year == 2000 | year == 2010

gen log_P_AGRO = log(P_AGRO)
gen log_P_INDUST = log(P_INDUST)


* Controls
gen log_income_1991 = log(income_1991)
gen log_popdens_1991 = log(pop_dens_1991)
gen agr_sh_1991 = pesorur_1991/pesotot_1991
gen log_val_outpa_1995 = log(val_outpa_1995)



* Past Controls
bysort cod: egen rural_adult_1991 = total(rural_adult) if year == 1991






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


gen open_exp_2000 = .
replace open_exp_2000 = open_exp if year == 2000
replace open_exp_2000 = open_exp_2000[_n-1] if year == 2010 & cod == cod[_n-1]


gen fao_diff = sum_fao_cattle_high_1995 - sum_fao_cattle_1995

******************* Differences ************************************************

*Standardized
*egen zsum_fao_cattle_1995 = std(sum_fao_cattle_1995)

* Measures
gen dfao      = sum_fao - sum_fao[_n-1] if year == 2010 & cod == cod[_n-1]
gen dshares   = pq_sum  - pq_sum[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaoc95   = sum_fao_cattle_1995 - sum_fao_cattle_1995[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaocact  = sum_fao_cattle_actual - sum_fao_cattle_actual[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfaochigh = sum_fao_cattle_high_1995 - sum_fao_cattle_high_1995[_n-1] if year == 2010 & cod == cod[_n-1]
gen dfao_diff = fao_diff - fao_diff[_n-1] if year == 2010 & cod == cod[_n-1]

gen dfao_placebo_1985 = sum_fao_placebo_1985 - sum_fao_placebo_1985[_n-1] if year == 2010 & cod == cod[_n-1]

*gen dzfaoc95 = zsum_fao_cattle_1995 - zsum_fao_cattle_1995[_n-1] if year == 2010 & cod == cod[_n-1]


gen dresid_not_farm = share_resid_not_farm - share_resid_not_farm[_n-1] if year == 2010 & cod == cod[_n-1]

* Shares - PNUD
gen dagro_sh = P_AGRO - P_AGRO[_n-1] if year == 2010 & cod == cod[_n-1]
replace dagro_sh = P_AGRO - P_AGRO[_n-1] if year == 2000 & cod == cod[_n-1]


gen dlog_agro_sh     = log_P_AGRO - log_P_AGRO[_n-1] if year == 2010 & cod == cod[_n-1]
replace dlog_agro_sh = log_P_AGRO - log_P_AGRO[_n-1] if year == 2000 & cod == cod[_n-1]


gen dserv_sh     = P_SERV - P_SERV[_n-1] if year == 2010 & cod == cod[_n-1]
replace dserv_sh = P_SERV - P_SERV[_n-1] if year == 2000 & cod == cod[_n-1]


gen dcom_sh = P_COM - P_COM[_n-1] if year == 2010 & cod == cod[_n-1]
gen dconstr_sh = P_CONSTR - P_CONSTR[_n-1] if year == 2010 & cod == cod[_n-1]
gen dextr_sh = P_EXTR - P_EXTR[_n-1] if year == 2010 & cod == cod[_n-1]
gen dtransf_sh = P_TRANSF - P_TRANSF[_n-1] if year == 2010 & cod == cod[_n-1]


gen dindust_sh = P_INDUST - P_INDUST[_n-1] if year == 2010 & cod == cod[_n-1]
replace dindust_sh = P_INDUST - P_INDUST[_n-1] if year == 2000 & cod == cod[_n-1]



gen     dlog_indust_sh     = log_P_INDUST - log_P_INDUST[_n-1] if year == 2010 & cod == cod[_n-1]
replace dlog_indust_sh     = log_P_INDUST - log_P_INDUST[_n-1] if year == 2000 & cod == cod[_n-1]


gen dtrab_sh = sharepop - sharepop[_n-1] if year == 2010 & cod == cod[_n-1]

gen durbsh = urbsh - urbsh[_n-1] if year == 2010 & cod == cod[_n-1]
replace durbsh = urbsh - urbsh[_n-1] if year == 2000 & cod == cod[_n-1]



gen wtot_sh = totsh[_n-1] if year == 2010 & cod == cod[_n-1]
gen didh = IDHM - IDHM[_n-1] if year == 2010 & cod == cod[_n-1]
gen deduc = IDHM_E - IDHM_E[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlongev = IDHM_L - IDHM_L[_n-1] if year == 2010 & cod == cod[_n-1]
gen dincome = IDHM_R - IDHM_R[_n-1] if year == 2010 & cod == cod[_n-1]
gen dschool1 = I_ESCOLARIDADE - I_ESCOLARIDADE[_n-1] if year == 2010 & cod == cod[_n-1]
gen dschool2 = I_FREQ_PROP - I_FREQ_PROP[_n-1] if year == 2010 & cod == cod[_n-1]


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

replace dlog_wagro = log_wagro- log_wagro[_n-1] if year == 2000 & cod == cod[_n-1]
replace dlog_windust = log_windust- log_windust[_n-1] if year == 2000 & cod == cod[_n-1]
replace dlog_wserv = log_wagro- log_wserv[_n-1] if year == 2000 & cod == cod[_n-1]




* Openness
gen dopen_exp   = open_exp - open_exp[_n-1] 	if year == 2010 & cod == cod[_n-1]
gen dopen_imp   = open_imp - open_imp[_n-1] 	if year == 2010 & cod == cod[_n-1]
gen dopen_total = open_total - open_total[_n-1] if year == 2010 & cod == cod[_n-1]




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


* Crop-specific controls

foreach v of varlist banana_1995-wheat_1995{

	gen d30_`v' = 1 if `v' > 0.3
	replace d30_`v' = 0 if missing(d30_`v')
}

foreach v of varlist banana_1995-wheat_1995{

	gen d50_`v' = 1 if `v' > 0.5
	replace d50_`v' = 0 if missing(d50_`v')
}


gen group50 = cattle_1995 + coffee_1995 + maize_1995 + soybean_1995 +  /// 
			   sugarcane_1995
			   
gen dgroup50 = 1 if group50 > 0.5
replace dgroup50 = 0 if missing(dgroup50)


by cod (year), sort: keep if _N == 2 & year[1] == 2000 & year[_N] == 2010



save "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\pop_struc_r.dta", replace
********************************************************************************

* Collapse and save

collapse (firstnm) codreg (mean) dfao dfaoc95 dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wser log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991, by(codmicro)

save "C:\Users\Andrei\Desktop\Dissertation\Analysis\master_thesis\StataFiles\pop_struc_micro.dta"


********************************************************************************
*Summary Statistics
sort  year
by year: summarize sum_fao_cattle_1995

summarize dfaoc95

by year: summarize P_AGRO P_INDUST P_SERV log_wagro log_windust log_wserv urbshare

summarize dagro_sh dindust_sh  dserv_sh dlog_wagro dlog_windust dlog_wserv durbsh


 
summarize dlog_tot_trab dlog_agro_ns dlog_indust_ns dlog_agro_gn dlog_indust_gn dlog_agro_os dlog_indust_os dlog_agro_gold dlog_indust_gold


format banana_1995 %4.2f
format banana_1995-wheat_1995
summarize banana_1995-wheat_1995

keep if year == 2000
drop if banana_1995 == .
tabstat banana_1995-wheat_1995, s(mean sd min max) format(%10.3fc)

gen dlog_tot_trab = log_tot_trab - log_tot_trab[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_gn = log_agro_gn - log_agro_gn[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_gn = log_indust_gn - log_indust_gn[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_ns = log_agro_ns - log_agro_ns[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_ns = log_indust_ns - log_indust_ns[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_os = log_agro_os - log_agro_os[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_os = log_indust_os  - log_indust_os[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_agro_gold = log_agro_gold  - log_agro_gold[_n-1] if year == 2010 & cod == cod[_n-1]
gen dlog_indust_gold = log_indust_gold - log_indust_gold[_n-1] if year == 2010 & cod == cod[_n-1]


forvalues i = 1/3{
    bysort cod: replace dfaoc95 = dfaoc95[_n+`i'] if dfaoc95==.
}

egen dfaoc_median = median(dfaoc95)


gen dfaoc95_p50 = 1 if dfaoc95 > dfaoc_median
replace dfaoc95_p50 = 0 if dfaoc95_p50 != 1

keep if year == 2010

foreach v in log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy ///
			 dist_federal dist_state altitude women_labor_share val_outpa_1995{
		ttest `v', by(dfaoc95_p50) 

}

log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
 capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995

 
keep if year == 2010

ttest log_income_1991, by(dfaoc95_p50)           
ttest log_popdens_1991 , by(dfaoc95_p50)           
ttest agr_sh_1991, by(dfaoc95_p50)
ttest analf_1991 , by(dfaoc95_p50)


replace dfaoc95_p50

keep if year == 1991

ttest P_AGRO, by(dfaoc95_p50)
ttest P_INDUST, by(dfaoc95_p50)

ttest altitude , by(dfaoc95_p50)
ttest longit , by(dfaoc95_p50)
ttest lat , by(dfaoc95_p50)
ttest dist_federal , by(dfaoc95_p50)
ttest dist_state , by(dfaoc95_p50)
ttest rain_daniel , by(dfaoc95_p50)
ttest temp_daniel , by(dfaoc95_p50)
ttest capital_dummy , by(dfaoc95_p50)
ttest log_area , by(dfaoc95_p50)





************************ Trying Latex Tables ***********************************

* Employment Shares


eststo clear
eststo: qui reg dagro_sh dfaoc95, vce (cluster cod)
eststo: qui reg dagro_sh dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dagro_sh dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/master_thesis/analysis/output/tables/empsh.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace f noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect of the Commodity Shock on Employment Shares and Wages) collabels(none) eqlabels(none) mlabels(none) mgroups(none) nolines prehead("\begin{table}[h]" "\centering" "\begin{adjustbox}{max width=\textwidth}" "\begin{threeparttable}" "\caption{@title}" "\begin{tabular}{l* {5}S[table-format = 1.6]}" ///
"\hline \hline" "\noalign{\vskip 0.2cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel A.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Agriculture}\\" "\noalign{\vskip 0.1cm}")



eststo clear
eststo: qui reg dindust_sh dfaoc95, vce (cluster cod)
eststo: qui reg dindust_sh dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dindust_sh dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dindust_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/master_thesis/analysis/output/tables/empsh.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) append f noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) plain prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel B.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Manufacturing}\\" "\noalign{\vskip 0.1cm}") ///



eststo clear
eststo: qui reg dserv_sh dfaoc95, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/master_thesis/analysis/output/tables/empsh.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfaoc95) append f noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) plain prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel C.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Services}\\" "\noalign{\vskip 0.1cm}") ///
prefoot("\noalign{\vskip 0.1cm}" "\noalign{\vskip 0.3cm}" "\hline" "\noalign{\vskip 0.1cm}" "Rural Share in 1991 & \multicolumn{1}{c}{} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Region FE  & & & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Baseline Controls & & & & \multicolumn{1}{c}{\checkmark}\\") ///
postfoot("\hline" "\end{tabular}" "\label{tab::empshares}" "\begin{tablenotes}[flushleft]" "\setlength{\itemindent}{-2.49997pt}" "\item \textit{Notes:} See \Cref{sec:defsource} for variable definition and sources. The table presents estimates of \Cref{eqn:maindiff}. The dependent variable is the 2000-2010 change in the listed outcomes. Column (1) reports the estimates without any control.  Columns (2)-(4) extend the set of controls by including the share of population living in the rural area; region fixed effect for the 5 macroregions of Brazil; log income per capita; log population density; and illiteracy rate. All municipalities controls are from the population census of 1991. The means and standard deviations (in parentheses) of the dependent variables in panels A, B and C are -0.068 (0.081), 0.002 (0.055), and 0.008 (0.055). \Cref{tab::mainallcoeff} reports the coefficient for all the controls. Robust standard errors reported in parentheses. *** Significant at the 1\% level; ** Significant at the 5\% level; * Significant at the 10\% level." "\end{tablenotes}" "\end{threeparttable}" "\end{adjustbox}" "\end{table}")


********************************************************************************










* Employment Shares


eststo clear
eststo: qui reg dagro_sh dfao_diff, vce (cluster cod)
eststo: qui reg dagro_sh dfao_diff agr_sh_1991, vce (cluster cod)
eststo: qui reg dagro_sh dfao_diff agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dagro_sh dfao_diff log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



esttab * using C:/Users/Andrei/Desktop/Dissertation/master_thesis/analysis/output/tables/empsh.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) replace f noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) title(The Effect of the Commodity Shock on Employment Shares and Wages - Productivity Gaps) collabels(none) eqlabels(none) mlabels(none) mgroups(none) nolines prehead("\begin{table}[h]" "\centering" "\begin{adjustbox}{max width=\textwidth}" "\begin{threeparttable}" "\caption{@title}" "\begin{tabular}{l* {8}S[table-format = 1.6]}" ///
"\hline \hline" "\noalign{\vskip 0.2cm}") ///
posthead("\noalign{\vskip 0.1cm}" "\hline" "\noalign{\vskip 0.1cm}" "\textbf{Panel A.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Agriculture}\\" "\noalign{\vskip 0.1cm}")



eststo clear
eststo: qui reg dindust_sh dfao_diff, vce (cluster cod)
eststo: qui reg dindust_sh dfao_diff agr_sh_1991, vce (cluster cod)
eststo: qui reg dindust_sh dfao_diff agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dindust_sh dfao_diff log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/master_thesis/analysis/output/tables/empsh.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(r2_a, labels("Adj. $ R^{2} $") fmt(3)) keep(dfaoc95) append f noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) plain prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel B.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Manufacturing}\\" "\noalign{\vskip 0.1cm}") ///



eststo clear
eststo: qui reg dserv_sh dfaoc95, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 agr_sh_1991, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 agr_sh_1991 i.codreg, vce (cluster cod)
eststo: qui reg dserv_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


esttab * using C:/Users/Andrei/Desktop/Dissertation/master_thesis/analysis/output/tables/empsh.tex, style(tex) label notype cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(N r2_a, labels("Observations" "Adj. $ R^{2} $") fmt(%12.0fc 3)) keep(dfaoc95) append f noabbrev varlabels (dfaoc95 "$\Delta$ CE") starlevels(* 0.10 ** 0.05 *** 0.01) collabels(none) eqlabels(none) mlabels(none) mgroups(none) plain prehead("\noalign{\vskip 0.25cm}") ///
posthead("\textbf{Panel C.} & \multicolumn{4}{c}{$\Delta$ Employment Share in Services}\\" "\noalign{\vskip 0.1cm}") ///
prefoot("\noalign{\vskip 0.1cm}" "\noalign{\vskip 0.3cm}" "\hline" "\noalign{\vskip 0.1cm}" "Rural Share in 1991 & \multicolumn{1}{c}{} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Region FE  & & & \multicolumn{1}{c}{\checkmark} & \multicolumn{1}{c}{\checkmark}\\" ///
"Baseline Controls & & & & \multicolumn{1}{c}{\checkmark}\\") ///
postfoot("\hline" "\end{tabular}" "\label{tab::empshares}" "\begin{tablenotes}[flushleft]" "\setlength{\itemindent}{-2.49997pt}" "\item \textit{Notes:} See \Cref{sec:defsource} for variable definition and sources. The table presents estimates of \Cref{eqn:maindiff}. The dependent variable is the 2000-2010 change in the listed outcomes. Column (1) reports the estimates without any control.  Columns (2)-(4) extend the set of controls by including the share of population living in the rural area; region fixed effect for the 5 macroregions of Brazil; log income per capita; log population density; and illiteracy rate. All municipalities controls are from the population census of 1991. The means and standard deviations (in parentheses) of the dependent variables in panels A, B and C are -0.068 (0.081), 0.002 (0.055), and 0.008 (0.055). \Cref{tab::mainallcoeff} reports the coefficient for all the controls. Robust standard errors reported in parentheses. *** Significant at the 1\% level; ** Significant at the 5\% level; * Significant at the 10\% level." "\end{tablenotes}" "\end{threeparttable}" "\end{adjustbox}" "\end{table}")












* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_diff log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_diff) star(* 0.10 ** 0.05 *** 0.01) compress

















********************************************************************************
replace share_resid_other_mun = share_resid_other_mun[_n-1] if missing(share_resid_other_mun)

gen resid = dfaoc95*share_resid_other_mun

gen teste = dfaoc95 * (1-share_resid_other_mun)

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95  log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


foreach var of varlist dfaoc95 dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv {
summ `var'
gen `var'std = `var'/r(sd)
}


eststo clear
foreach v in dagro_shstd dindust_shstd dserv_shstd durbshstd dlog_wagrostd dlog_winduststd dlog_wservstd{
eststo: qui reg `v' dfaoc95std  log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95std) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 resid share_resid_other_mun  log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 resid share_resid_other_mun) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 teste  log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 teste) star(* 0.10 ** 0.05 *** 0.01) compress







gen share_resid_inside_2006 = share_resid_farm + share_resid_mun

replace share_resid_inside_2006 = share_resid_inside_2006[_n-1] if missing(share_resid_inside_2006)

gen fao_resid = dfaoc95 * share_resid_inside_2006

gsort +share_resid_inside_2006

keep if year == 2010

gen dfao_resid = dfaoc95 * dresid_not_farm



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' fao_resid log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(fao_resid) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 fao_resid log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 fao_resid) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 dfao_resid dresid_not_farm log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 dfao_resid dresid_not_farm) star(* 0.10 ** 0.05 *** 0.01) compress



*******************************
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 fao_resid share_resid_inside_2006, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 fao_resid share_resid_inside_2006) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 fao_resid share_resid_inside_2006 agr_sh_1991, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 fao_resid share_resid_inside_2006) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 fao_resid share_resid_inside_2006 agr_sh_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 fao_resid share_resid_inside_2006) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 fao_resid share_resid_inside_2006 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 fao_resid share_resid_inside_2006) star(* 0.10 ** 0.05 *** 0.01) compress



*********** Remainig tables
keep if year == 2010



* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_placebo_1985 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_placebo_1985) star(* 0.10 ** 0.05 *** 0.01) compress



gen dfao_diff_2 = dfaochigh - dfaoc95

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaochigh log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaochigh) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_diff_2 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_diff_2) star(* 0.10 ** 0.05 *** 0.01) compress




******************************* Pre trends *************************************
********************************************************************************
bysort cod: gen time = _n

tsset cod time


**** PLACEBO

gen dfaoc95_placebo = dfaoc95[_n+1] if year == 2000
gen placebao = dfao_placebo_1985[_n+1]  if year == 2000

* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_placebo_1985 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg if year == 2010, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_placebo_1985) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh{
eststo: qui reg `v' placebao l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult i.codreg if year == 2000, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(placebao) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh{
eststo: qui reg `v' dfaoc95_placebo log_income_1991 log_popdens_1991 analf_1991 agr_sh_1991 i.codreg   if year == 2000, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95_placebo) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95_2010 dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg if pre == 1, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95_2010 dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress




* Baseline without Region Fixed Effects and rural share
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult   if year == 2010, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* Baseline new controls
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult i.codreg  if year == 2010, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg if year == 2010, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


gen agrish_r1991 = P_AGRO if year == 1991
bysort cod: replace agrish_r1991 = agrish_r1991[_n-2] if year == 2010



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 analf_1991 agrish_r1991 i.codreg   if year == 2010, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult i.codreg   if year == 2010, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress





reg dagro_sh dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult  		           if year == 2010
reg dindust_sh dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult  		       if year == 2010
reg dserv_sh dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult  		           if year == 2010


reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg 		   	   if year == 2010
reg dindust_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg 		   if year == 2010


reg dagro_sh dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult i.codreg    if year == 2010
reg dindust_sh dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult i.codreg  if year == 2010


**************************** Pre trends ****************************************
 
gen dfaoc95_2010 = dfaoc95 if year == 2010
gen faodt_2010   = faodt10 if year == 2010
gen faotmed_2010 = faotmed if year == 2010


replace dfaoc95_2010  = 0 if missing(dfaoc95_2010)
replace faodt_2010    = 0 if missing(faodt_2010)
replace faotmed_2010  = 0 if missing(faotmed_2010)


forvalues i = 1/3{
    bysort cod: replace dfaoc95 = dfaoc95[_n+`i'] if dfaoc95==.
}


gen 	rur_share_r =.
replace rur_share_r = agr_sh_1991 if year == 1991
replace rur_share_r = rural_adult if year == 1980

gen 	log_income_r = .
replace log_income_r = log_income_1991 if year == 1991
replace log_income_r = log_y_pc_r 	   if year == 1980

gen 	log_popdens_r = .
replace log_popdens_r = log_popdens_1991    if year == 1991
replace log_popdens_r = log_pop_area 	    if year == 1980






eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95_2010 dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg if pre == 1, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95_2010 dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



reg dagro_sh   dfaoc95_2010 dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg if pre == 1, vce (cluster cod)
reg dindust_sh dfaoc95_2010 dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg if pre == 1, vce (cluster cod)
reg dserv_sh   dfaoc95_2010 dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg if pre == 1, vce (cluster cod)

reg dagro_sh dfaoc95_2010 dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg   if pre == 1, vce (cluster cod)
reg dindust_sh dfaoc95_2010 dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg if pre == 1, vce (cluster cod)


* With dummies
reg dagro_sh  faodt_2010 faodt10 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg   if pre == 1, vce (cluster cod)




reg dagro_sh dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg   if year == 2000, vce (cluster cod)

reg dindust_sh dfaoc95 l2.rural_adult l2.log_y_pc_r l2.log_pop_area l2.alpha_adult y2010 i.codreg   if year == 2000, vce (cluster cod)







****************************** Openness ****************************************

gen fao_open = dfaoc95 * open_exp_2000



* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 fao_open open_exp_2000 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 fao_open open_exp_2000) star(* 0.10 ** 0.05 *** 0.01) compress



gen dfao_diff_2 = dfaochigh - dfaoc95

gsort -dfao_diff_2


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_diff log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_diff) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfao_diff_2 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfao_diff_2) star(* 0.10 ** 0.05 *** 0.01) compress



* Adding mean openness as a control
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 open_exp_mean i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 open_total_mean i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Openness as outcomes
eststo clear
foreach v in dopen_exp dopen_imp dopen_total{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


gen fao_open_exp = dfaoc95 * open_exp_mean
gen fao_open_tot = dfaoc95 * open_total_mean

gen dfao_open_exp = dfaoc95 * dopen_exp
gen dfao_open_tot = dfaoc95 * dopen_total

gen fao_open_exp_2000 = dfaoc95 * open_exp_2000

* As explanatory variable
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dopen_exp log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dopen_exp) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dopen_total log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dopen_total) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95  fao_open_exp_2000 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95  fao_open_exp_2000) star(* 0.10 ** 0.05 *** 0.01) compress




* Interactions
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 open_exp_mean fao_open_exp log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 open_exp_mean fao_open_exp) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 open_exp_mean fao_open_tot log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 open_exp_mean fao_open_tot) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 dopen_exp dfao_open_exp log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 dopen_exp dfao_open_exp) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 dopen_total dfao_open_tot log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 dopen_total dfao_open_tot) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 open_exp_2000 fao_open_exp_2000 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 open_exp_2000 fao_open_exp_2000) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 open_exp_2000 fao_open_exp_2000 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 open_exp_2000 fao_open_exp_2000) star(* 0.10 ** 0.05 *** 0.01) compress



* Full controls
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 dopen_exp dfao_open_exp log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 dopen_exp dfao_open_exp) star(* 0.10 ** 0.05 *** 0.01) compress


********************************************************************************

** Playing with controls

* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress




reg dagro_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)


psacalc delta dfaoc95, rmax(0.1118)

psacalc beta dfaoc95, rmax(0.1118)

psacalc delta dfaoc95, mcontrol(agr_sh_1991) rmax(0.1118)


reg dindust_sh dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)

psacalc delta dfaoc95, rmax(0.0676)

psacalc beta dfaoc95, rmax(0.0676)



reg dlog_wserv dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)

psacalc beta dfaoc95


set scheme s1color


net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3)

set scheme lean2

ssc install blindschemes, replace all

set scheme plottig

grstyle color background white
coefplot est1 est2 est3 est4, vertical keep(dfaoc95) yline(0) legend(ring(0) position(8) bmargin(large))
coefplot est5 est6 est7, keep(dfaoc95) xline(0)

* Distances
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* Agri
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state log_val_outpa_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* Past Share
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state log_val_outpa_1995  i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

* Bad?
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit rain_daniel temp_daniel capital_dummy dist_federal dist_state altitude i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



* No Temperature
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit capital_dummy dist_federal dist_state altitude i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


* No temperature + more
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit capital_dummy dist_federal dist_state altitude women_labor_share val_outpw_1995 val_outpa_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 lat longit capital_dummy dist_federal dist_state altitude women_labor_share val_outpw_1995) star(* 0.10 ** 0.05 *** 0.01) compress



* All controls
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 women_labor_share val_outpw_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 women_labor_share val_outpw_1995 val_outpa_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
 capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995 d1_banana_1995-d1_wheat_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
 capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995 d50_banana_1995-d50_wheat_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
 capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995 ///
 d50_banana_1995-d50_wheat_1995 dgroup50 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 ///
 capital_dummy dist_federal dist_state altitude women_labor_share val_outpa_1995 ///
 d50_banana_1995-d50_wheat_1995 dgroup50 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

********************************************************************************
* Teste

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 agr_sh_1991 log_val_outpa_1995 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress




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




********************************** Thiago **************************************



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_avg_tot_income log_density rural_share literacy_rate i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 women_labor_share i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_avg_tot_income log_density rural_share literacy_rate women_labor_share i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress







eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress




eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(4) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991) star(* 0.10 ** 0.05 *** 0.01) compress




foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}



****** AMC
drop if year == 2000


collapse (firstnm) codreg (mean) dfaoc95 dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wser log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 women_labor_share, by(id_AMC)

*collapse (firstnm) codreg (mean) dfaoc95 dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wser log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 [weight = pesotot], by(codmicro)


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster id_AMC)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 women_labor_share i.codreg, vce (cluster id_AMC)
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





* Robust
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce(robust)
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





****************************** Ferman (2021) ***********************************


/***************************************************

Bruno Ferman

This code presents a simple example to run the inference assessment proposed by Ferman (2019), "A simple way to assess inference methods".

First version: August 10th, 2020

Update: November 7th, 2020
- As shown in a revised version of the paper, there is no need to estimate the model under the null to construct the assessment. The assessment can be constructed by simple replacing the vector of outcomes with iid standard normal varibables. 


***************************************************/


/***************************************************
You should include this part of the code with the program "assessment" at the beginning of your code.

The program has 3 inputs, and should run like:

assessment "command" "D" "other".

"command": this input includes the first part of the estimation command. For example, "reg", "xtreg", "xi: reg", and so on.

"D": covariate of interest (we want to assess inference about this variable)

"other": rest of the command (for example, "X , robust" or "X , cluster(W)") 

This command runs 1000 simulations with iid standard normal distribution for the errors. It is easy to modify the command to consider other parameters for the assessment.

you cannot have any variable in your dataset labeled "random".

This code can be easily adjusted to considered other commands that are not OLS regressions. If the command does not have the form "something" "outcome variable" "covariate of interest" "something", then the code must be modified.

This code consider a 5% test based on a t-statistic using the determined estimator for the variance. You can easily adjust the code to consider alternative inference methods.

***************************************************/

* clear all
set matsize 1000


cap program drop assessment
program assessment, eclass
args command D other  

qui: mat R=J(1000,1,.)

_dots 0, title(Assessment running) reps(1000)
forvalues r=1(1)1000 {

qui: gen random = rnormal()

qui: `command' random `D' `other'
qui: mat R[`r',1]=abs(_b[`D']/_se[`D'])>1.96

qui: drop random


_dots `r' 0

}


qui {
preserve
clear 
svmat R
summ R
local summ = r(mean)
restore
}

di "Inference assessment = `summ'"

end	


/***************************************************
This part of the code simply creates a random dataset to use the program.

You should replace that with your dataset

***************************************************/

* set seed 1

* set obs 100 

* gen Y = rnormal() // outcome variable

* gen D =_n<=5 // covariate of interest (we want to assess inference about this variable)

* gen X = rnormal() // other control variables


/***************************************************
Run the regression, and then the assessment 

***************************************************/

* reg Y D X , robust // Regression you want to run. In this case, you want to check whether inference based on t=_b[D]/_se[D] using 1.96 as critical value is reliable.

* assessment "reg" "D" "X , robust"


reg dagro_s dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)

assessment reg dagro_s dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)



eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
assessment reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}

/***************************************************
Notes:

- If you are using another command, such as "areg", "xtreg", "xi: reg", and so on, you can just replace the first input in the program.

- The final input can be changed to include, for example, "X , cluster(Z)", "X , ab(FE)", and so on */




*** Spatial Correlation ***
* Cluster microreg
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress



foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
assessment reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmicro)
}




* Cluster mesoreg
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmeso)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress

foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmeso)
assessment reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster codmeso)
}



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



foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (200)
assessment acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (200)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui acreg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, ///
spatial latitude(lat) longitude (longit) dist (400)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


















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
eststo: qui reg `v'  zfao log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, cluster(cod)
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







* Baseline
eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' dfaoc95 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(dfaoc95) star(* 0.10 ** 0.05 *** 0.01) compress


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' sum_faocact_avg log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(sum_faocact_avg) star(* 0.10 ** 0.05 *** 0.01) compress






* Sensitivity
ssc install sensemakr



sensemakr dagro_sh faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, treat(faodt10) benchmark(agr_sh_1991)


eststo clear
foreach v in dagro_sh dindust_sh dserv_sh durbsh dlog_wagro dlog_windust dlog_wserv{
eststo: qui reg `v' faodt10 log_income_1991 log_popdens_1991 agr_sh_1991 analf_1991 i.codreg, vce (cluster cod)
}
esttab, se(3) ar2 stat (r2_a N, fmt(3 %12.0fc)) keep(faodt10) star(* 0.10 ** 0.05 *** 0.01) compress
