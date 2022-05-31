
set more off
cd "C:\Users\Andrei\Desktop\Dissertation\Dados"

import excel "C:\Users\Andrei\Desktop\Dissertation\Dados\pq_bartik_final.xlsx", sheet("Sheet1") firstrow clear


gen log_pq_quant = log(pq_quant)
replace log_pq_quant=0 if log_pq_quant==.

gen log_pq_shares = log(pq_shares)
replace log_pq_shares=0 if log_pq_shares==.

*restringir anos??
destring cod, replace
destring year, replace

xtset cod year

xtreg pq_quant i.year, fe
predict pq_quant_resid, residuals

xtreg pq_shares i.year, fe
predict pq_shares_resid, residuals

xtreg log_pq_quant i.year, fe
predict log_pq_quant_resid, residuals


xtreg log_pq_shares i.year, fe
predict log_pq_shares_resid, residuals 
