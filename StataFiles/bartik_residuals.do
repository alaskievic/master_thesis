cd "C:\Users\Andrei\Desktop\Dissertation\Dados"

log using mylog.log

import excel "C:\Users\Andrei\Desktop\Dissertation\Dados\pq_deflate_longer.xlsx", sheet("Sheet1") firstrow clear

xtset cod

xtreg pq_log i.year, fe

predict resid, residuals

