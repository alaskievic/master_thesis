set more off
use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\summ.dta"
destring year, replace

* Revenues
summarize exp_itr rev_impost_tot rev_impost_outros rev_iptu rev_iss rev_taxas rev_tot rev_fpm rev_corrente rev_orca transf_ipva transf_icms trans_estad if year==2000

* Controls 
summarize pesotot  log_population altitude dist_federal dist_state geo_area_2000 geo_area_2010 latitude longitude capital_dummy capitaldummy if year==2000

summarize pesotot  log_population altitude dist_federal dist_state geo_area_2000 geo_area_2010 latitude longitude capital_dummy capitaldummy if year==2000

estpost summarize exp_itr rev_impost_tot rev_impost_outros rev_iptu rev_iss rev_taxas rev_tot rev_fpm rev_corrente rev_orca transf_ipva transf_icms trans_estad if year==2000

estpost summarize exp_itr rev_impost_tot rev_impost_outros rev_iptu rev_iss rev_taxas rev_tot rev_fpm rev_corrente rev_orca transf_ipva transf_icms trans_estad if year==2010


