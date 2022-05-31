clear all

set more off,permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\munic_2006.dta"

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\munic_2006.log"



gen log_area = log(geo_area_2010)
replace log_area=0 if log_area==.

rename (A18 A19 A20 A21 A22 A23 A24 A25 A26 A27 A28 A29 A30) (mecan_incent iptu_parc iptu_tot iss taxas c_terr d_terr outros indust com_serv turis agro outro)



foreach v of varlist pq_2000-pq_2006 {
	eststo clear
	eststo: qui reg mecan_incent `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg iptu_parc `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg iptu_tot `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg iss `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg taxas `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	esttab, se ar2 keep(`v')
}


foreach v of varlist pq_2000-pq_2006 {
	eststo clear
	eststo: qui reg c_terr `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg d_terr `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg outros `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg indust `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg com_serv `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	esttab, se ar2 keep(`v')
}

foreach v of varlist pq_2000-pq_2006 {
	eststo clear
	eststo: qui reg turis `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg agro `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg outros `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	esttab, se ar2 keep(`v')
}


************************

clear all

set more off,permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\munic_2009.dta"

log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\munic_2009.log"


gen log_area = log(geo_area_2010)
replace log_area=0 if log_area==.

rename (A85 A86 A87 A88 A89 A90 A91 A92 A93 A94 A95 A96 A97 A98 A348 A349) (mecan_incent iptu_parc iptu_tot iss taxas c_terr d_terr outros n_util indust com_serv turis agro outro reg_fund1 reg_fund2)


foreach v of varlist pq_2005-pq_2010 {
	eststo clear
	eststo: qui reg mecan_incent `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg iptu_parc `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg iptu_tot `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg iss `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg taxas `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	esttab, se ar2 keep(`v')
}


foreach v of varlist pq_2005-pq_2010 {
	eststo clear
	eststo: qui reg c_terr `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg d_terr `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg outros `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg n_util `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg indust `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	esttab, se ar2 keep(`v')
}

foreach v of varlist pq_2005-pq_2010 {
	eststo clear
	eststo: qui reg com_serv `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg turis `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg agro `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg outro `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	esttab, se ar2 keep(`v')
}


foreach v of varlist pq_2005-pq_2010 {
	eststo clear
	eststo: qui reg reg_fund1 `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	eststo: qui reg reg_fund2 `v' altitude dist_federal dist_state latitude longitude log_area temp_outono temp_verao temp_primavera temp_inverno rain_inverno rain_verao rain_outono rain_primavera capital_dummy, vce(cluster cod)
	esttab, se ar2 keep(`v')
}
