***  ITR Agreement

clear all

set more off, permanently

use "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\itr_conv_final.dta"

********** Setting up **********

foreach v of varlist pq_2000-pq_2015{
	gen log_`v' = log(`v')

}



foreach v of varlist btk_2000-btk_2015{
	gen log_`v' = log(`v')

}

gen log_area = log(geo_area_2010)


************
eststo clear
foreach v of varlist log_pq_2005-log_pq_2010{

	eststo: qui reg dummy_op_2010 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2005 log_pq_2006 log_pq_2007 log_pq_2008 log_pq_2009 log_pq_2010) compress


eststo clear
foreach v of varlist log_pq_2005-log_pq_2010{

	eststo: qui reg dummy_conv_op_2010 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2005 log_pq_2006 log_pq_2007 log_pq_2008 log_pq_2009 log_pq_2010) compress


eststo clear
foreach v of varlist log_pq_2005-log_pq_2010{

	eststo: qui reg dummy_vigen_2010 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2005 log_pq_2006 log_pq_2007 log_pq_2008 log_pq_2009 log_pq_2010) compress


************
eststo clear
foreach v of varlist log_pq_2000-log_pq_2005{

	eststo: qui reg dummy_op_2010 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2000 log_pq_2001 log_pq_2002 log_pq_2003 log_pq_2004 log_pq_2005) compress


eststo clear
foreach v of varlist log_pq_2000-log_pq_2005{

	eststo: qui reg dummy_conv_op_2010 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2000 log_pq_2001 log_pq_2002 log_pq_2003 log_pq_2004 log_pq_2005) compress


eststo clear
foreach v of varlist log_pq_2000-log_pq_2005{

	eststo: qui reg dummy_vigen_2010 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2000 log_pq_2001 log_pq_2002 log_pq_2003 log_pq_2004 log_pq_2005) compress


************
eststo clear
foreach v of varlist log_pq_2010-log_pq_2015{

	eststo: qui reg dummy_op_2015 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2010 log_pq_2011 log_pq_2012 log_pq_2013 log_pq_2014 log_pq_2015) compress



eststo clear
foreach v of varlist log_pq_2010-log_pq_2015{

	eststo: qui reg dummy_conv_op_2015 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2010 log_pq_2011 log_pq_2012 log_pq_2013 log_pq_2014 log_pq_2015) compress


eststo clear
foreach v of varlist log_pq_2010-log_pq_2015{

	eststo: qui reg dummy_vigen_2015 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(log_pq_2010 log_pq_2011 log_pq_2012 log_pq_2013 log_pq_2014 log_pq_2015) compress


*********** Differences
log using "C:\Users\Andrei\Desktop\Dissertation\Dados\master_thesis\StataFiles\itr_conv_final.log"


gen dpq1 = pq_2010 - pq_2000
gen dpq2 = pq_2015 - pq_2000
gen dpq3 = log_pq_2010 - log_pq_2000
gen dpq4 = log_pq_2015 - log_pq_2000
gen dpq5 = log(dpq1)
gen dpq6 = log(dpq2)



gen dbtk1 = btk_2010 - btk_2000
gen dbtk2 = btk_2015 - btk_2000
gen dbtk3 = log_btk_2010 - log_btk_2000
gen dbtk4 = log_btk_2015 - log_btk_2000
gen dbtk5 = log(dbtk1)
gen dbtk6 = log(dbtk2)



eststo clear

foreach v of varlist dpq3-dpq4{
eststo: qui reg dummy_op_2010 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(dpq3 dpq4) compress


eststo clear
eststo: qui ivreg2 dummy_op_2010 dist_state dist_federal log_area latitude ///
 longitude altitude capital_dummy (dbtk3 = dpq3), robust ffirst savefirst

 eststo: estimates restore _ivreg2_dbtk3

 eststo: qui ivreg2 dummy_op_2010 dist_state dist_federal log_area latitude ///
 longitude altitude capital_dummy (dbtk4 = dpq4), robust ffirst savefirst

 eststo: estimates restore _ivreg2_dbtk4
 
 
 
 esttab, se ar2 stat (widstat) keep(dpq3 dpq4 dbtk3 dbtk4) compress


 

eststo clear

foreach v of varlist dpq3-dpq4{
eststo: qui reg dummy_conv_op_2010 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(dpq3 dpq4) compress


 eststo clear
eststo: qui ivreg2 dummy_conv_op_2010 dist_state dist_federal log_area latitude ///
 longitude altitude capital_dummy (dbtk3 = dpq3), robust ffirst savefirst

 eststo: estimates restore _ivreg2_dbtk3

 eststo: qui ivreg2 dummy_conv_op_2010 dist_state dist_federal log_area latitude ///
 longitude altitude capital_dummy (dbtk4 = dpq4), robust ffirst savefirst

 eststo: estimates restore _ivreg2_dbtk4
 
 
 esttab, se ar2 stat (widstat) keep(dpq3 dpq4 dbtk3 dbtk4) compress





eststo clear

foreach v of varlist dpq3-dpq4{
eststo: qui reg dummy_op_2015 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(dpq3 dpq4) compress



 eststo clear
eststo: qui ivreg2 dummy_op_2015 dist_state dist_federal log_area latitude ///
 longitude altitude capital_dummy (dbtk3 = dpq3), robust ffirst savefirst

 eststo: estimates restore _ivreg2_dbtk3

 eststo: qui ivreg2 dummy_op_2015 dist_state dist_federal log_area latitude ///
 longitude altitude capital_dummy (dbtk4 = dpq4), robust ffirst savefirst

 eststo: estimates restore _ivreg2_dbtk4
 
 
 
 esttab, se ar2 stat (widstat) keep(dpq3 dpq4 dbtk3 dbtk4) compress






eststo clear

foreach v of varlist dpq3-dpq4{
eststo: qui reg dummy_conv_op_2015 `v' dist_state dist_federal log_area latitude ///
                longitude altitude capital_dummy, robust
}
esttab, se ar2 keep(dpq3 dpq4) compress


eststo clear


 
 eststo clear
eststo: qui ivreg2 dummy_conv_op_2015 dist_state dist_federal log_area latitude ///
 longitude altitude capital_dummy (dbtk3 = dpq3), robust ffirst savefirst

 eststo: estimates restore _ivreg2_dbtk3

 eststo: qui ivreg2 dummy_conv_op_2015 dist_state dist_federal log_area latitude ///
 longitude altitude capital_dummy (dbtk4 = dpq4), robust ffirst savefirst

 eststo: estimates restore _ivreg2_dbtk4
 
 
 
 esttab, se ar2 stat (widstat) keep(dpq3 dpq4 dbtk3 dbtk4) compress
 
 
 

log close
