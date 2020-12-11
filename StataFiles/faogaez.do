*install fmlogit: module fitting a fractional multinomial logit model by quasi-maximum likelihood

*ssc install fmlogit

set more off


*Example

*use http://fmwww.bc.edu/repec/bocode/c/citybudget.dta, clear


*fmlogit governing safety education recreation social urbanplanning, ///
        *eta(minorityleft noleft houseval popdens)


*dfmlogit, at(minorityleft 0 noleft 0 )



***** FAO-GAEZ dataset
clear all

use "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_fao.dta"

*drop if total_quant ==.

local fao_int banana_int barley_int citrus_int cocoa_int coffee_int cotton_int ///
			  maize_int rice_int sorghum_int soybean_int sugarcane_int tea_int ///
			  tobacco_int wheat_int
			  
local q_shares banana barley orange cocoa coffee cotton maize rice sorghum soybean ///
			   sugar_cane tea tobacco wheat


fmlogit `q_shares' , eta(`fao_int') difficult technique(dfp)



predict pr_banana pr_barley pq_orange pr_cocoa pr_coffee pr_cotton pr_maize ///
		pr_rice pr_sorghum pr_soybean pr_sugarcane pr_tea pr_tobacco pr_wheat


		
twoway scatter banana pr_banana || lfit banana pr_banana


********

clear all 
use "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/fao_pr.dta"

rename pq_orange pr_orange
rename sugar_cane sugarcane

foreach v of varlist banana-tea {
	
	twoway scatter `v' pr_`v' || lfit `v' pr_`v' , name(`v')

}

graph combine banana barley orange cocoa coffee cotton maize rice sorghum soybean sugarcane ///
tea tobacco wheat


foreach v of varlist banana-tea {
	
	reg pr_`v' `v'

}


********* AKM Correction *******************************************************


clear all

use "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/fao_akm.dta"

*drop if total_quant ==.

local fao_int banana_int barley_int citrus_int cocoa_int coffee_int cotton_int ///
			  maize_int rice_int soybean_int sugarcane_int tea_int ///
			  tobacco_int wheat_int
			  
local q_shares banana barley orange cocoa coffee cotton maize rice soybean ///
			   sugar_cane tea tobacco wheat


fmlogit `q_shares' , eta(`fao_int')



predict pr_banana pr_barley pr_orange pr_cocoa pr_coffee pr_cotton pr_maize ///
		pr_rice pr_soybean pr_sugarcane pr_tea pr_tobacco pr_wheat
		
		
save "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_pr_akm.dta"


