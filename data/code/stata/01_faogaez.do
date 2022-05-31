*install fmlogit: module fitting a fractional multinomial logit model by quasi-maximum likelihood

*ssc install fmlogit

set more off


*Example

*use http://fmwww.bc.edu/repec/bocode/c/citybudget.dta, clear


*fmlogit governing safety education recreation social urbanplanning, ///
        *eta(minorityleft noleft houseval popdens)


*dfmlogit, at(minorityleft 0 noleft 0 )



************************ FAO-GAEZ intermediate inputs **************************
clear all

use "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_fao.dta"

*drop if total_quant ==.

local fao_int banana_int barley_int citrus_int cocoa_int coffee_int cotton_int ///
			  maize_int rice_int sorghum_int soybean_int sugarcane_int tea_int ///
			  tobacco_int wheat_int
			  
local q_shares banana barley orange cocoa coffee cotton maize rice sorghum soybean ///
			   sugar_cane tea tobacco wheat


fmlogit `q_shares' , eta(`fao_int') difficult technique(dfp)



predict pr_banana pr_barley pr_orange pr_cocoa pr_coffee pr_cotton pr_maize ///
		pr_rice pr_sorghum pr_soybean pr_sugarcane pr_tea pr_tobacco pr_wheat


		
twoway scatter banana pr_banana || lfit banana pr_banana


******** Graphing

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
********* Cattle and Actual Shares *********************************************
set more off


*** Pre-Shares
clear all
use "../.././output/fao_gaez/full_fao_cat_1995.dta"

* Replace order?

local fao_int banana_int barley_int citrus_int cocoa_int coffee_int cotton_int ///
			  maize_int rice_int sorghum_int soybean_int sugarcane_int tea_int ///
			  tobacco_int wheat_int grass_high
			  
local q_shares banana barley orange cocoa coffee cotton maize rice sorghum soybean ///
			   sugar_cane tea tobacco wheat cattle


fmlogit `q_shares' , eta(`fao_int') difficult technique(dfp)

predict pr_banana pr_barley pr_orange pr_cocoa pr_coffee pr_cotton pr_maize ///
		pr_rice pr_sorghum pr_soybean pr_sugarcane pr_tea pr_tobacco pr_wheat ///
		pr_cattle

save "../.././output/fao_gaez/fao_pr_cattle_1995.dta"


* Graphing		
twoway scatter banana pr_banana || lfit banana pr_banana
twoway scatter cattle pr_cattle || lfit cattle pr_banana



*** Actual Shares
clear all
use "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_fao_cat_actual.dta"


local fao_int banana_int barley_int citrus_int cocoa_int coffee_int cotton_int ///
			  maize_int rice_int sorghum_int soybean_int sugarcane_int tea_int ///
			  tobacco_int wheat_int grass_high
			  
local q_shares banana barley orange cocoa coffee cotton maize rice sorghum soybean ///
			   sugar_cane tea tobacco wheat cattle


fmlogit `q_shares' , eta(`fao_int') difficult technique(dfp)

predict pr_banana pr_barley pr_orange pr_cocoa pr_coffee pr_cotton pr_maize ///
		pr_rice pr_sorghum pr_soybean pr_sugarcane pr_tea pr_tobacco pr_wheat ///
		pr_cattle

save "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/fao_pr_cattle_actual.dta"


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



******************** FAO-GAEZ high inputs **************************************
clear all

use "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_fao_cat_1995.dta"


local fao_high banana_high barley_high citrus_high cocoa_high coffee_high cotton_high ///
			  maize_high rice_high sorghum_high soybean_high sugarcane_high tea_high ///
			  tobacco_high wheat_high grass_high
			  
local q_shares banana barley orange cocoa coffee cotton maize rice sorghum soybean ///
			   sugar_cane tea tobacco wheat cattle


fmlogit `q_shares' , eta(`fao_high') difficult technique(dfp)

predict pr_banana_high pr_barley_high pr_orange_high pr_cocoa_high pr_coffee_high ///
		pr_cotton_high pr_maize_high pr_rice_high pr_sorghum_high pr_soybean_high ///
		pr_sugarcane_high pr_tea_high pr_tobacco_high pr_wheat_high pr_cattle_high

save "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/fao_pr_cattle_high_1995.dta", replace


* Graphing		
twoway scatter banana pr_banana || lfit banana pr_banana
twoway scatter cattle pr_cattle || lfit cattle pr_cattle




********************************* Placebo **************************************
use "../.././output/fao_gaez/shares_to_fao_placebo.dta", clear

drop if banana ==.


local fao_int banana_int barley_int grass_high cocoa_int coffee_int maize_int citrus_int ///
			  rice_int sorghum_int soybean_int sugarcane_int ///
			  tobacco_int wheat_int cotton_int tea_int
			  
local q_shares banana barley cattle cocoa coffee maize orange rice sorghum soybean ///
			   sugar_cane tobacco wheat cotton tea 


fmlogit `q_shares' , eta(`fao_int') difficult technique(dfp)

predict pr_banana pr_barley pr_cattle pr_cocoa pr_coffee pr_maize pr_orange ///
		pr_rice pr_sorghum pr_soybean pr_sugarcane pr_tobacco pr_wheat pr_cotton pr_tea

save "../.././output/fao_gaez/fao_pr_placebo_1985.dta", replace


* Graphing		
twoway scatter banana pr_banana || lfit banana pr_banana
twoway scatter cattle pr_cattle || lfit cattle pr_cattle

twoway scatter sugar_cane pr_sugarcane || lfit sugar_cane pr_sugarcane
twoway scatter cocoa pr_cocoa || lfit cocoa pr_cocoa
