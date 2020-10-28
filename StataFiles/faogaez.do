*install fmlogit: module fitting a fractional multinomial logit model by quasi-maximum likelihood

ssc install fmlogit


set more off


*Example

use http://fmwww.bc.edu/repec/bocode/c/citybudget.dta, clear


fmlogit governing safety education recreation social urbanplanning, ///
        eta(minorityleft noleft houseval popdens)



dfmlogit, at(minorityleft 0 noleft 0 )



*FAO-GAEZ dataset
clear all

use "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/full_fao.dta"

*drop if total_quant ==.

local fao_low banana_low barley_low citrus_low cocoa_low coffee_low cotton_low ///
			  maize_low rice_low sorghum_low soybean_low sugarcane_low tea_low ///
			  tobacco_low wheat_low
			  
local q_shares banana barley orange cocoa coffee cotton maize rice sorghum soybean ///
			   sugar_cane tea tobacco wheat


fmlogit `q_shares' , eta(`fao_low') difficult technique(dfp)



predict pr_banana pr_barley pq_orange pr_cocoa pr_coffee pr_cotton pr_maize ///
		pr_rice pr_sorghum pr_soybean pr_sugarcane pr_tea pr_tobacco pr_wheat ///


		
twoway scatter banana pr_banana || lfit banana pr_banana



