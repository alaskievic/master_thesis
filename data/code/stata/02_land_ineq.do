clear all

/*
* censo 1995 correto(?)
gen avg = area/num

set more off

gen gini_1995_corr = .
qui levelsof mun_group, local(levels)
foreach i of local levels{
	ineqdec0 avg [w = num] if mun_group == `i'
	replace gini_1995_corr = r(gini) if mun_group == `i'

}
*/

* censo 2006 correto(?)
import excel "../../raw/data_municipality/censo_agro/2006/land_area/tabela837_area_def_2006.xlsx", clear

drop in 1/6
drop in -1

rename (A-F) (cod mun_name total group_area area num)

drop total

replace cod = cod[_n-1] if missing(cod)
replace mun_name = mun_name[_n-1] if missing(mun_name)

foreach v of varlist cod-num{
	format `v' %12s
}

destring cod, replace

destring area, replace force
destring num,  replace force

egen mun_group = group(cod)

replace area = 0 if missing(area)
replace num  = 0 if missing(num)

drop if mun_group == 225  | mun_group == 804 | mun_group == 3222 | mun_group == 3274 | ///
		mun_group == 3250 | mun_group == 3332

gen avg = area/num

egen num_tot = sum(num), by(mun_group)

drop if group_area == "Total"

set more off
gen gini_2006_corr = .
qui levelsof mun_group, local(levels)
foreach i of local levels{
	ineqdec0 avg [w = num] if mun_group == `i'
	replace gini_2006_corr = r(gini) if mun_group == `i'

}










* censo 20017 correto(?)
gen avg = area/num

set more off

gen gini_2017_corr = .
qui levelsof mun_group, local(levels)
foreach i of local levels{
	ineqdec0 avg [w = num] if mun_group == `i'
	replace gini_2017_corr = r(gini) if mun_group == `i'

}









/*
set more off
gen gini = .
qui levelsof mun_group, local(levels)
foreach i of local levels{
	ineqdec0 area [w = num] if mun_group == `i'
	replace gini = r(gini) if mun_group == `i'
}
*/



*set more off
*gen gini =.
*levelsof mun_group, local(levels)

*foreach i of local levels {
*	if (num_tot > 0) {
*		replace gini = `i'
*	}
*	else {
*		replace gini = 0
*	}
*}