
* censo 2006
destring cod, replace

egen mun_group = group(cod)

egen num_tot = sum(num), by(mun_group)


set more off
gen gini = .
qui levelsof mun_group, local(levels)
foreach i of local levels{
	ineqdec0 area [w = num] if mun_group == `i'
	replace gini = r(gini) if mun_group == `i'

}


drop if num_tot == 0

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

*censo 2017
destring cod, replace

egen mun_group = group(cod)

egen num_tot = sum(num), by(mun_group)
drop if num_tot == 0

set more off
gen gini_2017 = .
qui levelsof mun_group, local(levels)
foreach i of local levels{
	ineqdec0 area [w = num] if mun_group == `i'
	replace gini_2017 = r(gini) if mun_group == `i'

}

*censo 1995
destring cod, replace

egen mun_group = group(cod)

egen num_tot = sum(num), by(mun_group)
drop if num_tot == 0

set more off
gen gini_1995 = .
qui levelsof mun_group, local(levels)
foreach i of local levels{
	ineqdec0 area [w = num] if mun_group == `i'
	replace gini_1995 = r(gini) if mun_group == `i'

}


* censo 1995 correto(?)
gen avg = area/num

set more off

gen gini_1995_corr = .
qui levelsof mun_group, local(levels)
foreach i of local levels{
	ineqdec0 avg [w = num] if mun_group == `i'
	replace gini_1995_corr = r(gini) if mun_group == `i'

}

* censo 2006 correto(?)
gen avg = area/num

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





