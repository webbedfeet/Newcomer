use merged_ATI_LPC_CPC_may_5.dta, clear

order effectivenessquad*, alpha 
keep if unitnumber != 2 & unitnumber !=3

/*
keep if effectivenessquad1 > 0
keep if effectivenessquad2 > 0
keep if effectivenessquad3 > 0
keep if effectivenessquad4 > 0
keep if effectivenessquad5 > 0
keep if effectivenessquad6 > 0
keep if effectivenessquad7 > 0
keep if effectivenessquad8 > 0
keep if effectivenessquad9 > 0
*/
/* Alternatively, using macros */
foreach var of varlist effectivenessquad1-effectivenessquad9 {
	keep if `var' > 0
}



/*
	Table of tests comparing the effectiveness 
	of each sector with that of the center sector 
*/

foreach var of varlist effectivenessquad1-effectivenessquad4 effectivenessquad6-effectivenessquad9 {
 ttest `var' == effectivenessquad5 
}

* To save results into an excel file (will still need formatting)
net install mat2txt.pkg

foreach var of varlist effectivenessquad1-effectivenessquad4 effectivenessquad6-effectivenessquad9 {
 qui ttest `var' == effectivenessquad5
 matrix ttestout = (r(mu_1)-r(mu_2), r(p), r(mu_1)-r(mu_2)-invt(r(df_t),0.975)*r(se), r(mu_1)-r(mu_2)+invt(r(df_t), 0.975)*r(se))
 matrix rownames ttestout = `var'
 matrix colnames ttestout = mean_diff p_value lcb ucb
 
 mat2txt, matrix(ttestout) sav("testtab.xls") append
}


/* 
	Chi-square goodness-of-fit test to see if all sectors
	are equally likely to be the most effective
*/

net install pr0046.pkg

rowranks effectivenessquad1-effectivenessquad9, generate(r1-r9) 

gen best = .
replace best = 1 if r1 == 9
replace best = 2 if r2 == 9
replace best = 3 if r3 == 9
replace best = 4 if r4 == 9
replace best = 5 if r5 == 9
replace best = 6 if r6 == 9
replace best = 7 if r7 == 9
replace best = 8 if r8 == 9
replace best = 9 if r9 == 9

tab best

net install csgof.pkg

csgof best /* Chi square goodness of fit test */

/* Alternatively, using macros
gen best1 = .
foreach i of num 1/9 {
replace best1 = `i' if r`i' == 9
}
tab best1
csgof best1
*/
