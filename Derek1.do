use ~/ARAASTAT/Newcomer/merged_ATI_LPC_CPC_may_5.dta, clear

log using ~/ARAASTAT/Newcomer/Derek1.log, text replace

egen v_mean = rmean(lower*velocity middle*velocity upper*velocity)

gen velo_transformed = v_mean *22.5*22.5/144

gen A = (velo_transformed + balometerintake)/2
gen M = (velo_transformed - balometerintake)

twoway scatter M A

foreach var of varlist lower*velocity middle*velocity upper*velocity {
	gen `var'_transformed = `var' * 22.5*22.5/144
}
gen velo_transformed = v_mean *22.5*22.5/144

replace A = (lowercentervelocity_transformed + balometerintake)/2
replace M = (lowercentervelocity_transformed - balometerintake)

twoway scatter M A

egen 
