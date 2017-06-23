use "master jan 6 2017.dta", clear

//keep if study3==1 & mfg==6 & post==0
gen middlecentervolume = middlecentervelocity * 22.5 * 22.5 / 144.0
gen middleleftvolume = middleleftvelocity * 22.5 * 22.5 / 144.0
gen middlerightvolume = middlerightvelocity * 22.5 * 22.5 / 144.0
gen upperleftvolume = upperleftvelocity * 22.5 * 22.5 / 144.0
gen uppercentervolume = uppercentervelocity * 22.5 * 22.5 / 144.0
gen upperrightvolume = upperrightvelocity * 22.5 * 22.5 / 144.0
gen lowerleftvolume = lowerleftvelocity * 22.5 * 22.5 / 144.0
gen lowercentervolume = lowercentervelocity * 22.5 * 22.5 / 144.0
gen lowerrightvolume = lowerrightvelocity * 22.5 * 22.5 / 144.0

summ middlecentervolume

gen testavgvel = (upperleftvelocity + uppercentervelocity + upperrightvelocity + ///
	middleleftvelocity + middlecentervelocity + middlerightvelocity + 	///
	lowerleftvelocity + lowercentervelocity + lowerrightvelocity)/9.0
gen testvolume = 22.5 * 22.5 * testavgvel / 144.0

xtmixed intakevolume || unitnumber: if mfg == 6 & study3==1 & post==0
xtmixed exhaustvolume || unitnumber: if mfg == 6 & study3==1 & post==0
xtmixed balometerintake  || unitnumber: if mfg == 6 & study3==1 & post==1
xtmixed balometerexhaust   || unitnumber: if mfg == 6 & study3==1 & post==1

xtmixed middlecentervolume || unitnumber: if mfg==6 & study3==1 & post==0

xtmixed testvolume || unitnumber: if mfg==6 & study3==1 & post==0
xtmixed middlecentervolume || unitnumber: if mfg==6 & study3==1 & post==0

// keep if mfg==6 & study3==1
// graph box balometerintake , over(unitnumber)
