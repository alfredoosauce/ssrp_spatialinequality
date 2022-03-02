For creating dina_sd.dta, download DINA micro-files from http://gabriel-zucman.eu/usdina/ and run the following code. The data for the deflator can be found in the DINA excel files. 

matrix sd_dina = J(58,3,.)

forvalues y = 1962(1)2019 {
    
	local row = `y' - 1962 + 1
	matrix sd_dina[`row',1] = `y'
	
	if `y' != 1963 & `y' != 1965 {
	
		use usdina`y', clear
		gen year = `y'
		merge m:1 year using deflator_dina
		drop if _merge == 2
	
		gen threshold = 5000/deflator
	
		gen peinc_w = peinc
		replace peinc_w = threshold if peinc_w <= threshold
		gen poinc_w = peinc + dicab + inkindinc
		replace poinc_w = threshold if poinc_w <= threshold
	
		gen lpeinc_w = log(peinc_w)
		gen lpoinc_w = log(poinc_w)
	
		collapse (sd) lpeinc_w lpoinc_w [fw = dweght]
	
		mkmat lpeinc_w lpoinc_w, matrix(index`y')
	
		matrix sd_dina[`row',2] = index`y'[1,1]
		matrix sd_dina[`row',3] = index`y'[1,2]
	
	}

}

drop _all
svmat sd_dina
rename sd_dina1 year 
rename sd_dina2 sd_lpre_dina
rename sd_dina3 sd_lpost_dina

save dina_sd, replace