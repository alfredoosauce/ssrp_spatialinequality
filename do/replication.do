*** Trends in U.S. Spatial Inequality: Concentrating Affluence and a Democratization of Poverty ***
*** C. Gaubert, P. Kline, D. Vergara, D. Yagan ***

*This code replicates all figures in the paper and the online appendix.

*** MAIN FIGURES ***

*Figure 1

use state_BEA, clear

gen pre_caressb = pre + ssb + medicare
gen post_tr = pre + transfers

foreach c in pre pre_caressb post_tr post_tt {
    
	gen `c'_pc = `c'/pop_state
	gen l`c'_pc = log(`c'_pc)
	
}

collapse (sd) lpre_pc lpre_caressb_pc lpost_tr_pc lpost_tt_pc [fw = pop_state], by(year)

twoway (connected lpre_pc year, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lpre_caressb_pc year, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected lpost_tr_pc year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected lpost_tt_pc year, mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)), legend(order(1 "Pre-tax income" 2 "Pre-tax income + Social Security" "+ Medicare" 3 "Pre-tax income + transfers" 4 "Pre-tax income + transfers" "- taxes"))  xlabel(1960 1965 1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("") legend(region(lc(white)) rows(2)) ytitle("Std. dev. of log income") ysc(titleg(2))

*Figure 2

use county_BEA, clear

gen post_tr = pre + transf
gen pre_d = pre/deflator
gen post_tr_d = post_tr/deflator

foreach c in pre post_tr pre_d post_tr_d {
	
	gen `c'_pc = `c'/pop
	gen l`c'_pc = log(`c'_pc)
	
}

collapse (sd) lpre_pc lpost_tr_pc lpre_d_pc lpost_tr_d_pc [fw = pop], by(year)

merge 1:1 year using dina_sd // See readme for details on this database
drop if _merge != 3

gen share_pre = 100*(lpre_pc/sd_lpre_dina)^2
gen share_post = 100*(lpost_tr_pc/sd_lpost_dina)^2

twoway (connected lpre_pc year, mc(navy) lc(navy) lpattern(solid) ms(o) msize(medsmall) yaxis(1)) (connected lpost_tr_pc year, mc(navy) lc(navy) lpattern(tight_dot) ms(o) mfc(white) msize(medsmall) yaxis(1)) (connected lpre_d_pc year, mc(cranberry) lc(cranberry) lpattern(solid) ms(s) msize(medsmall) yaxis(1)) (connected lpost_tr_d_pc year, mc(cranberry) lc(cranberry) lpattern(tight_dot) ms(s) mfc(white) msize(medsmall) yaxis(1)) (connected share_pre year, mc(midgreen) lc(midgreen) lpattern(solid) ms(t) msize(medsmall) yaxis(2)) (connected share_post year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) mfc(white) msize(medsmall) yaxis(2)), legend(order(1 "Pre-tax income" 2 "Pre-tax income + transfers" 3 "Pre-tax income (deflated)" 4 "Pre-tax income + transfers" "(deflated)" 5 "County-level disp. as" "share of person-level:" "Pre-tax income" 6 "County-level disp. as" "share of person-level:" "Pre-tax income + transfers")) xlabel(1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Std. dev. of log income", axis(1)) ytitle("Share of person-level dispersion (%)", axis(2)) yscale(range(0.2 0.4) axis(1)) yscale(range(0 0.1) axis(2)) ylabel(0.2(0.05)0.4, axis(1)) ylabel(0(2)10, axis(2)) legend(region(lc(white))) ysc(titleg(2))

*Figure 3

use county_BEA, clear

foreach c in transf ssb medical inc_mant {
	
	gen `c'_pc = `c'/pop
	gen l`c'_pc = log(`c'_pc)
	
}

collapse (sd) ltransf_pc lssb_pc lmedical_pc linc_mant_pc [fw = pop], by(year)

twoway (connected ltransf_pc year, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lssb_pc year, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected lmedical_pc year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected linc_mant_pc year, mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)), legend(order(1 "Total transfers" 2 "Social Security" 3 "Medical benefits" 4 "Income maintenance benefits")) xlabel(1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("") ytitle("Std. dev. of log transfers") legend(region(lc(white)) rows(2)) ysc(titleg(2))

*Figure 4

use county_SAIPE, clear

preserve

gen lmed_hh_inc = log(med_hh_inc)
collapse (sd) lmed_hh_inc [fw = pop_all], by(year)
tempfile hh
save `hh'

restore

foreach c in all u18 {

	gen tot_pov_`c'_comp = pop_`c' - tot_pov_`c'
	egen A_`c' = sum(tot_pov_`c'), by(year)
	egen B_`c' = sum(tot_pov_`c'_comp), by(year)
	gen term_sum_`c' = abs(tot_pov_`c'/A_`c' - tot_pov_`c'_comp/B_`c')

}
	
collapse (sum) term_sum_all term_sum_u18, by(year)

foreach c in all u18 {

	gen D_pov_`c' = 0.5*term_sum_`c'

}
	
merge 1:1 year using `hh'

twoway (connected lmed_hh_inc year, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected D_pov_all year, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected D_pov_u18 year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)), legend(order(1 "Median household income" 2 "Poverty rate (all)" 3 "Poverty rate (under 18)")) xlabel(1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("") legend(region(lc(white))) ytitle("Std. dev. of log income / Dissimilarity index") ysc(titleg(2)) 

*Figure 5

use county_SAIPE, clear

keep if year == 1989 | year == 2018

xtile pov_rank_1989 = pov_rate_all [fw = pop_all] if year == 1989, nq(100)
xtile pov_rank_2018 = pov_rate_all [fw = pop_all] if year == 2018, nq(100)
gen pov_rank = pov_rank_1989 if year == 1989
replace pov_rank = pov_rank_2018 if year == 2018
replace pov_rank = 101 - pov_rank 

collapse (mean) pov_rate_all [fw = pop_all], by(year pov_rank)

twoway (scatter pov_rate pov_rank if year == 1989, mc(navy) msize(medsmall)) (scatter pov_rate pov_rank if year == 2018, mc(cranberry) ms(s) mfc(white) msize(medium)), graphregion(color(white)) legend(order(1 "1989" 2 "2018")) legend(region(lc(white))) ytitle("Poverty rate (%)") xtitle("Percentile (ranked from highest poverty to lowest poverty)") ysc(titleg(2)) 

*Figure 6

*For state-weights
use state_BEA, clear
keep statefip year pop_state
gen t = .
replace t = 1 if inrange(year,1976,1979)
replace t = 2 if inrange(year,1980,1984)
replace t = 3 if inrange(year,1985,1989)
replace t = 4 if inrange(year,1990,1994)
replace t = 5 if inrange(year,1995,1999)
replace t = 6 if inrange(year,2000,2004)
replace t = 7 if inrange(year,2005,2009)
replace t = 8 if inrange(year,2010,2014)
replace t = 9 if inrange(year,2015,2019)
drop if t == .
collapse (mean) pop_state, by (t statefip)
gen pop = round(pop_state)
tempfile state_weights
save `state_weights'

use ASEC, clear

merge m:1 year using deflator_PCE
drop _merge

replace inctot = (100*inctot)/pcedeflator

*Aggregate at the household level
collapse (sum) inctot (mean) asecwth t, by(hh statefip year)

*Compute V_naive
matrix V_naive = J(9,5,.)

preserve
collapse (p10) p_10 = inctot (p25) p_25 = inctot (p50) p_50 = inctot (p75) p_75 = inctot (p90) p_90 = inctot [pw = asecwth], by(t statefip)

foreach c in 10 25 50 75 90 {
	
	gen lp_`c' = log(p_`c')
	
}

merge 1:1 t statefip using `state_weights'

forvalues i = 1(1)9 {
	
	qui sum lp_10 if t == `i' [fw = pop]
	matrix V_naive[`i',1] = r(Var)
	qui sum lp_25 if t == `i' [fw = pop]
	matrix V_naive[`i',2] = r(Var)
	qui sum lp_50 if t == `i' [fw = pop]
	matrix V_naive[`i',3] = r(Var)
	qui sum lp_75 if t == `i' [fw = pop]
	matrix V_naive[`i',4] = r(Var)
	qui sum lp_90 if t == `i' [fw = pop]
	matrix V_naive[`i',5] = r(Var)
	
}
restore

*Compute V_noise
merge m:1 t statefip using `state_weights'
matrix V_noise = J(9,5,.)

local rep = 300
set seed 12345

forvalues i = 1(1)9 {
	
	matrix aux_`i' = J(51,6,.)
	
	local iter = 0
	local st = 0
	while `st' <= 55 {
		
		local iter = `iter' + 1
		local st = `st' + 1
		
		if `st' == 3 | `st' == 7 | `st' == 14 | `st' == 43 | `st' == 52 {
			
			local st = `st' + 1
			
		}
		
		preserve
		keep if t == `i' & statefip == `st'
		bootstrap stat1=log(r(p10)) stat2=log(r(p25)) stat3=log(r(p50)) stat4=log(r(p75)) stat5=log(r(p90)), reps(`rep'): summarize inctot, detail
		matrix aux_se = e(se)
		matrix aux_`i'[`iter',1] = aux_se[1,1]
		matrix aux_`i'[`iter',2] = aux_se[1,2]
		matrix aux_`i'[`iter',3] = aux_se[1,3]
		matrix aux_`i'[`iter',4] = aux_se[1,4]
		matrix aux_`i'[`iter',5] = aux_se[1,5]
		qui sum pop
		matrix aux_`i'[`iter',6] = r(mean)
		restore
	
	}
	
	preserve
	drop _all
	svmat aux_`i'
	
	egen tot_pop = sum(aux_`i'6)
	gen we = aux_`i'6/tot_pop
	
	forvalues j = 1(1)5 {
		replace aux_`i'`j' = (aux_`i'`j')^2 
		gen aux_`i'`j'_w = aux_`i'`j'*we
		egen aux`j' = sum(aux_`i'`j'_w)
		qui sum aux`j'
		matrix V_noise[`i',`j'] = r(mean)
	}
	restore
	
}

*Compute bias-corrected std. dev.
matrix std_corr = J(9,5,.)

forvalues i = 1(1)9 {
	forvalues j = 1(1)5 {
		
		matrix std_corr[`i',`j'] = sqrt(V_naive[`i',`j'] - V_noise[`i',`j'])
		
	}
	
}

drop _all
svmat std_corr
gen t = _n

twoway (connected std_corr1 t, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected std_corr2 t, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected std_corr3 t, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected std_corr4 t, mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)) (connected std_corr5 t, mc(purple) lc(purple) ms(X) lpattern(longdash)), legend(order(1 "p10" 2 "p25" 3 "p50" 4 "p75" 5 "p90")) graphregion(color(white)) xtitle("") xlabel(1 "76-79" 2 "80-84" 3 "85-89" 4 "90-94" 5 "95-99" 6 "00-04" 7 "05-09" 8 "10-14" 9 "15-19") legend(region(lc(white)) rows(1)) ytitle("Std. dev. of log income percentile") ysc(titleg(2))

*** APPENDIX ***

*Appendix I

*A.I (a)
use state_BEA, clear

gen pre_caressb = pre + ssb + medicare
gen post_tr = pre + transfers

foreach c in pre pre_caressb post_tr post_tt {
    
	gen `c'_pc = `c'/pop_state
	gen l`c'_pc = log(`c'_pc)
	
}

collapse (sd) lpre_pc lpre_caressb_pc lpost_tr_pc lpost_tt_pc, by(year)

twoway (connected lpre_pc year, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lpre_caressb_pc year, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected lpost_tr_pc year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected lpost_tt_pc year, mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)), legend(order(1 "Pre-tax income" 2 "Pre-tax income + Social Security" "+ Medicare" 3 "Pre-tax income + transfers" 4 "Pre-tax income + transfers" "- taxes"))  xlabel(1960 1965 1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("") legend(region(lc(white)) rows(2)) ytitle("Std. dev. of log income") ysc(titleg(2))

*A.I (b)
use county_BEA, clear

gen post_tr = pre + transf
gen pre_d = pre/deflator
gen post_tr_d = post_tr/deflator

foreach c in pre post_tr pre_d post_tr_d {
	
	gen `c'_pc = `c'/pop
	gen l`c'_pc = log(`c'_pc)
	
}

collapse (sd) lpre_pc lpost_tr_pc lpre_d_pc lpost_tr_d_pc, by(year)

merge 1:1 year using dina_sd // See readme for details on this database
drop if _merge != 3

gen share_pre = 100*(lpre_pc/sd_lpre_dina)^2
gen share_post = 100*(lpost_tr_pc/sd_lpost_dina)^2

twoway (connected lpre_pc year, mc(navy) lc(navy) lpattern(solid) ms(o) msize(medsmall) yaxis(1)) (connected lpost_tr_pc year, mc(navy) lc(navy) lpattern(tight_dot) ms(o) mfc(white) msize(medsmall) yaxis(1)) (connected lpre_d_pc year, mc(cranberry) lc(cranberry) lpattern(solid) ms(s) msize(medsmall) yaxis(1)) (connected lpost_tr_d_pc year, mc(cranberry) lc(cranberry) lpattern(tight_dot) ms(s) mfc(white) msize(medsmall) yaxis(1)) (connected share_pre year, mc(midgreen) lc(midgreen) lpattern(solid) ms(t) msize(medsmall) yaxis(2)) (connected share_post year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) mfc(white) msize(medsmall) yaxis(2)), legend(order(1 "Pre-tax income" 2 "Pre-tax income + transfers" 3 "Pre-tax income (deflated)" 4 "Pre-tax income + transfers" "(deflated)" 5 "County-level disp. as" "share of person-level:" "Pre-tax income" 6 "County-level disp. as" "share of person-level:" "Pre-tax income + transfers")) xlabel(1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Std. dev. of log income", axis(1)) ytitle("Share of person-level dispersion (%)", axis(2)) yscale(range(0.2 0.4) axis(1)) yscale(range(0 0.1) axis(2)) ylabel(0.2(0.05)0.4, axis(1)) ylabel(0(2)9, axis(2)) legend(region(lc(white))) ysc(titleg(2))

*A.I (c)
use county_BEA, clear

foreach c in transf ssb medical inc_mant {
	
	gen `c'_pc = `c'/pop
	gen l`c'_pc = log(`c'_pc)
	
}

collapse (sd) ltransf_pc lssb_pc lmedical_pc linc_mant_pc, by(year)

twoway (connected ltransf_pc year, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lssb_pc year, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected lmedical_pc year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected linc_mant_pc year, mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)), legend(order(1 "Total transfers" 2 "Social Security" 3 "Medical benefits" 4 "Income maintenance benefits")) xlabel(1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("") ytitle("Std. dev. of log transfers") legend(region(lc(white)) rows(2)) ysc(titleg(2))

*A.I (d)
use county_SAIPE, clear

preserve

gen lmed_hh_inc = log(med_hh_inc)
collapse (sd) lmed_hh_inc, by(year)
tempfile hh
save `hh'

restore

foreach c in all u18 {

	gen tot_pov_`c'_comp = pop_`c' - tot_pov_`c'
	egen A_`c' = sum(tot_pov_`c'), by(year)
	egen B_`c' = sum(tot_pov_`c'_comp), by(year)
	gen term_sum_`c' = abs(tot_pov_`c'/A_`c' - tot_pov_`c'_comp/B_`c')

}
	
collapse (sum) term_sum_all term_sum_u18, by(year)

foreach c in all u18 {

	gen D_pov_`c' = 0.5*term_sum_`c'

}
	
merge 1:1 year using `hh'

twoway (connected lmed_hh_inc year, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected D_pov_all year, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected D_pov_u18 year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)), legend(order(1 "Median household income" 2 "Poverty rate (all)" 3 "Poverty rate (under 18)")) xlabel(1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("") legend(region(lc(white))) ytitle("Std. dev. of log income / Dissimilarity index") ysc(titleg(2))

*A.I (e)
use county_SAIPE, clear

preserve

keep if year == 1989 | year == 2018

xtile pov_rank_1989 = pov_rate_all if year == 1989, nq(100)
xtile pov_rank_2018 = pov_rate_all if year == 2018, nq(100)
gen pov_rank = pov_rank_1989 if year == 1989
replace pov_rank = pov_rank_2018 if year == 2018
replace pov_rank = 101 - pov_rank 

collapse (mean) pov_rate_all, by(year pov_rank)

twoway (scatter pov_rate pov_rank if year == 1989, mc(navy) msize(medsmall)) (scatter pov_rate pov_rank if year == 2018, mc(cranberry) ms(s) mfc(white) msize(medium)), graphregion(color(white)) legend(order(1 "1989" 2 "2018")) legend(region(lc(white))) ytitle("Poverty rate (%)") xtitle("Percentile (ranked from highest poverty to lowest poverty)") ysc(titleg(2))

*A.I (f)
use ASEC, clear

merge m:1 year using deflator_PCE
drop _merge

replace inctot = (100*inctot)/pcedeflator
collapse (sum) inctot (mean) asecwth t, by(hh statefip year)

*Compute V_naive
matrix V_naive = J(9,5,.)

preserve
collapse (p10) p_10 = inctot (p25) p_25 = inctot (p50) p_50 = inctot (p75) p_75 = inctot (p90) p_90 = inctot [pw = asecwth], by(t statefip)

foreach c in 10 25 50 75 90 {
	
	gen lp_`c' = log(p_`c')
	
}

forvalues i = 1(1)9 {
	
	qui sum lp_10 if t == `i' 
	matrix V_naive[`i',1] = r(Var)
	qui sum lp_25 if t == `i' 
	matrix V_naive[`i',2] = r(Var)
	qui sum lp_50 if t == `i' 
	matrix V_naive[`i',3] = r(Var)
	qui sum lp_75 if t == `i'
	matrix V_naive[`i',4] = r(Var)
	qui sum lp_90 if t == `i' 
	matrix V_naive[`i',5] = r(Var)
	
}
restore

*Compute V_noise
matrix V_noise = J(9,5,.)

local rep = 300
set seed 12345

forvalues i = 1(1)9 {
	
	matrix aux_`i' = J(51,5,.)
	
	local iter = 0
	local st = 0
	while `st' <= 55 {
		
		local iter = `iter' + 1
		local st = `st' + 1
		
		if `st' == 3 | `st' == 7 | `st' == 14 | `st' == 43 | `st' == 52 {
			
			local st = `st' + 1
			
		}
		
		preserve
		keep if t == `i' & statefip == `st'
		bootstrap stat1=log(r(p10)) stat2=log(r(p25)) stat3=log(r(p50)) stat4=log(r(p75)) stat5=log(r(p90)), reps(`rep'): summarize inctot, detail
		matrix aux_se = e(se)
		matrix aux_`i'[`iter',1] = aux_se[1,1]
		matrix aux_`i'[`iter',2] = aux_se[1,2]
		matrix aux_`i'[`iter',3] = aux_se[1,3]
		matrix aux_`i'[`iter',4] = aux_se[1,4]
		matrix aux_`i'[`iter',5] = aux_se[1,5]
		restore
	
	}
	
	preserve
	drop _all
	svmat aux_`i'
	
	gen we = (1/51)
		
	forvalues j = 1(1)5 {
		replace aux_`i'`j' = (aux_`i'`j')^2 
		gen aux_`i'`j'_w = aux_`i'`j'*we
		egen aux`j' = sum(aux_`i'`j'_w)
		qui sum aux`j'
		matrix V_noise[`i',`j'] = r(mean)
	}
	restore
	
}

*Compute bias-corrected std
matrix std_corr = J(9,5,.)

forvalues i = 1(1)9 {
	forvalues j = 1(1)5 {
		
		matrix std_corr[`i',`j'] = sqrt(V_naive[`i',`j'] - V_noise[`i',`j'])
		
	}
	
}

drop _all
svmat std_corr
gen t = _n

twoway (connected std_corr1 t, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected std_corr2 t, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected std_corr3 t, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected std_corr4 t, mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)) (connected std_corr5 t, mc(purple) lc(purple) ms(X) lpattern(longdash)), legend(order(1 "p10" 2 "p25" 3 "p50" 4 "p75" 5 "p90")) graphregion(color(white)) xtitle("") xlabel(1 "76-79" 2 "80-84" 3 "85-89" 4 "90-94" 5 "95-99" 6 "00-04" 7 "05-09" 8 "10-14" 9 "15-19") legend(region(lc(white)) rows(1)) ytitle("Std. dev. of log income percentile") ysc(titleg(2))

*Appendix II

*A.II (a) and (c)
use county_BEA, clear

gen post_tr = pre + transf

foreach c in pre post_tr {
    
	gen `c'_pc = `c'/pop
	gen l`c'_pc = log(`c'_pc)
	
}

preserve
collapse (sum) pop, by(year cr)
rename pop pop_cr
tempfile pop_cr
save `pop_cr'
restore

preserve
collapse (mean) pre_pc post_tr_pc [fw = pop], by(year cr)
gen lpre_pc = log(pre_pc)
gen lpost_tr_pc = log(post_tr_pc)
merge 1:1 year cr using `pop_cr'
collapse (sd) lpre_pc lpost_tr_pc [fw = pop_cr], by(year)
rename lpre_pc lpre_pc_bet
rename lpost_tr_pc lpost_tr_pc_bet
tempfile between_cr
save `between_cr'
restore

preserve

collapse (sd) lpre_pc lpost_tr_pc [fw = pop], by(year cr)

merge m:1 year using `between_cr'

foreach c in lpre_pc lpost_tr_pc lpre_pc_bet lpost_tr_pc_bet {

	gen aux1 = `c' if year == 1969
	egen aux2 = max(aux1), by(cr)
	gen `c'2 = `c' - aux2
	drop aux1 aux2
	
}


twoway (connected lpre_pc2 year if cr == "West", mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lpre_pc2 year if cr == "Midwest", mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected lpre_pc2 year if cr == "South", mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected lpre_pc2 year if cr == "Northeast", mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)) (connected lpre_pc_bet2 year if cr == "Northeast", mc(black) lc(black) ms(X) lpattern(longdash) msize(medium)), legend(order(1 "West" 2 "Midwest" 3 "South" 4 "Northeast" 5 "Between")) xlabel(1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Std. dev. of log income", axis(1)) legend(region(lc(white)) rows(2)) ysc(titleg(2))

twoway (connected lpost_tr_pc2 year if cr == "West", mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lpost_tr_pc2 year if cr == "Midwest", mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected lpost_tr_pc2 year if cr == "South", mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected lpost_tr_pc2 year if cr == "Northeast", mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)) (connected lpost_tr_pc_bet2 year if cr == "Northeast", mc(black) ms(X) lc(black) lpattern(longdash) msize(medium)), legend(order(1 "West" 2 "Midwest" 3 "South" 4 "Northeast" 5 "Between")) xlabel(1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Std. dev. of log income", axis(1)) legend(region(lc(white)) rows(2)) ysc(titleg(2))

restore

*A.II (b) and (d)
preserve
collapse (sum) pop, by(year cd)
rename pop pop_cd
tempfile pop_cd
save `pop_cd'
restore

preserve
collapse (mean) pre_pc post_tr_pc [fw = pop], by(year cd)
gen lpre_pc = log(pre_pc)
gen lpost_tr_pc = log(post_tr_pc)
merge 1:1 year cd using `pop_cd'
collapse (sd) lpre_pc lpost_tr_pc [fw = pop_cd], by(year)
rename lpre_pc lpre_pc_bet
rename lpost_tr_pc lpost_tr_pc_bet
tempfile between_cd
save `between_cd'
restore

preserve

collapse (sd) lpre_pc lpost_tr_pc [fw = pop], by(year cd)

merge m:1 year using "$dta\between_cd"

foreach c in lpre_pc lpost_tr_pc lpre_pc_bet lpost_tr_pc_bet {

	gen aux1 = `c' if year == 1969
	egen aux2 = max(aux1), by(cd)
	gen `c'2 = `c' - aux2
	drop aux1 aux2
	
}

twoway (connected lpre_pc2 year if cd == "Pacific", mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lpre_pc2 year if cd == "Mountain", mc(navy) lpattern(tight_dot) ms(o) msize(medsmall) mfc(white) lcolor(navy)) (connected lpre_pc2 year if cd == "West North Central", mc(cranberry) lc(cranberry) lpattern(solid) ms(s) msize(medsmall)) (connected lpre_pc2 year if cd == "East North Central", mc(cranberry) lc(cranberry) lpattern(tight_dot) ms(s) msize(medsmall) mfc(white)) (connected lpre_pc2 year if cd == "West South Central", mc(midgreen) lc(midgreen) lpattern(solid) ms(t) msize(medsmall)) (connected lpre_pc2 year if cd == "East South Central", mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(medsmall) mfc(white)) (connected lpre_pc2 year if cd == "South Atlantic", mc("252 186 3") lc("252 186 3") lpattern(solid) ms(d) msize(medsmall)) (connected lpre_pc2 year if cd == "Middle Atlantic", mc("252 186 3") lc("252 186 3") lpattern(tight_dot) ms(d) msize(medsmall) mfc(white)) (connected lpre_pc2 year if cd == "New England", mc(purple) ms(+) lpattern(solid) lcolor(purple)) (connected lpre_pc_bet2 year if cd == "New England", mc(black) ms(X) lcolor(black) lpattern(tight_dot) msize(medium)), legend(order(1 "Pacific" 2 "Mountain" 3 "West North Central" 4 "East North Central" 5 "West South Central" 6 "East South Central" 7 "South Atlantic" 8 "Middle Atlantic" 9 "New England" 10 "Between")) xlabel(1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Std. dev. of log income", axis(1)) legend(region(lc(white)) rows(5)) ysc(titleg(2))

twoway (connected lpost_tr_pc2 year if cd == "Pacific", mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lpost_tr_pc2 year if cd == "Mountain", mc(navy) lpattern(tight_dot) ms(o) msize(medsmall) mfc(white) lcolor(navy)) (connected lpost_tr_pc2 year if cd == "West North Central", mc(cranberry) lc(cranberry) lpattern(solid) ms(s) msize(medsmall)) (connected lpost_tr_pc2 year if cd == "East North Central", mc(cranberry) lc(cranberry) lpattern(tight_dot) ms(s) msize(medsmall) mfc(white)) (connected lpost_tr_pc2 year if cd == "West South Central", mc(midgreen) lc(midgreen) lpattern(solid) ms(t) msize(medsmall)) (connected lpost_tr_pc2 year if cd == "East South Central", mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(medsmall) mfc(white)) (connected lpost_tr_pc2 year if cd == "South Atlantic", mc("252 186 3") lc("252 186 3") lpattern(solid) ms(d) msize(medsmall)) (connected lpost_tr_pc2 year if cd == "Middle Atlantic", mc("252 186 3") lc("252 186 3") lpattern(tight_dot) ms(d) msize(medsmall) mfc(white)) (connected lpost_tr_pc2 year if cd == "New England", mc(purple) ms(+) lpattern(solid) lcolor(purple)) (connected lpost_tr_pc_bet2 year if cd == "New England", mc(black) ms(X) lcolor(black) lpattern(tight_dot) msize(medium)), legend(order(1 "Pacific" 2 "Mountain" 3 "West North Central" 4 "East North Central" 5 "West South Central" 6 "East South Central" 7 "South Atlantic" 8 "Middle Atlantic" 9 "New England" 10 "Between")) xlabel(1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Std. dev. of log income", axis(1)) legend(region(lc(white)) rows(5)) ysc(titleg(2))

restore

*A.II (e) and (f)
use county_SAIPE, clear

gen tot_pov_all_comp = pop_all - tot_pov_all

preserve
collapse (sum) tot_pov_all tot_pov_all_comp, by(year cr)

egen A = sum(tot_pov_all), by(year)
egen B = sum(tot_pov_all_comp), by(year)
gen term_sum = abs(tot_pov_all/A - tot_pov_all_comp/B)

collapse (sum) term_sum, by(year)
gen D_pov_bet = 0.5*term_sum

tempfile between_cr_pov
save `between_cr_pov'
restore

preserve

egen A = sum(tot_pov_all), by(year cr)
egen B = sum(tot_pov_all_comp), by(year cr)
gen term_sum = abs(tot_pov_all/A - tot_pov_all_comp/B)

collapse (sum) term_sum, by(year cr)
gen D_pov = 0.5*term_sum

merge m:1 year using `between_cr_pov'

foreach c in D_pov D_pov_bet {

	gen aux1 = `c' if year == 1989
	egen aux2 = max(aux1), by(cr)
	gen `c'2 = `c'/aux2
	drop aux1 aux2
	
}

twoway (connected D_pov2 year if cr == "West", mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected D_pov2 year if cr == "Midwest", mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected D_pov2 year if cr == "South", mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected D_pov2 year if cr == "Northeast", mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)) (connected D_pov_bet2 year if cr == "Northeast", mc(black) lc(black) ms(X) lpattern(longdash) msize(medium)), legend(order(1 "West" 2 "Midwest" 3 "South" 4 "Northeast" 5 "Between")) xlabel(1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Dissimilarity index", axis(1)) legend(region(lc(white)) rows(2)) ysc(titleg(2))

restore

preserve
collapse (sum) tot_pov_all tot_pov_all_comp, by(year cd)

egen A = sum(tot_pov_all), by(year)
egen B = sum(tot_pov_all_comp), by(year)
gen term_sum = abs(tot_pov_all/A - tot_pov_all_comp/B)

collapse (sum) term_sum, by(year)

gen D_pov_bet = 0.5*term_sum
tempfile between_cd_pov
save `between_cd_pov'
restore

preserve

egen A = sum(tot_pov_all), by(year cd)
egen B = sum(tot_pov_all_comp), by(year cd)
gen term_sum = abs(tot_pov_all/A - tot_pov_all_comp/B)

collapse (sum) term_sum, by(year cd)
gen D_pov = 0.5*term_sum

merge m:1 year using `between_cd_pov'

foreach c in D_pov D_pov_bet {

	gen aux1 = `c' if year == 1989
	egen aux2 = max(aux1), by(cd)
	gen `c'2 = `c'/aux2
	drop aux1 aux2
	
}

twoway (connected D_pov2 year if cd == "Pacific", mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected D_pov2 year if cd == "Mountain", mc(navy) lpattern(tight_dot) ms(o) msize(medsmall) mfc(white) lcolor(navy)) (connected D_pov2 year if cd == "West North Central", mc(cranberry) lc(cranberry) lpattern(solid) ms(s) msize(medsmall)) (connected D_pov2 year if cd == "East North Central", mc(cranberry) lc(cranberry) lpattern(tight_dot) ms(s) msize(medsmall) mfc(white)) (connected D_pov2 year if cd == "West South Central", mc(midgreen) lc(midgreen) lpattern(solid) ms(t) msize(medsmall)) (connected D_pov2 year if cd == "East South Central", mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(medsmall) mfc(white)) (connected D_pov2 year if cd == "South Atlantic", mc("252 186 3") lc("252 186 3") lpattern(solid) ms(d) msize(medsmall)) (connected D_pov2 year if cd == "Middle Atlantic", mc("252 186 3") lc("252 186 3") lpattern(tight_dot) ms(d) msize(medsmall) mfc(white)) (connected D_pov2 year if cd == "New England", mc(purple) ms(+) lpattern(solid) lcolor(purple)) (connected D_pov_bet2 year if cd == "New England", mc(black) ms(X) lcolor(black) lpattern(tight_dot) msize(medium)), legend(order(1 "Pacific" 2 "Mountain" 3 "West North Central" 4 "East North Central" 5 "West South Central" 6 "East South Central" 7 "South Atlantic" 8 "Middle Atlantic" 9 "New England" 10 "Between")) xlabel(1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Dissimilarity index", axis(1)) legend(region(lc(white)) rows(5)) ysc(titleg(2))

restore

*Appendix III

*A.III (a)
use county_CENSUS, clear

gen tot_pov_comp = pop - tot_pov
egen A = sum(tot_pov), by(year)
egen B = sum(tot_pov_comp), by(year)
gen term_sum = abs(tot_pov/A - tot_pov_comp/B)

collapse (sum) term_sum, by(year)

gen D_pov = 0.5*term_sum

twoway (connected D_pov year, mc(navy) lpattern(solid) ms(o) msize(medsmall)), xlabel(1960 1970 1980 1990 2000 2010) graphregion(color(white)) xtitle("")  ytitle("Dissimilarity index", axis(1)) ysc(titleg(2))

*A.III (b)
use county_SAIPE, clear

keep if inrange(year,2016,2018)

collapse (mean) pov_rate_all pov_rate_u18 pop_all pop_u18, by(county state)
replace pop_all = round(pop_all)
replace pop_u18 = round(pop_u18)
gen year = 2017

append using county_estCNALL1989

foreach c in all u18 {
	
	preserve 

	xtile pov_rank_1990_`c' = pov_rate_`c' [fw = pop_`c'] if year == 1990, nq(10)
	egen county_id = group(county state)
	egen pov_rank_`c' = max(pov_rank_1990_`c'), by(county_id)
	replace pov_rank_`c' = 11 - pov_rank_`c'

	collapse (mean) pov_rate_`c' [fw = pop_`c'], by(year pov_rank_`c')
	replace pov_rate_`c' = pov_rate_`c'*100
	
	rename pov_rank_`c' decile
	tempfile aux_pov_`c'
	save `aux_pov_`c''
	
	restore

}

use `aux_pov_all', clear
merge 1:1 year decile using `aux_pov_u18'
drop _merge
	
gen t = 1 if year == 1990
replace t = 2 if year == 2017
xtset decile t

foreach c in all u18  {
	
	gen gr_pov_`c' = log(pov_rate_`c') - log(l.pov_rate_`c')
	replace gr_pov_`c' = 100*gr_pov_`c'
	
}

drop if decile == .	

twoway (scatter gr_pov_all decile if year == 2017, mc(navy) ms(o) msize(medium)) (scatter gr_pov_u18 decile if year == 2017, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) mfc(white) msize(medium)), legend(order(1  "Poverty rate (all)" 2 "Poverty rate (under 18)")) graphregion(color(white)) ytitle("Percent change in poverty rate" "from 1989 to 2016-2018 (%)") xtitle("County decile (ranked form highest 1989 poverty to lowest 1989 poverty)") ysc(titleg(2)) xlabel(1(1)10) legend(region(lc(white)) rows(1)) 

*A.III (c)

use county_SAIPE, clear

preserve
collapse (mean) pov_rate_all [fw = pop_all], by(year)
rename pov_rate pov_rate_US
tempfile pov_US
save `pov_US'
restore

collapse (mean) pov_rate_all [fw = pop_all], by(cr year)
merge m:1 year using `pov_US'

twoway (connected pov_rate_all year if cr == "West", mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected pov_rate_all year if cr == "Midwest", mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected pov_rate_all year if cr == "South", mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected pov_rate_all year if cr == "Northeast", mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)) (connected pov_rate_US year if cr == "Northeast", mc(black) lc(black) ms(X) lpattern(longdash) msize(medium)), legend(order(1 "West" 2 "Midwest" 3 "South" 4 "Northeast" 5 "National")) xlabel(1990 1995 2000 2005 2010 2015) graphregion(color(white)) xtitle("")  ytitle("Poverty rate (%)", axis(1)) legend(region(lc(white)) rows(2)) ysc(titleg(2))

*Appendix IV

*A.IV (a) and (b)
use county_SAIPE, clear

keep if year == 1989 | year == 2018

preserve

xtile pov_rank_1989 = pov_rate_all [fw = pop_all] if year == 1989, nq(10)
xtile pov_rank_2018 = pov_rate_all [fw = pop_all] if year == 2018, nq(10)
gen pov_rank = pov_rank_1989 if year == 1989
replace pov_rank = pov_rank_2018 if year == 2018
replace pov_rank = 11 - pov_rank 

collapse (mean) pov_rate_all [fw = pop_all], by(year pov_rank)

twoway (scatter pov_rate pov_rank if year == 1989, mc(navy) msize(medsmall)) (scatter pov_rate pov_rank if year == 2018, mc(cranberry) ms(s) mfc(white) msize(medium)), graphregion(color(white)) legend(order(1 "1989" 2 "2018")) legend(region(lc(white))) ytitle("Poverty rate (%)") xtitle("Decile (ranked from highest poverty to lowest poverty)") ysc(titleg(2)) xlabel(1(1)10) 

restore

xtile pov_rank_1989 = pov_rate_u18 [fw = pop_u18] if year == 1989, nq(10)
xtile pov_rank_2018 = pov_rate_u18 [fw = pop_u18] if year == 2018, nq(10)
gen pov_rank = pov_rank_1989 if year == 1989
replace pov_rank = pov_rank_2018 if year == 2018
replace pov_rank = 11 - pov_rank 

collapse (mean) pov_rate_u18 [fw = pop_u18], by(year pov_rank)

twoway (scatter pov_rate pov_rank if year == 1989, mc(navy) msize(medsmall)) (scatter pov_rate pov_rank if year == 2018, mc(cranberry) ms(s) mfc(white) msize(medium)), graphregion(color(white)) legend(order(1 "1989" 2 "2018")) legend(region(lc(white))) ytitle("Poverty rate (%)") xtitle("Decile (ranked from highest poverty to lowest poverty)") ysc(titleg(2)) xlabel(1(1)10) 

*Appendix V

use state_SP, clear

foreach c in p90 p95 p99 p999 {
    
	gen l`c' = log(`c')
	
}

collapse (sd) lp90 lp95 lp99 lp999 [fw = taxpayers], by(year)

twoway (connected lp90 year, mc(navy) lpattern(solid) ms(o) msize(medsmall)) (connected lp95 year, mc(cranberry) lc(cranberry) lpattern(vshortdash) ms(s) msize(small)) (connected lp99 year, mc(midgreen) lc(midgreen) lpattern(tight_dot) ms(t) msize(small)) (connected lp999 year, mc("252 186 3") lc("252 186 3") lpattern(shortdash_dot_dot) ms(d) msize(small)), legend(order(1 "p90" 2 "p95" 3 "p99" 4 "p99.9")) graphregion(color(white)) xtitle("") legend(region(lc(white)) rows(1)) xlabel(1960 1965 1970 1975 1980 1985 1990 1995 2000 2005 2010 2015) ytitle("Std. dev. of log income percentile") ysc(titleg(2))