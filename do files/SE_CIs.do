*** Calculating SE and CI ***


* Set the characteristics of the survey: PSU and Strata
svyset hv001 [pw = weight], strata(hv022)

* Incidence of Poverty (H)
* For details and discussions see equations (8.13) and (8.31), chapter 8 of OPHI book
forvalue k = 10(10)100 {
	svy: mean multid_poor_`k' 
	gen se_H_`k' = (_se[multid_poor_`k'])
	gen lb_H_`k' = _b[multid_poor_`k'] - 1.96 * se_H_`k'
	gen ub_H_`k' = _b[multid_poor_`k'] + 1.96 * se_H_`k'
	}
sum multid_poor_* lb_H_* ub_H_* [aw = weight]

* Adjusted Headcount Ratio (M0)
* For details and discussions see equations (8.11) and (8.30), chapter 8 of OPHI book
forvalue k = 10(10)100 {
	svy: mean cens_c_vector_`k'
	gen se_M0_`k' = (_se[cens_c_vector_`k'])
	gen lb_M0_`k' = _b[cens_c_vector_`k'] - 1.96 * se_M0_`k'
	gen ub_M0_`k' = _b[cens_c_vector_`k'] + 1.96 * se_M0_`k'
	}
sum cens_c_vector_* lb_M0_* ub_M0_* [aw= weight]

* Average Deprivation among the Poor (A)
* For details and discussions see equations (8.19), (8.35) and (8.36), chapter 8 of OPHI book
forvalue k = 10(10)100 {
	svy: mean multid_poor_`k' cens_c_vector_`k'
	mat cov = e(V)
	loc cov = cov[2,1]
	loc var_H = cov[1,1]
	loc var_MPI = cov[2,2]
	* Standard errors for Q
	gen se_A_`k' = ((`var_MPI'/_b[multid_poor_`k']^2) + (((_b[cens_c_vector_`k']/_b[multid_poor_`k']^2)^2)*(`var_H')) ///
	- 2*((_b[cens_c_vector_`k']/_b[multid_poor_`k']^3)*`cov'))^0.5
	* Lower and Upper limit
	gen lb_A_`k' = (_b[cens_c_vector_`k']/_b[multid_poor_`k']) - 1.96 * se_A_`k'
	gen ub_A_`k' = (_b[cens_c_vector_`k']/_b[multid_poor_`k']) + 1.96 * se_A_`k'
	}

	
	
	**************************************************************************************************
	**************************************************************************************************
	**************************************************************************************************
	**************************************************************************************************
	
	* more complex version 
*glo kcutoffs ".3333 .1 .20 .40 .50 .60 .70 .80 .90"	// choosing k-cutoffs for M0, H, A

glo kcutoffs ".3333"	// choosing k-cutoffs for M0, H, A
		
foreach k of numlist $kcutoffs {
		loc kc = subinstr("`=strofreal(`k',"%4.2f")'","0.","",.)				// clean k (robust)

* create poverty status and censored counting vector
		gen byte ps_`kc' = (c_vector >= `k') if c_vector < .
		gen	ci_`kc' = c_vector
		replace ci_`kc' = 0 if ps_`kc' == 0
	
}
*

gen district_num = district



foreach k of numlist $kcutoffs {
			loc kc = subinstr("`=strofreal(`k',"%4.2f")'","0.","",.)				// clean k (robust)

			eststo M0_`kc' : svy: mean ci_`kc', over(district)
			estadd loc measure "M0"
				
			eststo H_`kc' :  svy: mean ps_`kc', over(district)
			estadd loc measure "H"
		
			count if ps_`kc' == 1 
			if `r(N)' > 0 {
				eststo A_`kc': svy , subpop(ps_`kc'): mean ci_`kc', over(district) 
				estadd loc measure "A"
				
			}
			
estadd sca k = `kc' : *_`kc'
			

*
* indicator lists 
glo dim_health "d_cm d_nutr "
glo dim_educ "d_satt d_educ "
glo dim_livst "d_elct d_sani d_wtr d_hsg d_ckfl d_asst"
glo depind "$dim_health $dim_educ $dim_livst"

glo wgtscheme 1 			// 1 = eqlnest; 2 = health50; 3 = educ50; 4 = livst50


	*************************************************************
	*** Setting the weights conditional on missing indicators ***
	*************************************************************

	di "Note: setting the weights now..."
	loc indlist = ""						// collect all missing indicators across dim
	
	*** Education ***
	loc educind "$dim_educ"
	foreach var of varlist $dim_educ {
	capture drop w_`var' 
	loc indlist "`indlist' `var'"
		gen w_`var' = 1/6
	}
	*
	*** Health *** 
	loc healthind "$dim_health"
	foreach var of varlist `healthind'  {
	capture drop w_`var' 
			loc indlist "`indlist' `var'"
			gen w_`var' = 1/6
	}
	
	*** Living Standards ***

	foreach var of varlist $dim_livst  {
			loc indlist "`indlist' `var'"
				capture drop w_`var' 
			gen w_`var' = 1/18
		}
		
di "our list: `indlist'"				// for debug

		* censored headcounts
	foreach k of numlist $kcutoffs {
		loc kc = subinstr("`=strofreal(`k',"%4.2f")'","0.","",.)				// clean k (robust)
		foreach v of varlist `indlist' {
			gen byte c`v'_`kc' = `v'
			replace c`v'_`kc' = 0 if ps_`kc' == 0
		}
	}
*
			* dimension-specific
			foreach k of numlist $kcutoffs {										 
				loc kc = subinstr("`=strofreal(`k',"%4.2f")'","0.","",.)		// clean k (robust)
				
				foreach v of varlist `indlist' {
					
					* cens headcounts
					eststo hd`kc'_`v' : svy : mean c`v'_`kc' , over(district)
					
						estadd loc indicator = "`v'"
						estadd loc measure "hdk"
				}
				estadd sca k = `kc' , replace : *`kc'* 
			}
