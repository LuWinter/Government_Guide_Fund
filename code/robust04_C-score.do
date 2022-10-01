
cd "~/Documents/R_Projects/Government_Guide_Fund/output"

* clear
* use "merged_for_reg_reduced_19796.dta"

destring Stkcd, replace
xtset Stkcd Year

gen IndustryCode2 = cond(substr(IndustryCode, 1, 1) != "C", substr(IndustryCode, 1, 1), substr(IndustryCode, 1, 2))
egen Industry = group(IndustryCode2)

egen Province2 = group(Province)
rename Province Province_str
rename Province2 Province


winsor2 Ret Size Lev GDP_p Age MHRatio, cuts(1 99) by(Year) trim



/******************************* Macros Defination ****************************/

// Control Variable Macros
global control_vari "Size Lev MHRatio Age GDP_p"

// Fixed Effect Macros
global common_fe "i.Year i.Industry i.Province"
global high_fe "i.Year#i.Province"


/********************************* Perform Test ******************************/

#delimit ;
eststo fe_simple:
	quietly reghdfe C_Score GGF, 
	absorb($common_fe)
	  ;
#delimit cr
quietly estadd local control "NO", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "NO", replace
quietly estadd local fe_prov_year "NO", replace


#delimit ;
eststo fe_control:
	quietly reghdfe C_Score GGF $control_vari, 
	absorb($common_fe)
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "NO", replace
quietly estadd local fe_prov_year "NO", replace

#delimit ;
eststo high_fe_control:
	quietly reghdfe C_Score GGF $control_vari, 
	absorb($common_fe $high_fe)
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace


#delimit ;
eststo cluster_fe_control:
	quietly reghdfe C_Score GGF $control_vari, 
	absorb($common_fe) vce(cl Industry)
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "NO", replace
quietly estadd local fe_prov_year "NO", replace


/*************************** Output Regression Result *************************/

global var_list "GGF Size Lev MHRatio Age GDP_p"

#delimit ;                               
esttab fe_simple fe_control high_fe_control cluster_fe_control,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"));
#delimit cr



