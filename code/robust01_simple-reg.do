
cd "~/Documents/R_Projects/Government_Guide_Fund/output/tables"

* clear
* use "merged_for_reg_reduced_19796.dta"

destring Stkcd, replace
xtset Stkcd Year

gen EPS_P = EPS / YearOpen
gen IndustryCode2 = cond(substr(IndustryCode, 1, 1) != "C", substr(IndustryCode, 1, 1), substr(IndustryCode, 1, 2))
egen Industry = group(IndustryCode2)

egen Province2 = group(Province)
rename Province Province_str
rename Province2 Province


winsor2 Size Lev GDP_p MHRatio Age, cuts(1 99) by(Year) replace



/******************************* Macros Defination ****************************/

// Control Variable Macros
global base_reg "DR Ret DR#c.Ret"
global GGF_reg "GGF GGF#DR GGF#c.Ret GGF#DR#c.Ret"
global Size_reg "Size c.Size#DR c.Size#c.Ret c.Size#DR#c.Ret"
global MB_reg "MB c.MB#DR c.MB#c.Ret c.MB#DR#c.Ret"
global Lev_reg "Lev c.Lev#DR c.Lev#c.Ret c.Lev#DR#c.Ret"
global Big4_reg "Big4 Big4#DR Big4#c.Ret Big4#DR#c.Ret"
global SOE_reg "SOE SOE#DR SOE#c.Ret SOE#DR#c.Ret"
global GDP_reg "GDP_p c.GDP_p#DR c.GDP_p#c.Ret c.GDP_p#DR#c.Ret"
global SuperINS_reg "SuperINS c.SuperINS#DR c.SuperINS#c.Ret c.SuperINS#DR#c.Ret"
global MHRatio_reg "MHRatio c.MHRatio#DR c.MHRatio#c.Ret c.MHRatio#DR#c.Ret"
global Age_reg "Age c.Age#DR c.Age#c.Ret c.Age#DR#c.Ret"
global CG_reg "CG c.CG#DR c.CG#c.Ret c.CG#DR#c.Ret"

// Fixed Effect Macros
global common_fe "i.Year i.Industry i.Province"
global high_fe "i.Year#i.Province i.Year#i.Industry"
global high_fe2 "i.Year#i.Province"


/********************************* Perform Test ******************************/

#delimit ;
eststo fe_simple:
	quietly reghdfe EPS_P $base_reg $GGF_reg, 
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
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg, 
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
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg, 
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
eststo high_fe_control2:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg $CG_reg, 
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
quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg $CG_reg, 
	absorb($common_fe $high_fe)
    ;
#delimit cr


/*************************** Output Regression Result *************************/

global var_list "DR Ret 1.DR#c.Ret GGF 0.GGF#1.DR 1.GGF#c.Ret 1.GGF#1.DR#c.Ret"

#delimit ;                               
esttab fe_simple fe_control high_fe_control high_fe_control2
	/* using robust01_simple-reg.rtf */,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"));
#delimit cr



