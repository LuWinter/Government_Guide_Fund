
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


winsor2 Accrual NI_lag1 CFO Size Lev GDP_p Age MHRatio, cuts(1 99) by(Year) trim



/******************************* Macros Defination ****************************/

// Control Variable Macros
global base_reg "DNI_lag1 NI_lag1 DNI_lag1#c.NI_lag1"
global GGF_reg "GGF GGF#DNI_lag1 GGF#c.NI_lag1 GGF#DNI_lag1#c.NI_lag1"
global Size_reg "Size c.Size#DNI_lag1 c.Size#c.NI_lag1 c.Size#DNI_lag1#c.NI_lag1"
global Lev_reg "Lev c.Lev#DNI_lag1 c.Lev#c.NI_lag1 c.Lev#DNI_lag1#c.NI_lag1"
global GDP_reg "GDP_p c.GDP_p#DNI_lag1 c.GDP_p#c.NI_lag1 c.GDP_p#DNI_lag1#c.NI_lag1"
global MHRatio_reg "MHRatio c.MHRatio#DNI_lag1 c.MHRatio#c.NI_lag1 c.MHRatio#DNI_lag1#c.NI_lag1"
global Age_reg "Age c.Age#DNI_lag1 c.Age#c.NI_lag1 c.Age#DNI_lag1#c.NI_lag1"

global base_reg2 "DCFO CFO DCFO#c.CFO"
global GGF_reg2 "GGF GGF#DCFO GGF#c.CFO GGF#DCFO#c.CFO"
global Size_reg2 "Size c.Size#DCFO c.Size#c.CFO c.Size#DCFO#c.CFO"
global Lev_reg2 "Lev c.Lev#DCFO c.Lev#c.CFO c.Lev#DCFO#c.CFO"
global GDP_reg2 "GDP_p c.GDP_p#DCFO c.GDP_p#c.CFO c.GDP_p#DCFO#c.CFO"
global MHRatio_reg2 "MHRatio c.MHRatio#DCFO c.MHRatio#c.CFO c.MHRatio#DCFO#c.CFO"
global Age_reg2 "Age c.Age#DCFO c.Age#c.CFO c.Age#DCFO#c.CFO"

// Fixed Effect Macros
global common_fe "i.Year i.Industry i.Province"
global high_fe "i.Year#i.Province"


/********************************* Perform Test ******************************/

#delimit ;
eststo fe_simple:
	reghdfe NI $base_reg $GGF_reg, 
	absorb($common_fe $high_fe)
	  ;
#delimit cr
quietly estadd local control "NO", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace


#delimit ;
eststo fe_control:
	quietly reghdfe NI $base_reg $GGF_reg
	$Size_reg $Lev_reg $GDP_reg $MHRatio_reg $Age_reg, 
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
	quietly reghdfe NI $base_reg $GGF_reg
	$Size_reg $Lev_reg $GDP_reg $MHRatio_reg $Age_reg, 
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
eststo fe_simple2:
	reghdfe Accrual $base_reg2 $GGF_reg2, 
	absorb($common_fe $high_fe)
	  ;
#delimit cr
quietly estadd local control "NO", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace


#delimit ;
eststo fe_control2:
	quietly reghdfe Accrual $base_reg2 $GGF_reg2
	$Size_reg2 $Lev_reg2 $GDP_reg2 $MHRatio_reg2 $Age_reg2, 
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
eststo high_fe_control2:
	quietly reghdfe Accrual $base_reg2 $GGF_reg2
	$Size_reg2 $Lev_reg2 $GDP_reg2 $MHRatio_reg2 $Age_reg2, 
	absorb($common_fe $high_fe)
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace


/*************************** Output Regression Result *************************/

global var_list "_cons DNI_lag1 NI_lag1 1.DNI_lag1#c.NI_lag1 GGF 0.GGF#1.DNI_lag1 1.GGF#c.NI_lag1 1.GGF#1.DNI_lag1#c.NI_lag1"

#delimit ;                               
esttab fe_simple fe_control high_fe_control
	using robust04_basu-model.rtf,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"));
#delimit cr


global var_list2 "_cons DCFO CFO 1.DCFO#c.CFO GGF 0.GGF#1.DCFO 1.GGF#c.CFO 1.GGF#1.DCFO#c.CFO"

#delimit ;                               
esttab fe_simple2 fe_control2 high_fe_control2
	using robust04_ball-model.rtf,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    keep($var_list2) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"));
#delimit cr

