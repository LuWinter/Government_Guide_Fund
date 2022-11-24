
/******************* Content *******************
****** 0. Test Regression				  ******
****** 1. Base Regression				  ******
****** 2. Government Level                ******
****** 3. Holding Ratio & Same Province   ******
****** 4. Dispatch Directors			  ******
****** 5. Risk Adversion                  ******
***********************************************/


/******************************** Preprocession ********************************/

cd "~/Documents/R_Projects/Government_Guide_Fund/output/tables"

destring Stkcd, replace
xtset Stkcd Year

gen EPS_P = EPS / YearOpen
gen IndustryCode2 = cond(substr(IndustryCode, 1, 1) != "C", substr(IndustryCode, 1, 1), substr(IndustryCode, 1, 2))
egen Industry = group(IndustryCode2)

egen Province2 = group(Province)
rename Province Province_str
rename Province2 Province
gen same_province = strmatch(GGFProvince, Province_str)

winsor2 Size Lev MHRatio RDRatio GDP_p INS Age SuperINS, cuts(1 99) by(Year) trim replace


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

// Fixed Effect Macros
global common_fe "i.Year i.Industry i.Province"
global high_fe "i.Year#i.Industry i.Year#i.Province"


/******************************* 0. Test Regression ****************************/
#delimit ;
eststo test_size:
	quietly reghdfe EPS_P $base_reg $Size_reg,
	absorb($common_fe $high_fe)
	vce(cl Stkcd);
#delimit cr

#delimit ;
eststo test_lev:
	quietly reghdfe EPS_P $base_reg $Lev_reg,
	absorb($common_fe $high_fe)
	vce(cl Stkcd);
#delimit cr

#delimit ;
eststo test_age:
 	quietly reghdfe EPS_P $base_reg $Age_reg,
	absorb($common_fe $high_fe)
	vce(cl Stkcd);
#delimit cr

#delimit ;
eststo test_mhratio:
 	quietly reghdfe EPS_P $base_reg $MHRatio_reg,
	absorb($common_fe $high_fe)
	vce(cl Stkcd);
#delimit cr

#delimit ;                               
esttab test*          
    using main-analysis00_test-regression.rtf      ,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
	varwidth(15) b t(4) ar2(4) nobaselevels
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Obs" "adjusted-R2"))	
    mtitle("Size" "Lev" "Age" "MHRatio");
#delimit cr


/******************************* 1. Base Regression ****************************/
#delimit ;
eststo simple_ols:
	quietly reg EPS_P $base_reg $GGF_reg
	;
#delimit cr
quietly estadd local fe_industry "NO", replace
quietly estadd local fe_year "NO", replace
quietly estadd local fe_province "NO", replace
quietly estadd local fe_indu_year "NO", replace
quietly estadd local fe_prov_year "NO", replace

#delimit ;
eststo simple_fe:
	quietly reghdfe EPS_P $base_reg $GGF_reg, 
	absorb($common_fe)
	;
#delimit cr
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "NO", replace
quietly estadd local fe_prov_year "NO", replace

#delimit ;
eststo control_ols:
    quietly reg EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg,
	;
#delimit cr
quietly estadd local fe_industry "NO", replace
quietly estadd local fe_year "NO", replace
quietly estadd local fe_province "NO", replace
quietly estadd local fe_indu_year "NO", replace
quietly estadd local fe_prov_year "NO", replace
    
#delimit ;
eststo simple_high_fe:
	quietly reghdfe EPS_P $base_reg $GGF_reg, 
	absorb($common_fe $high_fe)
    ;
#delimit cr
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace

#delimit ;
eststo control_high_fe:
	quietly reghdfe EPS_P $base_reg $GGF_reg
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg, 
	absorb($common_fe $high_fe)
	;
#delimit cr
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace


/******************************* 2. Government Level ****************************/                    
tab GGFLevel

#delimit ;
eststo only_country_level:
	quietly reghdfe EPS_P $base_reg $GGF_reg  
    if GGFLevel == "国家级" | GGFLevel == "NA", 
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
eststo no_country_level:
	quietly reghdfe EPS_P $base_reg $GGF_reg   
    if GGFLevel != "国家级" | GGFLevel == "NA", 
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
eststo only_country_level_control:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg 
    if GGFLevel == "国家级" | GGFLevel == "NA", 
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
eststo no_country_level_control:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if GGFLevel != "国家级" | GGFLevel == "NA", 
	absorb($common_fe $high_fe)   
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace

           
/************************** 3. Holding Ratio & Same Province ***********************/

#delimit ;
eststo no_minority_holder:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if (HoldRank <= 5 | HoldRank == .),   
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
eststo minority_holder:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if (HoldRank > 5 | HoldRank == .),        
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
eststo same_province:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if (same_province == 1 | GGF == 0),        
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
eststo no_same_province:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if same_province != 1,     
    absorb($common_fe $high_fe)  
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace


/******************************* 4. Dispatch Directors ****************************/
tab Related

#delimit ;
eststo dispatch_director:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
    if Related > 0 | GGF == 0, 
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
eststo dispatch_director_control:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg 
    if Related > 0 | GGF == 0, 
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
eststo no_director:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
    if Related == 0 | GGF == 0, 
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
eststo no_director_control:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if Related == 0 | GGF == 0, 
	absorb($common_fe $high_fe)   
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace


/******************************* 5. Risk Aversion ****************************/
global Risk_reg "Risk c.Risk#DR c.Risk#c.Ret c.Risk#DR#c.Ret"

gen Risk = sdROA3 * 100
#delimit ;
eststo risk_roa:
	reghdfe EPS_P $base_reg  $Risk_reg
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if GGF == 1,        
    absorb(i.Stkcd i.Year)   
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_stkcd "YES", replace
quietly estadd local fe_year "YES", replace

drop Risk 
gen Risk = sdROE3 * 100
#delimit ;
eststo risk_roe:
	quietly reghdfe EPS_P $base_reg  $Risk_reg
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if GGF == 1,        
    absorb(i.Stkcd i.Year)   
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_stkcd "YES", replace
quietly estadd local fe_year "YES", replace



/******************************* Output Regression Result ****************************/

global var_list "_cons DR Ret 1.DR#c.Ret GGF 0.GGF#1.DR 1.GGF#c.Ret 1.GGF#1.DR#c.Ret"
global var_list2 "_cons DR Ret 1.DR#c.Ret Risk 1.DR#c.Risk c.Risk#c.Ret 1.DR#c.Risk#c.Ret"


#delimit ;                               
esttab simple_ols simple_fe simple_high_fe control_ols control_high_fe           
    using main-analysis01_base-regression.rtf, 
	replace baselevel label nogap star(* 0.10 ** 0.05 *** 0.01) 
    varwidth(15) b t(4) ar2(4) 
	s(fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))	
    mtitle("Simple OLS" "Simple FE" "Simple High FE" "Control OLS" "Control High FE");
#delimit cr

#delimit ;           
esttab no_country*   only_country*               
    using main-analysis02_country-level.rtf, 
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))	
    mtitle("No Country GGF" "No Country GGF (Control)" "Only Country GGF" "Only Country GGF (Control)");
#delimit cr

#delimit ;
esttab no_minority_holder minority_holder same_province no_same_province       
    using main-analysis03_holding-ratio-and-same-province.rtf, 
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))	
    mtitle("No Minority" "Only Minority" "Only Same Province" "No Same Province");
#delimit cr

#delimit ;           
esttab dispatch_director* no_director*              
    using main-analysis04_dispatch-directors.rtf, 
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))	
    mtitle("Dispatch Director" "Dispatch Director CT" "No Director" "No Director CT");
#delimit cr

#delimit ;
esttab risk_roa risk_roe               
    using main-analysis05_risk-aversion.rtf         ,
	replace label nogap star(* 0.10 ** 0.05 *** 0.01)
    keep($var_list2) varwidth(15) b t(4) ar2(4) 
	s(control fe_stkcd fe_year N r2_a, 
	  label("Control Variables" "Stkcd FE" "Year FE" "Obs" "adjusted-R2"))	
    mtitle("ROA" "ROE");
#delimit cr

