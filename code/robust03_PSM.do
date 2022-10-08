
cd "~/Documents/R_Projects/Government_Guide_Fund/output/tables"

*clear
*use "PSM-DID_2022-10-06.dta"

destring Stkcd, replace
xtset Stkcd Year

gen EPS_P = EPS / YearOpen

egen Province2 = group(Province)
rename Province Province_str
rename Province2 Province
egen Industry2 = group(Industry)
drop Industry
rename Industry2 Industry

gen Ret_DR = DR * Ret
gen Post_DR = Post * DR
gen Post_Ret = Post * Ret
gen Post_Ret_DR = Post * Ret * DR
gen Treat_DR = Treat * DR
gen Treat_Ret = Treat * Ret
gen Treat_Ret_DR = Treat * Ret * DR
gen Post_Treat_Ret = Post_Treat * Ret
gen Post_Treat_DR = Post_Treat * DR
gen Post_Treat_Ret_DR = Post_Treat * Ret * DR


/******************************* Macros Defination ****************************/

// Control Variable Macros
global vars "EPS_P Ret DR Ret_DR Post_DR Post Post_Ret Post_Ret_DR"
global vars2 "EPS_P Ret DR Ret_DR Treat_DR Treat Treat_Ret Treat_Ret_DR"

// Fixed Effect Macros
global common_fe "i.Year i.Industry i.Province"
global high_fe "i.Year#i.Province i.Year#i.Industry"


/********************************* Perform Test ******************************/

eststo x1: reghdfe $vars if Treat == 0, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

eststo x2: reghdfe $vars if Treat == 1, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

eststo x3: reghdfe $vars2 if Post == 0, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

eststo x4: reghdfe $vars2 if Post == 1, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

*bdiff, group(Treat) model(reg $vars) surtest
   
#delimit ;
eststo did:
    reghdfe EPS_P Ret DR Ret_DR 
    Post Post_Ret Post_DR Post_Ret_DR
    Treat Treat_Ret Treat_DR Treat_Ret_DR
    Post_Treat Post_Treat_Ret Post_Treat_DR Post_Treat_Ret_DR
    , absorb($common_fe);
#delimit cr
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"


/*************************** Output Regression Result *************************/

global var_list "Post Post_Ret_DR Treat Treat_Ret_DR Post_Treat Post_Treat_Ret_DR"

#delimit ;                               
esttab x1 x2 x3 x4 did
	using robust03_PSM-DID.rtf,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    varwidth(15) b t(4) ar2(4) 
	s(fe_industry fe_year fe_province N r2_a, 
	    label("Industry FE" "Year FE" "Province FE" "Obs" "adjusted-R2"));
#delimit cr

