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


/***************************** Generate New Variable **************************/

gen Post = 0
replace Post = 1 if Year >= 2018
tab Post GGF

gen Ret_DR = DR * Ret
gen Post_DR = Post * DR
gen Post_Ret = Post * Ret
gen Post_Ret_DR = Post * Ret * DR
gen GGF_DR = GGF * DR
gen GGF_Ret = GGF * Ret
gen GGF_Ret_DR = GGF * Ret * DR
gen Post_GGF = Post * GGF
gen Post_GGF_Ret = Post * GGF * Ret
gen Post_GGF_DR = Post * GGF * DR
gen Post_GGF_Ret_DR = Post * GGF * Ret * DR

// Fixed Effect Macros
global common_fe "i.Year i.Industry i.Province"


/********************************* Perform Test ******************************/

#delimit ;
eststo only_country_level:
    reghdfe EPS_P DR Ret Ret_DR 
    Post Post_DR Post_Ret Post_Ret_DR
    GGF GGF_DR GGF_Ret GGF_Ret_DR
    Post_GGF Post_GGF_DR Post_GGF_Ret Post_GGF_Ret_DR
    if GGFLevel == "国家级" | GGFLevel == "NA"
    , absorb($common_fe);
#delimit cr
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

#delimit ;
eststo no_country_level:
    reghdfe EPS_P DR Ret Ret_DR 
    Post Post_DR Post_Ret Post_Ret_DR
    GGF GGF_DR GGF_Ret GGF_Ret_DR
    Post_GGF Post_GGF_DR Post_GGF_Ret Post_GGF_Ret_DR
    if GGFLevel != "国家级" | GGFLevel == "NA"
    , absorb($common_fe);
#delimit cr
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

#delimit ;
eststo total_sample:
    reghdfe EPS_P DR Ret Ret_DR 
    Post Post_DR Post_Ret Post_Ret_DR
    GGF GGF_DR GGF_Ret GGF_Ret_DR
    Post_GGF Post_GGF_DR Post_GGF_Ret Post_GGF_Ret_DR
    , absorb($common_fe);
#delimit cr
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"


/*************************** Output Regression Result *************************/

global var_list "Post Post_Ret_DR GGF GGF_Ret_DR Post_GGF Post_GGF_Ret_DR"

#delimit ;                               
esttab only_country_level no_country_level total_sample
	/* using further-analysis06_AM-new-rule.rtf */,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    varwidth(15) b t(4) ar2(4) 
	s(fe_industry fe_year fe_province N r2_a, 
	    label("Industry FE" "Year FE" "Province FE" "Obs" "adjusted-R2"))
    mtitle("Only Country Level" "No Country Level" "All Level");
#delimit cr

