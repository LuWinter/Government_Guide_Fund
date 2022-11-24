
cd "~/Documents/R_Projects/Government_Guide_Fund/output/tables"

destring Stkcd, replace
xtset Stkcd Year


/******************************* Variable Defination ****************************/

egen Industry2 = group(Industry)
rename Industry Industry_str
rename Industry2 Industry

egen Province2 = group(Province)
rename Province Province_str
rename Province2 Province


/******************************* Macros Defination ****************************/
global common_fe "i.Year i.Industry i.Province"
global control_vars "Size Lev ROA Growth Big4"


/******************************* Perform Test ****************************/

eststo EarningQuality: reghdfe l.ABSDA HoldRatio $control_vars, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

eststo TaxAvoid: reghdfe l.TaxAvoid HoldRatio $control_vars, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

eststo RiskTaking: reghdfe l.RiskTaking HoldRatio $control_vars, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"


#delimit ;                               
esttab EarningQuality TaxAvoid RiskTaking
	using further-analysis07_economic-result.rtf,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    varwidth(15) b t(4) ar2(4) 
	s(fe_industry fe_year fe_province N r2_a, 
	    label("Industry FE" "Year FE" "Province FE" "Obs" "adjusted-R2"));
#delimit cr

