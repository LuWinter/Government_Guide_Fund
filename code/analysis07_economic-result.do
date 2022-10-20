
cd "~/Documents/R_Projects/Government_Guide_Fund/output/tables"

destring Stkcd, replace
xtset Stkcd Year


* ttable2 LOGMV TobinQ Growth ROA LEV RDRatio RDRatio2 INS SOE, by(Treat)


/******************************* Variable Defination ****************************/
gen IndustryCode2 = cond(substr(IndustryCode, 1, 1) != "C", substr(IndustryCode, 1, 1), substr(IndustryCode, 1, 2))
egen Industry = group(IndustryCode2)

egen Province2 = group(Province)
rename Province Province_str
rename Province2 Province

winsor2 NCSKEW DUVOL ABSDA, cuts(1 99) by(Year) replace


/******************************* Macros Defination ****************************/
global common_fe "i.Year i.Industry i.Province"
global high_fe "i.Year#i.Province i.Year#i.Industry"


/******************************* Perform Test ****************************/
* reghdfe NCSKEW HoldRatio Size Lev SIGMA RET MB ABSDA FCF ROA, absorb($common_fe $high_fe)
* reghdfe DUVOL HoldRatio Size Lev SIGMA RET MB ABSDA FCF ROA, absorb($common_fe $high_fe)
* reghdfe ABSDA HoldRatio Size Lev MB ROA FCF Big4 CG, absorb($common_fe $high_fe)

reghdfe NCSKEW Post_Treat, absorb($common_fe)

reghdfe DUVOL Post_Treat, absorb($common_fe)

eststo EQ: reghdfe ABSDA Post_Treat, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"

eststo TA: reghdfe BTD Post_Treat, absorb($common_fe)
quietly estadd local fe_industry "YES"
quietly estadd local fe_year "YES"
quietly estadd local fe_province "YES"


#delimit ;                               
esttab EQ TA
	using further-analysis07_economic-result.rtf,  
	replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
    varwidth(15) b t(4) ar2(4) 
	s(fe_industry fe_year fe_province N r2_a, 
	    label("Industry FE" "Year FE" "Province FE" "Obs" "adjusted-R2"));
#delimit cr

