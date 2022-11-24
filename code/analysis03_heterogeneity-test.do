
/***************** Content *****************
****** 1. Corporate Governance	      ******
****** 2. Institution Hold            ******
****** 3. Regional Finance    		  ******
****** 4. R&D Investment			  ******
****** 5. Corporate Strateg           ******
*******************************************/


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

display _N

winsor2 Size MB Lev GDP_p SuperINS RDRatio, cuts(1 99) by(Year) trim replace


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



/****************************** 1. Corporate Governance **************************/

egen CG_p50 = pctile(CG), p(50)
gen CG_d = 1 if CG >= CG_p50 & CG != .
replace CG_d = 0 if CG < CG_p50
tab CG_d

#delimit ;
eststo high_cg: 
	quietly reghdfe EPS_P $base_reg $GGF_reg
    if CG_d == 1,
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
eststo low_cg:
	quietly reghdfe EPS_P $base_reg $GGF_reg   
    if CG_d == 0, 						
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
eststo high_cg_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg	
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if CG_d == 1, 						
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
eststo low_cg_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg	
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if CG_d == 0, 						
	absorb($common_fe $high_fe)
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace

                         
/******************************* 2. Institution Hold *****************************/      

egen INS_p50 = pctile(INS), p(50)
gen INS_d = 1 if INS >= INS_p50 & INS != .
replace INS_d = 0 if INS < INS_p50

display _N                         
tab INS_d

#delimit ;
eststo high_ins:
	quietly reghdfe EPS_P $base_reg $GGF_reg  	
    if INS_d == 1, 				
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
eststo low_ins:		   		   
	quietly reghdfe EPS_P $base_reg $GGF_reg  	
    if INS_d == 0, 				
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
eststo high_ins_ct:                               
	quietly reghdfe EPS_P $base_reg $GGF_reg 	
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg
    if INS_d == 1, 				
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
eststo low_ins_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg	
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg 
    if INS_d == 0, 				
	absorb($common_fe $high_fe)   		
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace

   
/******************************* 3. Regional Finance *****************************/

#delimit ;
eststo high_fin:
	quietly reghdfe EPS_P $base_reg $GGF_reg   	
    if RegionFin == 1, 					
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
eststo low_fin:	
	quietly reghdfe EPS_P $base_reg $GGF_reg   	
    if RegionFin == 0, 					
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
eststo high_fin_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg 	
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg  
    if RegionFin == 1, 					
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
eststo low_fin_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg 	
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg  
    if RegionFin == 0, 					
	absorb($common_fe $high_fe)   		
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace
                                
								
/******************************** 4. R&D Investment *******************************/

egen rd_p50 = pctile(RDRatio), p(50)
gen rd_d = 1 if RDRatio >= rd_p50 & RDRatio != .
replace rd_d = 0 if RDRatio < rd_p50

display _N                         
tab rd_d GGF
                                 

#delimit ;
eststo low_rd:
	quietly reghdfe EPS_P $base_reg $GGF_reg   
    if rd_d == 0, 				
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
eststo high_rd:
	quietly reghdfe EPS_P $base_reg $GGF_reg 
    if rd_d == 1, 				
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
eststo low_rd_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg 	
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg   
    if rd_d == 0, 				
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
eststo high_rd_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg	
	$Size_reg $Lev_reg $MHRatio_reg $Age_reg $GDP_reg  
    if rd_d == 1, 				
	absorb($common_fe $high_fe)   		
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace
                                
								
/********************************* 5. Corp Strategy ******************************/             

egen ss_p50 = pctile(StrategyScore), p(66)
gen ss_d = 1 if StrategyScore > 21 & StrategyScore != .
replace ss_d = 0 if StrategyScore <= 21

#delimit ;
eststo high_stra:                                
	reghdfe EPS_P $base_reg $GGF_reg   	
    if ss_d == 1, 				
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
eststo low_stra:   
	reghdfe EPS_P $base_reg $GGF_reg 
    if ss_d == 0, 			   	
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
eststo high_stra_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg	
	$Size_reg $SuperINS_reg $MHRatio_reg $Age_reg $GDP_reg  
    if ss_d == 1, 				
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
eststo low_stra_ct:                         
	quietly reghdfe EPS_P $base_reg $GGF_reg  	
	$Size_reg $SuperINS_reg $MHRatio_reg $Age_reg $GDP_reg
    if ss_d == 0, 				
	absorb($common_fe $high_fe)			
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace


/********************************* 5. Corp Strategy ******************************/             

#delimit ;
eststo state_owned:                                
	reghdfe EPS_P $base_reg $GGF_reg   	
    if SOE == 1, 				
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
eststo privity_owned:   
	reghdfe EPS_P $base_reg $GGF_reg 
    if SOE == 0, 			   	
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
eststo state_owned_ct:
	quietly reghdfe EPS_P $base_reg $GGF_reg	
	$Size_reg $SuperINS_reg $MHRatio_reg $Age_reg $GDP_reg  
    if SOE == 1, 				
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
eststo privity_owned_ct:                         
	quietly reghdfe EPS_P $base_reg $GGF_reg  	
	$Size_reg $SuperINS_reg $MHRatio_reg $Age_reg $GDP_reg
    if SOE == 0, 				
	absorb($common_fe $high_fe)			
    ;
#delimit cr
quietly estadd local control "YES", replace
quietly estadd local fe_industry "YES", replace
quietly estadd local fe_year "YES", replace
quietly estadd local fe_province "YES", replace
quietly estadd local fe_indu_year "YES", replace
quietly estadd local fe_prov_year "YES", replace



                              
/********************************* Output Result ******************************/

global var_list "_cons DR Ret 1.DR#c.Ret GGF 0.GGF#1.DR 1.GGF#c.Ret 1.GGF#1.DR#c.Ret"

#delimit ;                    
esttab high_cg low_cg high_cg_ct low_cg_ct
    using further-analysis01_corp-governance.rtf      ,                   	
    replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
	keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))					
    mtitle("High CG" "Low CG" "High CG (Control)" "Low CG (Control)");
#delimit cr

#delimit ;
esttab high_ins low_ins high_ins_ct low_ins_ct 
    using further-analysis02_institution-hold.rtf   ,                     	
    replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
	keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
		    "Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))						
    mtitle("High Ins" "Low Ins" "High Ins (Control)" "Low Ins (Control)");
#delimit cr

#delimit ;         
esttab high_fin low_fin high_fin_ct low_fin_ct	
    using further-analysis03_regional-finance.rtf   ,      
    replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
	keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))			
    mtitle("High Finance" "Low Finance" "High Finance (Control)" "Low Finance (Control)");
#delimit cr

#delimit ;
esttab high_rd low_rd high_rd_ct low_rd_ct
    using further-analysis04_rd-investment.rtf   ,                    
    replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
	keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))		
    mtitle("High R&D" "Low R&D" "High R&D (Control)" "Low R&D (Control)");
#delimit cr

#delimit ;         
esttab high_stra low_stra high_stra_ct low_stra_ct	
    using further-analysis05_corp-strategy.rtf  ,                                	
    replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
	keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))
    mtitle("High Strategy" "Low Strategy" "High Strategy CT" "Low Strategy CT");
#delimit cr	

#delimit ;         
esttab state_owned privity_owned state_owned_ct privity_owned_ct	
    using further-analysis06_equity-nature.rtf  ,                                	
    replace label nogap star(* 0.10 ** 0.05 *** 0.01) 
	keep($var_list) varwidth(15) b t(4) ar2(4) 
	s(control fe_industry fe_year fe_province fe_indu_year fe_prov_year N r2_a, 
	  label("Control Variables" "Industry FE" "Year FE" "Province FE" 
			"Industry ✖ Year FE" "Province ✖ Year FE" "Obs" "adjusted-R2"))
    mtitle("State Owned" "Privity Owned" "State Owned CT" "Privity Owned CT");
#delimit cr	
