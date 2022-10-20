cd "~/Documents/R_Projects/Government_Guide_Fund/output/tables"

gen EPS_P = EPS / YearOpen

#delimit ;
winsor2 EPS_P Size Lev GDP_p Age MHRatio CG INS SuperINS RegionFin RDRatio StrategyScore, 
    cuts(1 99) by(Year) trim;
#delimit cr

#delimit ;
logout, save("summary-variables.doc") word replace:
    tabstat EPS_P DR Ret GGF 
    Size Lev MHRatio Age GDP_p 
    CG INS SuperINS RegionFin RDRatio StrategyScore,
    s(N mean sd min p25 p50 p75 max) c(s)
    ;
#delimit cr

gen DR_Ret = DR * Ret
gen GGF_DR_Ret = GGF * DR * Ret
* gen Size_DR_Ret = Size * DR * Ret
* gen Lev_DR_Ret = Lev * DR * Ret
* gen MHRatio_DR_Ret = MHRatio * DR * Ret
* gen Age_DR_Ret = Age * DR * Ret
* gen GDP_DR_Ret = GDP_p * DR * Ret 

#delimit ;
logout, save ("correlation") excel replace:
    pwcorr EPS_P GGF_DR_Ret Size Lev MHRatio Age GDP_p
    CG INS SuperINS RegionFin RDRatio StrategyScore,
    star(.001)
    ;
#delimit cr

