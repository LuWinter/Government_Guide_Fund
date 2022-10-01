cd "~/Documents/R_Projects/Government_Guide_Fund/output/tables"

gen EPS_P = EPS / YearOpen
keep if Year >= 2017

#delimit ;
logout, save("summary-variables.doc") word replace:
    tabstat EPS_P DR Ret GGF 
    Size Lev MHRatio Age GDP_p 
    CG SuperINS RegionFin RDRatio StrategyScore,
    s(N mean p50 sd min max) c(s)
    ;
#delimit cr

gen DR_Ret = DR * Ret
gen GGF_DR_Ret = GGF * DR * Ret
gen Size_DR_Ret = Size * DR * Ret
gen Lev_DR_Ret = Lev * DR * Ret
gen MHRatio_DR_Ret = MHRatio * DR * Ret
gen Age_DR_Ret = Age * DR * Ret
gen GDP_DR_Ret = GDP_p * DR * Ret

#delimit ;
logout, save ("correlation2") excel replace:
    pwcorr EPS_P DR_Ret GGF_DR_Ret
    Size_DR_Ret Lev_DR_Ret MHRatio_DR_Ret
    Age_DR_Ret GDP_DR_Ret,
    star(.001)
    ;
#delimit cr

