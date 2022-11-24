clear
cd ~/Documents/R_Projects/Government_Guide_Fund/output

use "placebo-test_2022-10-24.dta", clear
list in 1/5

/****************
#delimit ;
kdensity value if LineType == "Placebo Test", addplot( 
    kdensity value if LineType == "Normal Distribution") 
    bw(0.018) 
    title("安慰剂检验")  
    xtitle("估计系数")  
    ytitle("概率密度")  
    legend(label(1 "置换检验系数概率分布") label(2 "正态分布") order(1 2));
#delimit cr
*****************/

/****************
#delimit ;
twoway
   (kdensity value if LineType == "Placebo Test", clpattern(l) bw(0.02))
   (kdensity value if LineType == "Normal Distribution", clpattern(-) bw(0.02))
   ,  
   xline(0 , lc(black*0.5) lp(solid)) 
   xline(0.07385598 , lc(black*0.2) lp(dash)) 
   plotregion(margin(zero)) 
   title("安慰剂检验") 
   xtitle("估计系数") 
   ytitle("概率密度")
   legend(label(1 "置换检验系数概率分布") label(2 "正态分布") order(1 2));
#delimit cr
graph export "figures/安慰剂检验1.png", replace
*****************/


use "placebo-test_2022-11-04.dta", clear
list in 1/5

generate p_level = 0.05

#delimit ;
twoway
   (scatter p_value Estimate, msize(tiny))
   (line p_level Estimate, lc(black*0.5) lp(solid))
   ,
   xline(0 , lc(black*0.2) lp(dash)) 
   plotregion(margin(zero)) 
   title("安慰剂检验") 
   xtitle("估计系数") 
   ytitle("P值")
   legend(label(1 "置换检验系数P值分布") label(2 "P = 0.05") order(1 2));
#delimit cr
graph export "figures/安慰剂检验2-tiny.png", replace


