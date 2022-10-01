
gen EPS_P = EPS / YearOpen

gen IndustryCode2 = cond(substr(IndustryCode, 1, 1) != "C", substr(IndustryCode, 1, 1), substr(IndustryCode, 1, 2))
egen Industry = group(IndustryCode2)

reghdfe EPS_P Ret DR c.Ret#DR Post Post#c.Ret Post#DR Post#c.Ret#DR, absorb(i.Year i.Industry)

