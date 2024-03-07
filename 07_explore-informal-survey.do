
use "G:\My Drive\LSE\Year 1\PP455 Quantitative Methods\WT Memo\colombia-enterprise-survey\Data\Peru-2022-ISES-full-data\Peru-2022-ISES-full-data.dta", clear

svyset [weight=wmedian], psu(idstd) strata(strata)


gen new_city = b12 >= 1 & b12 <= 8
tab new_city
des r2a r2f r2e r2b r2c r2d