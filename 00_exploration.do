
cd "G:\My Drive\LSE\Year 1\PP455 Quantitative Methods\WT Memo\colombia-enterprise-survey"

// 								DATA

use "Data\Colombia-2017-full-data\Colombia-2017-full-data.dta", clear

des a4a a6a a2 a3b a3 a4b a0 a3a a6b a7 b6a b6b e11 ASCe12 ASCe14 e30 j7a j7b m1a_labor_pos l30a n2a ///
m1a m1a_finance_pos m1a_land_pos m1a_permit_pos m1a_corruption_pos m1a_courts_pos m1a_crime_pos ///
m1a_trade_pos m1a_electricity_pos m1a_workforce_pos m1a_labor_pos m1a_instability_pos /// 
m1a_informal_pos m1a_taxadmin_pos m1a_taxrate_pos m1a_transport_pos

* Sector and region variables
des a4a a6a a2 a3b a3 a4b a0 a3a a6b a7

*Informality-related variables
des b6a b6b e11 ASCe12 ASCe14 e30 j7a j7b m1a_labor_pos l30a


