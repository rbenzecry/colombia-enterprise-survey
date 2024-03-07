

use "G:\My Drive\LSE\Year 1\PP455 Quantitative Methods\WT Memo\colombia-enterprise-survey\Tables\peru_2006-2023.dta", clear

drop if year != 2023

// Specify survey design
svyset [weight=wmedian], psu(idstd)
*svyset [weight=wstrict], psu(idstd)
*svyset [weight=wweak], psu(idstd)

*gen log_inf_competition = log(informal_competition)
*gen log_percent_pop = log(percent_pop)
*reg log_inf_competition log_percent_pop sample_size

// INFORMAL COMPETITION
svy: reg informal_competition annual_sales
svy: reg informal_competition annual_sales i.sample_region
svy: reg informal_competition annual_sales i.sample_region i.industry_sector
svy: reg informal_competition annual_sales i.sample_region i.industry_sector i.sample_size
svy: reg informal_competition registered_start annual_sales i.sample_region i.industry_sector i.sample_size
svy: reg informal_competition registered_start tax_administrations_obstacle annual_sales i.sample_region i.industry_sector i.sample_size 
svy: reg informal_competition labor_regulations_obstacle annual_sales informal_payment_percentage i.sample_region i.industry_sector i.sample_size

// INFORMALITY OBSTACLE
/*svy: reg informal_practices_obstacle annual_sales
svy: reg informal_practices_obstacle annual_sales i.sample_region
svy: reg informal_practices_obstacle annual_sales i.sample_region i.industry_sector
svy: reg informal_practices_obstacle annual_sales i.sample_region i.industry_sector i.sample_size
svy: reg informal_practices_obstacle registered_start annual_sales i.sample_region i.industry_sector i.sample_size
svy: reg informal_practices_obstacle registered_start annual_sales i.sample_region i.industry_sector i.sample_size tax_administrations_obstacle*/
svy: reg informal_practices_obstacle labor_regulations_obstacle annual_sales informal_payment_percentage i.sample_region i.industry_sector i.sample_size

svy: reg informal_practices_obstacle informal_competition
svy: reg informal_practices_obstacle informal_competition annual_sales informal_payment_percentage i.sample_region i.industry_sector i.sample_size
