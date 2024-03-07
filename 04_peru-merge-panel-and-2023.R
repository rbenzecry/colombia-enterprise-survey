
# rm(list = ls())

source("00_settings.R")

# DATA --------------------------------------------------------------------

peru_panel <- read_dta("Data/Peru-panel-data/Peru_2006_2010_2017.dta")

peru_2023 <- read_dta("Data/Peru-2023-full-data/Peru-2023-full-data.dta")

ven_peru <- read_excel("Data/ven-immigration-department-COL-PER.xlsx",
                       sheet = "per-enpove-pop")

# SET UP ------------------------------------------------------------------

peru_panel <- peru_panel %>% 
  # IDs and weights
  select(idstd,
         panelid,
         year,
         panel,
         eligibility2017,
         eligibility2010,
         
         wstrict,
         wmedian,
         wweak,
         
         # Variables of interest
         industry_sector = a4a,
         sample_size = a6a,
         sample_region = a2,
         capital_city = a3b,
         locality_size = "_2010_2017_a3",
         
         screener_sector = a4b,
         questionnaire = a0,
         establishment_region = a3a,
         
         # screener_size = a6b,
         large_firm = a7,
         
         start_year = b5,
         registration_year = b6b,
         # registered_start = b6a,
         
         annual_sales = d2,
         
         informal_competition = e11,
         # "informal_practices_2017" = "_2017_ASCe12",
         # "informal_contracts_2010_17" = "_2010_2017_ASCe14",
         informal_practices_obstacle = e30,
         informal_payment_percentage = j7a,
         
         total_informal_payment = j7b,
         labor_regulations_obstacle = l30a,
         total_labor_cost_lfy = n2a,
         
         electricity_obstacle = c30a,
         # telecom_obstacle = c30b,
         transport_obstacle = d30a,
         customs_trade_obstacle = d30b,
         
         access_land_obstacle = g30a,
         crime_disorder_obstacle = i30,
         access_finance_obstacle = k30,
         tax_rates_obstacle = j30a,
         tax_administrations_obstacle = j30b,
         business_licensing_obstacle = j30c,
         political_instability_obstacle = j30e,
         corruption_obstacle = j30f,
         
         courts_obstacle = h30,
         # labor_regulations_obstacle_2 = l30a,
         inadequate_workforce_obstacle = l30b) %>% 
  
  # Convert to character to avoid mismatch of labels 
  mutate(across(.cols = c("sample_region", "sample_size",
                          "industry_sector", "screener_sector"),
                .fns = to_character)) %>% 
  
  # Difference between start and registration
  mutate(start_year = ifelse(start_year == -9, 
                             yes = registration_year, no = start_year),
         
         registration_year = ifelse(registration_year == -9 | is.na(registration_year),
                                    yes = start_year, no = registration_year),
         
         dif_register = registration_year - start_year) %>% 
  
  mutate(dif_register = ifelse(dif_register < 0,
                               yes = 0, no = dif_register),
         registered_start = as.numeric(dif_register == 0))

  
# 2023 
peru_2023 <- peru_2023 %>%   
  
  # Generate the variables missing in the 2023 dataset
  mutate(year = 2023,
         panel = NA,
         eligibility2017 = NA,
         eligibility2010 = NA) %>% 
  
  select(idstd,
         
         # 2023 Firm ID
         id,
         year,
         panel,
         eligibility2017,
         eligibility2010,
         
         # 2023 weights
         wstrict,
         wmedian,
         wweak,
         
         # Variables of interest
         industry_sector = a4a,
         sample_size = a6a,
         sample_region = a2,
         capital_city = a3b,
         locality_size = a3,
         
         screener_sector = a4b_v4,
         questionnaire = a0,
         establishment_region = a3a,
         
         # screener_size = a6b,
         large_firm = a7,
         
         start_year = b5,
         registration_year = b6b,
         # registered_start = b6a,
         
         annual_sales = d2,
         
         informal_competition = e11,
         # "informal_practices_2017" = "_2017_ASCe12",
         # "informal_contracts_2010_17" = "_2010_2017_ASCe14",
         informal_practices_obstacle = e30,
         informal_payment_percentage = j7a,
         
         total_informal_payment = j7b,
         labor_regulations_obstacle = l30a,
         total_labor_cost_lfy = n2a,
         
         electricity_obstacle = c30a,
         # telecom_obstacle = c30b,
         transport_obstacle = d30a,
         customs_trade_obstacle = d30b,
         
         access_land_obstacle = g30a,
         crime_disorder_obstacle = i30,
         access_finance_obstacle = k30,
         tax_rates_obstacle = j30a,
         tax_administrations_obstacle = j30b,
         business_licensing_obstacle = j30c,
         political_instability_obstacle = j30e,
         corruption_obstacle = j30f,
         
         courts_obstacle = h30,
         # labor_regulations_obstacle_2 = l30a,
         inadequate_workforce_obstacle = l30b) %>% 
  
  # Convert to character to avoid mismatch of labels 
  mutate(across(.cols = c("sample_region", "sample_size",
                          "industry_sector", "screener_sector"),
                .fns = to_character)) %>% 
  
  mutate(industry_sector = ifelse(industry_sector == "Textiles" |
                                    industry_sector == "Garments",
                                  yes = "Textiles & Garments", 
                                  no = industry_sector)) %>% 
  
  # Difference between start and registration
  mutate(start_year = ifelse(start_year == -9, 
                             yes = registration_year, no = start_year),
         
         registration_year = ifelse(registration_year == -9 | is.na(registration_year),
                                    yes = start_year, no = registration_year),
         
         dif_register = registration_year - start_year) %>% 
  
  mutate(dif_register = ifelse(dif_register < 0,
                               yes = 0, no = dif_register),
         
         registered_start = as.numeric(dif_register == 0))



# VENEZUELAN MIGRANTS -----------------------------------------------------


ven_peru <- ven_peru %>% 
  select("City",
         starts_with("ppop"),
         starts_with("pmig"),
         starts_with("migrants_"),
         starts_with("pop_")) %>% 
  slice(1:10) %>%
  
  # Long format all
  pivot_longer(cols = -City,
               names_to = "variable",
               values_to = "values") %>% 
  separate(col = "variable", 
           into = c("var", "year"),
           sep = "_") %>% 
  
  # Put the variables back in the columns
  pivot_wider(names_from = var,
              values_from = values) %>% 
  rename(percent_migrants = pmig,
         percent_pop = ppop,
         
         total_population = pop) %>%
  
  mutate(year = as.numeric(year)) %>% 
  
  # We are going to create a variable of year to match the data with the survey
  mutate(year_survey = case_when(year == 2022 ~ 2023,
                                 year == 2018 ~ 2017,
                                 TRUE~year),
         
         # And modify the City to match the survey
         City = ifelse(City == "Lima y Callao",
                       yes = "Lima", no = City))
         

# CHECK -------------------------------------------------------------------

# Economic sector
peru_panel$industry_sector %>% attr('labels')
peru_2023$industry_sector %>% attr('labels')

peru_panel$screener_sector %>% attr('labels')
peru_2023$screener_sector %>% attr('labels')

# Informality variables
peru_2023$informal_competition %>% attr('labels')
peru_2023$informal_competition %>% attr('label')
peru_2023$informal_practices_obstacle %>% attr('labels')

# MERGE DATASETS ----------------------------------------------------------

peru_all <- bind_rows(peru_panel, peru_2023) %>%
  
  # Recode variable values
  mutate(across(.cols = c("informal_competition"),
                .fns = function(x) case_when(x == 2 ~ 0,
                                             x < 0 ~ NA,
                                             TRUE ~ x))) %>%
  
  mutate(across(.cols = ends_with("_obstacle"),
                .fns = function(x) ifelse(x < 0,
                                          yes = NA, no = x))) %>% 
         
         
         # informal_practices_obstacle = ifelse(informal_practices_obstacle < 0,
         #                                      yes = NA, no = informal_practices_obstacle)) %>% 
  
  # Joining with the data frame of migration but only keeping a specific number of columns
  left_join(select(ven_peru, 
                   -year), 
            
            by = c("sample_region" = "City",
                   "year" = "year_survey")) %>% 
  
  # rename("share_ven_migrants" = "share",
  #        "ven_migrants" = "ven_population") %>% 
  
  
  # Further cleaning of the panel
  mutate(across(.cols = c("sample_region", "sample_size", 
                          "industry_sector", "screener_sector"),
                .fns = as.factor))

  
# EXPORT ------------------------------------------------------------------

write_dta(peru_all, "Tables/peru_2006-2023.dta")

# -------------------------------------------------------------------------


