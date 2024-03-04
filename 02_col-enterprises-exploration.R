
# source("01_ven-migrants.R")

# DATA --------------------------------------------------------------------

enterprise_survey <- read_dta("Data/Colombia-2017-full-data/Colombia-2017-full-data.dta")

# SET UP ------------------------------------------------------------------

# Rename columns
enterprise_survey <- enterprise_survey %>%
  rename(industry_sector = a4a,
         sample_size = a6a,
         sample_region = a2,
         capital_city = a3b,
         locality_size = a3,
         
         screener_sector = a4b,
         questionnaire = a0,
         establishment_region = a3a,
         screener_size = a6b,
         large_firm = a7,
         
         start_year = b5,
         registration_year = b6b,
         registered_start = b6a,
         
         informal_competition = e11,
         informal_practices = ASCe12,
         informal_contracts = ASCe14,
         informal_practices_obstacle = e30,
         informal_payment_percentage = j7a,
         
         total_informal_payment = j7b,
         labor_regulations_obstacle = l30a,
         total_labor_cost_last_fiscal_year = n2a,
         
         electricity_obstacle = c30a,
         telecom_obstacle = c30b,
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
         labor_regulations_obstacle_2 = l30a,
         inadequate_workforce_obstacle = l30b) %>% 
  
  # Recode variables
  mutate(across(.cols = c("registered_start", 
                          "informal_competition", 
                          "informal_contracts"),
                .fns = function(x) case_when(x == 2 ~ 0,
                                             x < 0 ~ NA,
                                             TRUE ~ x))) %>% 
  mutate(industry_sector = to_factor(industry_sector))
  
  

# EXPLORATION -------------------------------------------------------------

# Formally registered when started operating
enterprise_survey %>% 
  # Calculate percentages by industry
  group_by(industry_sector) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = reorder(industry_sector, registered_start), 
             y = (1 - registered_start)*100)) +
  
  geom_col(aes(fill = industry_sector)) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Industry", y = "% registered when started")




  

# PLOTS INFORMALITY -------------------------------------------------------

# Formally registered when started operating
enterprise_survey %>% 
  # Calculate percentages by industry
  group_by(industry_sector) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = industry_sector, 
             y = (1 - registered_start)*100)) +
  
  geom_col(aes(fill = industry_sector)) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Industry", y = "% registered when started")

# PLOTS OBSTACLES ---------------------------------------------------------

# Formally registered when started operating
enterprise_survey %>% 
  # Calculate percentages by industry
  group_by(industry_sector) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = industry_sector, 
             y = informal_competition*100)) +
  
  geom_col(aes(fill = industry_sector)) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Industry", y = "% facing informal competition")


# Obstacle
enterprise_survey %>% 
  # Calculate percentages by industry
  group_by(industry_sector) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = industry_sector, 
             y = informal_competition*100)) +
  
  geom_col(aes(fill = industry_sector)) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Industry", y = "% facing informal competition")


# EXPORT ------------------------------------------------------------------


# -------------------------------------------------------------------------

