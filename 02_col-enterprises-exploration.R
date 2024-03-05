
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
  mutate(across(.cols = c("sample_region", "industry_sector"),
                .fns = to_factor)) %>% 
  
  mutate(informal_practices_obstacle = ifelse(informal_practices_obstacle < 0,
                                              yes = NA, no = informal_practices_obstacle))
  
  


# PLOTS INFORMALITY -------------------------------------------------------

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


# PLOTS OBSTACLES ---------------------------------------------------------

# Facing informal competition
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



# BOXPLOTS
# Degree of obstacle
enterprise_survey %>% 
  
  # Plot
  ggplot(aes(x = industry_sector, 
             y = informal_practices_obstacle)) +
  
  geom_jitter(aes(col = industry_sector), alpha = 0.3, stroke = 0) +
  
  geom_boxplot(aes(fill = industry_sector), alpha = 0.5) +
  
  ylim(0, 5) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Industry", y = "degree of obstacle")




# HISTOGRAM
# Degree of obstacle per sector
enterprise_survey %>%
  filter(!is.na(informal_practices_obstacle)) %>% 
  
  # Plot
  ggplot(aes(informal_practices_obstacle)) +
  
  geom_density(aes(fill = industry_sector), alpha = 0.5) +
  facet_grid(industry_sector~.) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )



# PLOTS PER REGION --------------------------------------------------------


# BOXPLOTS
# Degree of obstacle
enterprise_survey %>% 
  
  # Plot
  ggplot(aes(x = sample_region, 
             y = informal_practices_obstacle)) +
  
  geom_jitter(aes(col = sample_region), alpha = 0.3, stroke = 0) +
  
  geom_boxplot(aes(fill = sample_region), alpha = 0.5) +
  
  ylim(0, 5) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Region", y = "degree of obstacle")




# HISTOGRAM
# Degree of obstacle per sector
enterprise_survey %>%
  filter(!is.na(informal_practices_obstacle)) %>% 
  
  # Plot
  ggplot(aes(informal_practices_obstacle)) +
  
  geom_density(aes(fill = sample_region), alpha = 0.5) +
  facet_grid(sample_region~.) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )


# EXPORT ------------------------------------------------------------------

aux <- enterprise_survey %>% 
  filter(sample_region == "Bogota")

aux %>% 
  ggplot(aes(informal_practices_obstacle)) +
  geom_histogram(fill = "midnightblue", alpha = 0.5, width = 4) +
  custom_theme()

enterprise_survey$informal_practices_obstacle %>% attr('labels')
enterprise_survey$sample_region %>% attr('labels')

# -------------------------------------------------------------------------

