
source("00_settings.R")

# DATA --------------------------------------------------------------------

panel_col_raw <- read_dta("Data/Colombia-panel-data/Colombia_2006_2010_2017.dta")

# SET UP ------------------------------------------------------------------

panel_col <- panel_col_raw %>%
  rename(industry_sector = a4a,
         sample_size = a6a,
         sample_region = a2,
         capital_city = a3b,
         locality_size_2010_17 = "_2010_2017_a3",
         
         screener_sector = a4b,
         questionnaire = a0,
         establishment_region = a3a,
         screener_size = a6b,
         large_firm = a7,
         
         start_year = b5,
         registration_year = b6b,
         registered_start = b6a,
         
         informal_competition = e11,
         "informal_practices_2017" = "_2017_ASCe12",
         "informal_contracts_2010_17" = "_2010_2017_ASCe14",
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
                          "informal_contracts_2010_17"),
                .fns = function(x) case_when(x == 2 ~ 0,
                                             x < 0 ~ NA,
                                             TRUE ~ x))) %>% 
  mutate(across(.cols = c("sample_region", "industry_sector"),
                .fns = to_factor)) %>% 
  
  mutate(informal_practices_obstacle = ifelse(informal_practices_obstacle < 0,
                                              yes = NA, no = informal_practices_obstacle))


# EXPLORE -----------------------------------------------------------------

panel_col$informal_contracts_2010_17 %>% attr('labels')
panel_col$sample_region %>% attr('labels')


# PLOTS INFORMALITY -------------------------------------------------------

# Formally registered when started operating
panel_col %>% 
  # Calculate percentages by industry
  group_by(year, industry_sector) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = year, 
             y = (1 - registered_start)*100, 
             col = industry_sector)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Industry", y = "% registered when started")

  
# PLOTS OBSTACLES ---------------------------------------------------------

# BOXPLOTS
# Degree of obstacle
panel_col %>% 
  
  # Plot
  ggplot(aes(x = industry_sector, 
             y = informal_practices_obstacle)) +
  
  # geom_jitter(aes(col = industry_sector), alpha = 0.3, stroke = 0) +
  
  geom_boxplot(aes(fill = as.factor(year)), alpha = 0.5) +
  
  ylim(0, 5) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Industry", y = "degree of obstacle")



# HISTOGRAM
# Degree of obstacle per sector
panel_col %>%
  filter(!is.na(informal_practices_obstacle)) %>% 
  
  # Plot
  ggplot(aes(informal_practices_obstacle)) +
  
  geom_density(aes(fill = as.factor(year)), alpha = 0.5) +
  facet_grid(industry_sector~.) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )


#
panel_col %>%
  # filter(!is.na(informal_practices_obstacle)) %>% 
  
  # Plot
  ggplot(aes(year, 
             informal_practices_obstacle)) +
  
  geom_jitter(aes(fill = industry_sector), alpha = 0.5) +
  # facet_grid(industry_sector~.) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )


# AVERAGE -----------------------------------------------------------------


# Average informality by sector
panel_col %>% 
  group_by(year, industry_sector) %>% 
  summarise(informality_obstacle = weighted.mean(informal_practices_obstacle, 
                                                 wmedian, na.rm = T)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = year, 
            y = informality_obstacle,
            col = industry_sector)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  ylim(0, 5) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )

  
  

# Average informality by region
panel_col %>% 
  group_by(year, sample_region) %>% 
  summarise(informality_obstacle = weighted.mean(informal_practices_obstacle, 
                                                 wmedian, na.rm = T)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = year, 
             y = informality_obstacle,
             col = sample_region)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  ylim(0, 5) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )  



# Average informality by region
panel_col %>% 
  group_by(year) %>% 
  summarise(informality_obstacle = weighted.mean(informal_practices_obstacle, 
                                                 wmedian, na.rm = T)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = year, 
             y = informality_obstacle)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  ylim(0, 5) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )  


# By firm size
panel_col %>% 
  mutate(sample_size = as.factor(sample_size)) %>% 
  group_by(year, sample_size) %>% 
  summarise(informality_obstacle = weighted.mean(informal_practices_obstacle, 
                                                 wmedian, na.rm = T)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = year, 
             y = informality_obstacle,
             col = sample_size)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  ylim(0, 5) +
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )  

# -------------------------------------------------------------------------

# vector_new <- c("des", "a4a", "a6a", "a2", "a3b", "_2010_2017_a3", "a4b", "a0", "a3a", "a6b", "a7", 
#                     "b6a", "b6b", "e11", "_2017_ASCe12", "_2010_2017_ASCe14", "e30", "j7a", "j7b", 
#                     "l30a", "n2a")
# 
# vector_old <- c("des", "a4a", "a6a", "a2", "a3b", "a3", "a4b", "a0", "a3a", "a6b", "a7", 
#                     "b6a", "b6b", "e11", "ASCe12", "ASCe14", "e30", "j7a", "j7b", "m1a_labor_pos",
#                     "l30a", "n2a")
# 
# setdiff(vector_new, vector_old)
