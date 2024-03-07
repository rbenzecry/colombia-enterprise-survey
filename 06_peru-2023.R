
source("00_settings.R")



# SECTOR ------------------------------------------------------------------


# REGISTRATION YEAR
p_sector_registry <- peru_all %>%
  filter(year == 2023) %>% 
  
  # Calculate percentages by industry
  group_by(year, industry_sector) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = reorder(industry_sector, informal_competition), 
             y = (1 - registered_start)*100)) +
  
  geom_col(aes(fill = -informal_competition)) +
  # scale_fill_brewer(palette="Dark2") +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(subtitle =  "Q: Was Establishment Formally Registered When It Began Operations?",
       x = "Sector", y = "% registered when started")

p_sector_registry


# INFORMAL COMPETITION
p_sector_informal <- peru_all %>%
  filter(year == 2023) %>%  
  
  # Calculate percentages by industry
  group_by(industry_sector) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = reorder(industry_sector, informal_competition), 
             y = informal_competition*100)) +
  
  geom_col(aes(fill = -informal_competition)) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(subtitle = "Q: Does This Establishment Compete Against Unregistered Or Informal Firms?",
       x = "Sector", y = "% face informal competition")

p_sector_informal 



grid.arrange( p_sector_informal, p_sector_registry,
              ncol = 2)

# REGION ------------------------------------------------------------------


# REGISTRATION
p_region_registry <- peru_all %>%
  filter(year == 2023) %>%  
  # Calculate percentages by region
  group_by(sample_region) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = reorder(sample_region, informal_competition), 
             y = (1 - registered_start)*100)) +
  
  geom_col(aes(fill = informal_competition)) +
  scale_fill_gradient(low = "purple", high = "#301934") +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(subtitle =  "Q: Was Establishment Formally Registered When It Began Operations?",
       x = "Sector", y = "% registered when started")

p_region_registry


# INFORMAL COMPETITION
p_region_informal <- peru_all %>%
  filter(year == 2023) %>%  
  
  # Calculate percentages by industry
  group_by(sample_region) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = reorder(sample_region, informal_competition), 
             y = informal_competition*100)) +
  
  geom_col(aes(fill = informal_competition)) +
  scale_fill_gradient(low = "purple", high = "#301934") +
  
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(subtitle = "Q: Does This Establishment Compete Against Unregistered Or Informal Firms?",
       x = "Region", y = "% face informal competition")

p_region_informal 


grid.arrange( p_region_informal, p_region_registry,
              ncol = 2)

# FIRM SIZE ---------------------------------------------------------------

# INFORMAL COMPETITION
peru_all %>%
  filter(year == 2023) %>%  
  # Calculate percentages by region
  group_by(sample_size) %>% 
  summarise(across(.cols = c("registered_start", 
                             "informal_competition", 
                             "informal_practices_obstacle"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = sample_size, 
             y = informal_competition*100)) +
  
  geom_col(aes(fill = sample_size)) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Firm size", y = "% facing informal competition")



# REGISTRATION
peru_all %>%
  filter(year == 2023) %>%  
  # Calculate percentages by region
  group_by(sample_size) %>% 
  summarise(across(.cols = c("registered_start", 
                             "informal_competition", 
                             "informal_practices_obstacle"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = sample_size, 
             y = (1 - registered_start)*100)) +
  
  geom_col(aes(fill = sample_size)) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Firm size", y = "% facing informal competition")




# OBSTACLE
peru_all %>%
  filter(year == 2023) %>%  
  # Calculate percentages by region
  group_by(sample_size) %>% 
  summarise(across(.cols = c("registered_start", 
                             "informal_competition", 
                             "informal_practices_obstacle"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = sample_size, 
             y = informal_practices_obstacle)) +
  
  geom_col(aes(fill = sample_size)) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Firm size", y = "degree of obstacle")




# ANNUAL SALES ------------------------------------------------------------

# INFORMAL COMPETITION
peru_all %>%
  filter(year == 2023) %>%  
  
  ggplot(aes(x = log(annual_sales), y = informal_competition)) +
  geom_point() +
  custom_theme()

# OBSTACLE
peru_all %>%
  filter(year == 2023) %>%  
  
  ggplot(aes(x = log(annual_sales), y = informal_practices_obstacle)) +
  geom_point() +
  custom_theme()

# -------------------------------------------------------------------------


