

# INFORMAL COMPETITION ----------------------------------------------------

peru_all %>% 
  # Calculate percentages by industry
  group_by(year) %>% 
  summarise(across(.cols = c("informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = year, 
             y = (1 - informal_competition)*100)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(title = "Percentage of firms facing ",
       subtitle = "Q: Does This Establishment Compete Against Unregistered Or Informal Firms?", 
       x = "Year", y = "% facing informal competition")



# By sector
peru_all %>% 
  # Calculate percentages by industry
  group_by(year, industry_sector) %>% 
  summarise(across(.cols = c("informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = year, 
             y = (1 - informal_competition)*100, 
             col = industry_sector)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Year", y = "% facing informal competition")


# INFORMALITY AS AN OBSTACLE ----------------------------------------------

# Average informality obstacle
peru_all %>% 
  group_by(year) %>% 
  summarise(informality_obstacle = weighted.mean(informal_practices_obstacle, 
                                                 weight = wmedian, na.rm = T)) %>% 
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


# BY REGION
peru_all %>% 
  group_by(year, sample_region) %>% 
  summarise(informality_obstacle = weighted.mean(informal_practices_obstacle, 
                                                 weight = wmedian, na.rm = T)) %>% 
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


# BY SECTOR
peru_all %>% 
  group_by(year, industry_sector) %>% 
  summarise(informality_obstacle = weighted.mean(informal_practices_obstacle, 
                                                 weight = wmedian, na.rm = T)) %>% 
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


# BY FIRM SIZE 
peru_all %>% 
  group_by(year, sample_size) %>% 
  summarise(informality_obstacle = weighted.mean(informal_practices_obstacle, 
                                                 weight = wmedian, na.rm = T)) %>% 
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



# REGISTRATION YEAR -------------------------------------------------------

# HIstogram years late
peru_all %>% 
  filter(dif_register != 0) %>% 
  ggplot(aes(dif_register)) + 
  geom_histogram() +
  custom_theme()


# By sector
peru_all %>% 
  
  filter(year == 2006) %>%
  
  # Calculate percentages by industry
  group_by(year, industry_sector) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = industry_sector, 
             y = (1 - registered_start)*100)) +
  
  geom_col(aes(fill = industry_sector)) +
  facet_grid(year~.) +
  
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Industry", y = "% registered when started")


# By region
peru_all %>% 
  # Calculate percentages by industry
  group_by(year, sample_region) %>% 
  summarise(across(.cols = c("registered_start", "informal_competition"),
                   .fns = function(x) weighted.mean(x, weight = wmedian, na.rm = T))) %>% 
  ungroup() %>% 
  
  # Plot
  ggplot(aes(x = sample_region, 
             y = (1 - registered_start)*100)) +
  
  geom_col(aes(fill = sample_region)) +
  facet_grid(year~.) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  ) +
  labs(x = "Region", y = "% registered when started")


# -------------------------------------------------------------------------


