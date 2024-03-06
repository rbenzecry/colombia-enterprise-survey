

# INFORMAL COMPETITION ----------------------------------------------------

peru_all %>% 
  # Calculate percentages by industry
  group_by(year) %>% 
  summarise(across(.cols = c("informal_competition"),
                   .fns = function(x) weighted.mean(x, wmedian, na.rm = T))) %>% 
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
  labs(x = "Year", y = "% facing informal competition")



# By sector
peru_all %>% 
  # Calculate percentages by industry
  group_by(year, industry_sector) %>% 
  summarise(across(.cols = c("informal_competition"),
                   .fns = function(x) weighted.mean(x, wmedian, na.rm = T))) %>% 
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


# BY REGION
peru_all %>% 
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


# -------------------------------------------------------------------------


