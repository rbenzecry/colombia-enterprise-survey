
source("00_settings.R")

# DATA --------------------------------------------------------------------

ven_migration <- read.csv("Data/Entradas_de_venezolanos_a_Colombia_20240303.csv")


month_number <- tibble("esp_name" = c("Enero", "Febrero", "Marzo", 
                                      "Abril", "Mayo", "Junio", 
                                      "Julio", "Agosto", "Septiembre", 
                                      "Octubre", "Noviembre", "Diciembre"),
                       "month_n" = seq(1, 12))

# SET UP ------------------------------------------------------------------


# Venezuelan migration to long and monthly
v_migrant_long <- ven_migration %>% 
  clean_names() %>% 
  
  # Add month number
  left_join(month_number, by = c("mes" = "esp_name")) %>% 
  # Remove all non-character number and turn to numeric
  mutate(across(.cols = c("femenino", "masculino", "total"),
                .fns = parse_number)) %>% 
  # Pivot
  pivot_longer(cols = c("femenino", "masculino", "total"),
               names_to = "group",
               values_to = "total_migrants") %>% 
  # Summarise by month and sex group
  mutate(month = ym(paste(ano, month_n))) %>%
  group_by(month, group) %>% 
  summarise(total_migrants = sum(total_migrants)) %>% 
  ungroup() %>% 
  # Create year column
  mutate(year = substr(month, 1, 4)) 
  


# EXPLORATION -------------------------------------------------------------

# Explore variation
v_migrant_long %>% 
  filter(group == "total") %>% 
  group_by(year) %>% 
  summarise(total = sum(total_migrants)) %>% 
  ungroup() %>% 
  mutate(variation = total/lag(total) - 1)


# Total
v_migrant_long %>% 
  filter(group == "total") %>% 
  summarise(total = sum(total_migrants)) 
  



# PLOTS MIGRATION ---------------------------------------------------------


# Total by gender
v_migrant_long %>% 
  
  filter(group != "total") %>% 
  
  ggplot(aes(x = month, y = total_migrants, col = group)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )



# Variation by gender
v_migrant_long %>% 
  group_by(year, group) %>% 
  summarise(total = sum(total_migrants)) %>% 
  ungroup() %>% 
  group_by(group) %>% 
  mutate(variation = total/lag(total) - 1) %>% 
  
  ggplot(aes(x = as.numeric(year), y = variation, col = group)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )

  
# Total by month 2017
v_migrant_long %>% 
  
  filter(year == "2017") %>% 
  
  ggplot(aes(x = month, y = total_migrants/10**3, col = group)) +
  
  geom_point(size = 1) +
  geom_line(linewidth = 0.8) +
  geom_line(linewidth = 3, alpha = 0.3) +
  
  custom_theme() +
  # Rotate x-axis labels
  theme(
    axis.text.x = element_text(angle = 270, hjust = 1)
  )



# EXPORT ------------------------------------------------------------------


# -------------------------------------------------------------------------

