# ---
# Title: Plot Figures 4a, 5a, and 6a
# Author: Dongmin (Dennis) Kim, UMN; Harvard
# Date: April 2026
# Description: Visualization: Jitter plots for Figures 4a, 5a, and 6a.
# ---

# call libraries 
library(here)
library(MASS)
library(nlme)
library(mgcv)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)

genPath <- "/path_to_folder/"

# load data 
final_df <- readRDS(here::here(genPath, "Final_dryland_data_mar2026.rds"))

# remove species that has small samples (4 species removed - now 13 species)
final_df1 <- final_df %>% dplyr::filter(! species %in% c("Cathartes burrovianus", "Gyps bengalensis", "Gyps rueppellii", "Sarcoramphus papa"))

# Remove NA rows and drop unused factor levels
final_df1a <- final_df1 %>%
  filter(!is.na(REALM) & REALM != "N/A") %>%
  drop_na() %>%
  droplevels()

# only select the columns applicable for the visualization
final_df2a <- final_df1a %>% dplyr::select(species, REALM, animal_id, year, month, UDsizeKm2_99, dsl_UD, wmNDVI)
final_df2b <- final_df1 %>% dplyr::select(species, HIX_DESC, animal_id, year, month, UDsizeKm2_99, dsl_UD, wmNDVI)

# prepare for the visualization

# realm: create an explicit date and time-difference metrics 
final_df2a <- final_df2a %>% mutate(date = lubridate::make_date(year, month, 1)) %>% arrange(species, REALM, animal_id, date)

# compute time gaps between consecutive UDs (per animal)
final_df2a <- final_df2a %>% group_by(species, REALM, animal_id) %>% mutate(time_lag_months = interval(lag(date), date) %% months(1)) %>% ungroup()

# dryland: create an explicit date and time-difference metrics 
final_df2b <- final_df2b %>% mutate(date = lubridate::make_date(year, month, 1)) %>% arrange(species, HIX_DESC, animal_id, date)

# compute time gaps between consecutive UDs (per animal)
final_df2b <- final_df2b %>% group_by(species, HIX_DESC, animal_id) %>% mutate(time_lag_months = interval(lag(date), date) %% months(1)) %>% ungroup()



# Figure 4a: plot the UD size changes per realm across the time 

# Okabe-Ito palette (first 5 colors)
okabe_ito <- c(
  "#B85C00", # dark orange (unchanged)
  "#00714A", # dark bluish green (kept, very distinct)
  "#7A4900", # warm dark brown (replaces gold/yellow)
  "#781C81", # dark purple (strong contrast with others)
  "#1B4F72"  # deep steel blue (unique, darker and not similar to sky blue)
)

figure4a <- ggplot(final_df2a, aes(x = date, y = dsl_UD, fill = REALM, color = REALM))+
  geom_point(alpha = 0.6)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = okabe_ito) +
  scale_fill_manual(values = okabe_ito) +
  theme_minimal()+
  labs(
    x = "Time (year)",
    y = expression("Change in UD Size (km²)")
  )+
  theme(strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 15)),  # space above x title
        axis.title.y = element_text(margin = margin(r = 15))
  )+
  facet_wrap(~ REALM)


# Figure 5a: plot the UD size changes per Dryland classes across the time 
# dryland color palettes
dryland_colors <- c(
  "Hyperarid" = "#E41A1C",
  "Arid" = "#FF7F00",
  "Semiarid" = "#984EA3",
  "Dry subhumid" = "#377EB8",
  "Non-dryland" = "#4DAF4A"
)

final_df2b$HIX_DESC <- factor(final_df2b$HIX_DESC, level = c("Hyperarid", "Arid", "Semiarid", "Dry subhumid", "Non-dryland"))

figure3c <- ggplot(final_df2b, aes(x = date, y = dsl_UD, fill = HIX_DESC, color = HIX_DESC))+
  geom_point(alpha = 0.6)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # Color-blind friendly palette for points
  scale_color_manual(values = dryland_colors)+
  scale_fill_manual(values = dryland_colors)+
  theme_minimal()+
  labs(
    x = "Time (year)",
    y = expression("Change in UD Size (km²)")
  )+
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 15)),  # space above x title
        axis.title.y = element_text(margin = margin(r = 15))
  )+
  facet_wrap(~ HIX_DESC)



# Figure 6a: plot the UD size changes per species across the time 

# for 13 species
okabe_ito_13 <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
  "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"
)

figure6a <- ggplot(final_df2a, aes(x = date, y = dsl_UD, fill = species, color = species))+
  geom_point(alpha = 0.6)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_manual(values = okabe_ito_13) +
  scale_fill_manual(values = okabe_ito_13) +
  theme_minimal()+
  labs(
    x = "Time (year)",
    y = expression("Change in UD Size (km²)")
  )+
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(margin = margin(t = 15)),  # space above x title
        axis.title.y = element_text(margin = margin(r = 15))
  )+
  facet_wrap(~ species)