# ---
# Title: Plot Figure 3
# Author: Dongmin (Dennis) Kim, UMN; Harvard
# Date: April 2026
# Description: Visualization: Figure 3
# ---

# call libraries
library(rnaturalearth)
library(ggspatial)
library(ggmapcn)
library(ggplot2)
library(patchwork)

genPath <- "/path_to_folder/"

# Load the map of terrestrial realms
realm_base <- ne_download(scale = 50, type = "land", category = "physical", returnclass = "sf")

# read the final data
final_df1 <- readRDS(here::here(genPath, "Final_dryland_data_mar2026.rds"))

# remove species that has small samples (4 species removed - now 13 species)
final_df1 <- final_df_clean %>% dplyr::filter(! species %in% c("Cathartes burrovianus", "Gyps bengalensis", "Gyps rueppellii", "Sarcoramphus papa"))

# remove all the NAs in REALM group
final_df_clean <- final_df1 %>% filter(!is.na(REALM) & REALM != "N/A") %>% droplevels()


# Figure 3a. Realm map with locations

# Okabe-Ito palette (first 5 colors)
okabe_ito <- c(
  "#B85C00", # dark orange (unchanged)
  "#00714A", # dark bluish green (kept, very distinct)
  "#7A4900", # warm dark brown (replaces gold/yellow)
  "#781C81", # dark purple (strong contrast with others)
  "#1B4F72"  # deep steel blue (unique, darker and not similar to sky blue)
)

# Plot Figure 3a
fig3a <- ggplot() +
  annotation_graticule(
    lon_step = 60, lat_step = 30,
    line_color = "gray70", line_type = "dashed",
    label_color = "gray30"
  ) +
  geom_sf(data = realm_base, fill = "gray95", color = "black", size = 0.2) +
  geom_point(
    data = final_df_clean,
    aes(x = UDwMeanLongitude, y = UDwMeanLatitude, color = REALM),
    size = 2, alpha = 0.7
  ) +
  scale_color_manual(values = okabe_ito) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Realm"
  )+
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.background = element_rect(fill = NA),  # transparent panel
    plot.background = element_rect(fill = NA),   # transparent overall plot
    legend.background = element_rect(fill = NA, color = NA), # transparent legend background
    legend.key = element_rect(fill = NA, color = NA)         # transparent key boxes
  )


# Figure 3b. Average UD size (Km^2) per species
okabe_ito_fig1c <- c(
  Afrotropic  = "#B85C00",
  Indomalayan = "#00714A",
  Nearctic    = "#7A4900",
  Neotropic   = "#781C81",
  Palearctic  = "#1B4F72"
)

# Futher data formatting for the Figure 3b
figure3b_raw <- final_df_clean %>%
  filter(!is.na(UDsizeKm2_99)) %>%
  mutate(
    REALM = factor(REALM, levels = names(okabe_ito_fig1c))
  ) %>%
  group_by(species, REALM) %>%
  mutate(avg_ud = mean(UDsizeKm2_99, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(REALM, avg_ud) %>%
  # order_id per species-REALM combination
  mutate(order_id = as.integer(factor(paste(species, REALM, sep = "_"),
                                      levels = unique(paste(species, REALM, sep = "_")))))

figure3b_raw <- figure3b_raw %>%
  mutate(
    # unique factor per species–realm for plotting
    species_factor = factor(paste0(species, "_", REALM),
                            levels = unique(paste0(species, "_", REALM)[order(order_id)])),
    # y-axis labels: only species names
    species_label = species
  )

# Build background rectangles for each realm block ----
bg_df <- figure3b_raw %>%
  distinct(order_id, REALM) %>%
  group_by(REALM) %>%
  summarise(
    ymin = min(order_id) - 0.5,
    ymax = max(order_id) + 0.5,
    .groups = "drop"
  )


# Figure 3b 
fig3b <- ggplot(figure3b_raw,
                aes(y = species_factor,  # unique factor for plotting
                    x = UDsizeKm2_99)) + # numeric x-axis
  
  # Realm background rectangles
  geom_rect(data = bg_df,
            aes(xmin = 0,
                xmax = max(figure1b_raw$UDsizeKm2_99) + 2000,
                ymin = ymin,
                ymax = ymax,
                fill = REALM),
            alpha = 0.5,
            inherit.aes = FALSE) +
  
  # Boxplots
  geom_boxplot(
    fill = "gray",
    color = "black",
    width = 0.7,
    outlier.size = 0.8
  ) +
  
  # Colors
  scale_fill_manual(values = okabe_ito_fig1c) +
  
  # Axis labels
  labs(x = "UD size (km\u00B2)", y = NULL) +
  
  # Show only species names on y-axis
  scale_y_discrete(labels = figure1b_raw$species_label[!duplicated(figure1b_raw$species_factor)]) +
  
  # Theme tweaks
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, margin = margin(r = 1)),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15)),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA)
  )