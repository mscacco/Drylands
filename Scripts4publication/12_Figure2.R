# ---
# Title: Plot Figure 2
# Author: Dongmin (Dennis) Kim, UMN; Harvard
# Date: April 2026
# Description: Visualization: Figure 2
# ---

# call libraries
library(rnaturalearth)
library(ggspatial)
library(ggmapcn)
library(ggplot2)
library(patchwork)

genPath <- "/path_to_folder/"

# read the final data
final_df1 <- readRDS(here::here(genPath, "Final_dryland_data_mar2026.rds"))

# remove species that has small samples (4 species removed - now 13 species)
final_df1 <- final_df_clean %>% dplyr::filter(! species %in% c("Cathartes burrovianus", "Gyps bengalensis", "Gyps rueppellii", "Sarcoramphus papa"))

# Load the map of terrestrial realms
map <- ne_download(scale = 50, type = "land", category = "physical", returnclass = "sf")

# Figure 2a
fig2a <- ggplot() +
  # Graticule
  annotation_graticule(
    lon_step = 60, lat_step = 30,
    line_color = "gray70", line_type = "dashed",
    label_color = "gray30"
  ) +
  # Base land map
  geom_sf(data = map, fill = "white", color = "black", size = 0.2) +
  # Dryland polygons outline (no fill)
  geom_sf(
    data = dryland,
    fill = "#7A4900",
    color = "#7A4900",
    size = 0.2,
    alpha = 0.3
  ) +
  # Points colored by dryland_bin
  geom_point(
    data = final_df1,
    aes(x = UDwMeanLongitude, y = UDwMeanLatitude, color = HIX_DESC),
    size = 3, alpha = 0.5
  ) +
  # Color-blind friendly palette for points
  scale_color_manual(
    values = c("Hyperarid" = "#E41A1C",
               "Arid" = "#FF7F00",
               "Semiarid" = "#984EA3",
               "Dry subhumid" = "#377EB8",
               "Non-dryland" = "#4DAF4A"
    )) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Drylands"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA)
  )


# Figure 2b

# set color-palettes
dryland_colors <- c(
  "Hyperarid" = "#E41A1C",
  "Arid" = "#FF7F00",
  "Semiarid" = "#984EA3",
  "Dry subhumid" = "#377EB8",
  "Non-dryland" = "#4DAF4A"
)

# further data wrangling for the Figure 2b
figure2b_raw <- final_df1 %>%
  filter(!is.na(UDsizeKm2_99)) %>%
  mutate(
    HIX_DESC = factor(HIX_DESC, levels = names(dryland_colors))
  ) %>%
  group_by(HIX_DESC) %>%
  mutate(avg_ud = mean(UDsizeKm2_99, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(HIX_DESC, avg_ud) %>%
  # order_id per species-REALM combination
  mutate(order_id = as.integer(factor(paste(HIX_DESC),
                                      levels = unique(paste(HIX_DESC)))))

figure2b_raw <- figure2b_raw %>%
  mutate(
    # unique factor per species–realm for plotting
    dryland_factor = factor(paste0(HIX_DESC),
                            levels = unique(paste0(HIX_DESC)[order(order_id)])),
    # y-axis labels: only species names
    dryland_label = HIX_DESC
  )

# Build background rectangles for each realm block ----
bg_df <- figure2b_raw %>%
  distinct(order_id, HIX_DESC) %>%
  group_by(HIX_DESC) %>%
  summarise(
    ymin = min(order_id) - 0.5,
    ymax = max(order_id) + 0.5,
    .groups = "drop"
  )

# plot Figure 2b
fig2b <-  ggplot(figure1b_raw,
                 aes(y = dryland_factor,  # unique factor for plotting
                     x = UDsizeKm2_99)) + # numeric x-axis
  
  # Realm background rectangles
  geom_rect(data = bg_df,
            aes(xmin = 0,
                xmax = max(figure1b_raw$UDsizeKm2_99) + 2000,
                ymin = ymin,
                ymax = ymax,
                fill = HIX_DESC),
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
  scale_fill_manual(values = dryland_colors) +
  
  # Axis labels
  labs(x = "UD size (km\u00B2)", y = NULL) +
  
  # Show only species names on y-axis
  #scale_y_discrete(labels = figure1b_raw$species_label[!duplicated(figure1b_raw$species_factor)]) +
  
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
