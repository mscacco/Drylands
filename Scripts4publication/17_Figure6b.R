# ---
# Title: Plot Figures 6b
# Author: Dongmin (Dennis) Kim, UMN; Harvard
# Date: April 2026
# Description: Figure 6b from the fitted GAM model per species
# ---

# call libraries 
library(here)
library(MASS)
library(nlme)
library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

genPath <- "/path_to_folder/"

# load data 
final_df <- readRDS(here::here(genPath, "Final_dryland_data_mar2026.rds"))
model <- readRDS(here::here(genPath, "vulture_model_mar2026.rds"))

# factorize the cateogrical columns (animal_id, REALM, BIOME_NAME) 
final_df$REALM <- as.factor(final_df$REALM)
final_df$animal_id <- as.factor(final_df$animal_id)
final_df$HIX_DESC <- factor(final_df$HIX_DESC, level = c("Hyperarid", "Arid", "Semiarid", "Dry subhumid", "Non-dryland"))

# remove species that has small samples (4 species removed - now 13 species)
final_df1 <- final_df %>% dplyr::filter(! species %in% c("Cathartes burrovianus", "Gyps bengalensis", "Gyps rueppellii", "Sarcoramphus papa"))

# Remove NA rows and drop unused factor levels
final_df1 <- final_df1 %>% dplyr::select(species, animal_id, REALM, year, month, UDsizeKm2_99, dsl_UD, wmNDVI, dsl_wmNDVI, wmLifestock, totNbLocs, HIX_DESC, UDwMeanLatitude, UDwMeanLongitude) %>% 
  filter(!is.na(REALM) & REALM != "N/A") %>%
  drop_na() %>%
  droplevels()


# Create Prediction Grids

# this is optional -- to group them by new world vs old world species.
new_world <- c("Vultur gryphus", "Cathartes aura", "Coragyps atratus")

## Unique species and REALMs
valid_pairs <- final_df1 %>%
  mutate(world = if_else(species %in% new_world, "New World", "Old World")) %>%
  distinct(species, REALM, world)

## Sequences for NDVI and Livestock
ndvi_seq <- seq(min(final_df1$dsl_wmNDVI), max(final_df1$dsl_wmNDVI), length.out = 1000)
lifestock_seq <- seq(min(final_df1$wmLifestock), max(final_df1$wmLifestock), length.out = 1000)

## Species-specific median coordinates
species_coords <- final_df1 %>%
  group_by(species) %>%
  summarize(
    lon_med = median(UDwMeanLongitude),
    lat_med = median(UDwMeanLatitude),
    .groups = "drop"
  )

## Choose a known animal_id and dryland (just one to avoid NA)
ref_animal <- levels(final_df1$animal_id)[1]
ref_dryland <- levels(final_df1$HIX_DESC)[1]

# 2. Build NDVI grid
pred_ndvi_grid <- valid_pairs %>%
  left_join(species_coords, by = c("species")) %>%
  tidyr::crossing(dsl_wmNDVI = ndvi_seq) %>%
  mutate(
    wmLifestock = median(final_df1$wmLifestock),
    UDwMeanLongitude = lon_med,
    UDwMeanLatitude  = lat_med,
    totNbLocs = median(final_df1$totNbLocs),
    animal_id = ref_animal,
    HIX_DESC = ref_dryland
  ) %>%
  dplyr::select(-lon_med, -lat_med)

# 3. Build Livestock grid
pred_LS_grid <- valid_pairs %>%
  left_join(species_coords, by = c("species")) %>%
  tidyr::crossing(wmLifestock = lifestock_seq) %>%
  mutate(
    dsl_wmNDVI = median(final_df1$dsl_wmNDVI),
    UDwMeanLongitude = lon_med,
    UDwMeanLatitude  = lat_med,
    totNbLocs = median(final_df1$totNbLocs),
    animal_id = ref_animal,
    HIX_DESC = ref_dryland
  ) %>%
  dplyr::select(-lon_med, -lat_med)

# Match factor levels
pred_ndvi_grid$species <- factor(pred_ndvi_grid$species, levels = levels(final_df1$species))
pred_ndvi_grid$animal_id <- factor(pred_ndvi_grid$animal_id, levels = levels(final_df1$animal_id))
pred_ndvi_grid$HIX_DESC <- factor(pred_ndvi_grid$HIX_DESC, levels = levels(final_df1$HIX_DESC))

pred_LS_grid$species <- factor(pred_LS_grid$species, levels = levels(final_df1$species))
pred_LS_grid$animal_id <- factor(pred_LS_grid$animal_id, levels = levels(final_df1$animal_id))
pred_LS_grid$HIX_DESC <- factor(pred_LS_grid$HIX_DESC, levels = levels(final_df1$HIX_DESC))

# Predict NDVI
predictions_ndvi <- predict(model, newdata = pred_ndvi_grid, se.fit = TRUE, re.form = NA)
pred_ndvi_grid$fit <- predictions_ndvi$fit
pred_ndvi_grid$se  <- predictions_ndvi$se.fit

# Predict Livestock
predictions_LS <- predict(model, newdata = pred_LS_grid, se.fit = TRUE, re.form = NA)
pred_LS_grid$fit <- predictions_LS$fit
pred_LS_grid$se  <- predictions_LS$se.fit

# Add 95% confidence intervals
pred_ndvi_grid <- pred_ndvi_grid %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

pred_LS_grid <- pred_LS_grid %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

# use model output directly
smoothed_ndvi_ribbons <- pred_ndvi_grid %>%
  rename(
    fit_smooth   = fit,
    lower_smooth = lower,
    upper_smooth = upper
  )

smoothed_LS_ribbons <- pred_LS_grid %>%
  rename(
    fit_smooth   = fit,
    lower_smooth = lower,
    upper_smooth = upper
  )

smoothed_LS_clean <- smoothed_LS_ribbons_sig %>%
  distinct(species, wmLifestock, .keep_all = TRUE)


# Plot figure
# for 13 species
okabe_ito_13 <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
  "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3"
)

# Make sure species is a factor so the colors map correctly
smoothed_ndvi_clean$species <- factor(smoothed_ndvi_clean$species)

# Create a named vector to map colors to species
species_colors <- setNames(okabe_ito_13, levels(smoothed_ndvi_clean$species))

fig6b <- ggplot(smoothed_ndvi_clean) +
  geom_ribbon(
    aes(
      x = dsl_wmNDVI,
      ymin = lower_smooth,
      ymax = upper_smooth,
      fill = species  # map fill to species
    ),
    alpha = 0.2
  ) +
  geom_line(
    aes(
      x = dsl_wmNDVI,
      y = fit_smooth,
      color = species  # map line color to species
    ),
    size = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "Change in NDVI (weighted mean)",
    y = expression("Change in UD Size (km²)")) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  facet_wrap(~ species, scales = "free", ncol = 4) +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors)

# you can do the same workflow for visualilzing the space use change against predicted livestock ranges.