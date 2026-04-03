# ---
# Title: Plot Figures 4b
# Author: Dongmin (Dennis) Kim, UMN; Harvard
# Date: April 2026
# Description: Figure 4b from the fitted GAM model per REALM
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
ndvi_seq <- seq(min(final_df1$dsl_wmNDVI), max(final_df1$dsl_wmNDVI), length.out = 100)
lifestock_seq <- seq(min(final_df1$wmLifestock), max(final_df1$wmLifestock), length.out = 100)

## Species–REALM-specific median coordinates
species_coords <- final_df1 %>%
  group_by(species, REALM) %>%
  summarize(
    lon_med = median(UDwMeanLongitude),
    lat_med = median(UDwMeanLatitude),
    .groups = "drop"
  )

## Choose a known animal_id and dryland (just one to avoid NA)
ref_animal <- levels(final_df1$animal_id)[1]
ref_dryland <- levels(final_df1$HIX_DESC)[1]

# Build NDVI grid
pred_ndvi_grid <- valid_pairs %>%
  left_join(species_coords, by = c("species", "REALM")) %>%
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

# Build Livestock grid
pred_LS_grid <- valid_pairs %>%
  left_join(species_coords, by = c("species", "REALM")) %>%
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
pred_ndvi_grid$REALM <- factor(pred_ndvi_grid$REALM, levels = levels(final_df1$REALM))
pred_ndvi_grid$animal_id <- factor(pred_ndvi_grid$animal_id, levels = levels(final_df1$animal_id))
pred_ndvi_grid$HIX_DESC <- factor(pred_ndvi_grid$HIX_DESC, levels = levels(final_df1$HIX_DESC))

pred_LS_grid$species <- factor(pred_LS_grid$species, levels = levels(final_df1$species))
pred_LS_grid$REALM <- factor(pred_LS_grid$REALM, levels = levels(final_df1$REALM))
pred_LS_grid$animal_id <- factor(pred_LS_grid$animal_id, levels = levels(final_df1$animal_id))
pred_LS_grid$HIX_DESC <- factor(pred_LS_grid$HIX_DESC, levels = levels(final_df1$HIX_DESC))

## get predictions from the model 
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

# FIXED — use model output directly (NO LOESS)
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


# Plot
# REALM grouping visualization

# Okabe-Ito palette (first 5 colors)
okabe_ito <- c(
  "#B85C00", # dark orange (unchanged)
  "#00714A", # dark bluish green (kept, very distinct)
  "#7A4900", # warm dark brown (replaces gold/yellow)
  "#781C81", # dark purple (strong contrast with others)
  "#1B4F72"  # deep steel blue (unique, darker and not similar to sky blue)
)

# Apply loess smoothing per species
smoothed_ndvi_realm_summary <- smoothed_ndvi_ribbons %>%
  group_by(REALM, dsl_wmNDVI) %>%
  summarise(
    fit_smooth   = mean(fit_smooth),
    lower_smooth = mean(lower_smooth),
    upper_smooth = mean(upper_smooth),
    .groups = "drop"
  )

smoothed_LS_realm_ribbons <- pred_LS_grid %>%
  # Create the *_smooth columns from model predictions
  mutate(
    fit_smooth   = fit,
    lower_smooth = lower,
    upper_smooth = upper
  ) %>%
  group_by(REALM, wmLifestock) %>%
  summarise(
    fit_smooth   = mean(fit_smooth),
    lower_smooth = mean(lower_smooth),
    upper_smooth = mean(upper_smooth),
    .groups = "drop"
  )

# plot the figure 4b
fig4b <- ggplot() +
  geom_ribbon(
    data = smoothed_ndvi_realm_summary,
    aes(x = dsl_wmNDVI, ymin = lower_smooth, ymax = upper_smooth, fill = REALM),
    alpha = 0.1, color = NA
  ) +
  geom_line(
    data = smoothed_ndvi_realm_summary,
    aes(x = dsl_wmNDVI, y = fit_smooth, color = REALM),
    size = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = okabe_ito) +
  scale_fill_manual(values = okabe_ito) +
  theme_minimal() +
  labs(
    x = "Change in NDVI (weighted mean)",
    y = expression("Change in UD Size (km²)"),
    color = "Realm",
    fill = "Realm"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 15)),  # space above x title
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  facet_wrap(~REALM)

# you can do the same workflow for visualilzing the space use change against predicted livestock ranges.