# ---
# Title: Plot Figures 5b
# Author: Dongmin (Dennis) Kim, UMN; Harvard
# Date: April 2026
# Description: Figure 5b from the fitted GAM model per dryland
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
final_df <- readRDS(here::here("data/0.final_data", "Final_dryland_data_mar2026.rds"))
model <- readRDS(here::here("data/0.final_data", "dryland_model_mar2026.rds"))

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

# 1. Create Prediction Grids
new_world <- c("Vultur gryphus", "Cathartes aura", "Coragyps atratus")

## Unique species and dryland indicators
valid_pairs <- final_df1 %>%
  mutate(world = if_else(species %in% new_world, "New World", "Old World")) %>%
  distinct(species, HIX_DESC, world)

## Sequences for NDVI and Livestock
ndvi_seq <- seq(min(final_df1$dsl_wmNDVI), max(final_df1$dsl_wmNDVI), length.out = 100)
lifestock_seq <- seq(min(final_df1$wmLifestock), max(final_df1$wmLifestock), length.out = 100)

## Species–dryland-specific median coordinates
species_coords <- final_df1 %>%
  group_by(species, HIX_DESC) %>%
  summarize(
    lon_med = median(UDwMeanLongitude),
    lat_med = median(UDwMeanLatitude),
    .groups = "drop"
  )

## Choose a known animal_id and dryland (just one to avoid NA)
ref_animal <- levels(final_df1$animal_id)[1]
ref_REALM <- levels(final_df1$REALM)[3]

# 2. Build NDVI grid
pred_ndvi_grid <- valid_pairs %>%
  left_join(species_coords, by = c("species", "HIX_DESC")) %>%
  tidyr::crossing(dsl_wmNDVI = ndvi_seq) %>%
  mutate(
    wmLifestock = median(final_df1$wmLifestock),
    UDwMeanLongitude = lon_med,
    UDwMeanLatitude  = lat_med,
    totNbLocs = median(final_df1$totNbLocs),
    animal_id = ref_animal,
    REALM = ref_REALM
  ) %>%
  dplyr::select(-lon_med, -lat_med)

# 3. Build Livestock grid
pred_LS_grid <- valid_pairs %>%
  left_join(species_coords, by = c("species", "HIX_DESC")) %>%
  tidyr::crossing(wmLifestock = lifestock_seq) %>%
  mutate(
    dsl_wmNDVI = median(final_df1$dsl_wmNDVI),
    UDwMeanLongitude = lon_med,
    UDwMeanLatitude  = lat_med,
    totNbLocs = median(final_df1$totNbLocs),
    animal_id = ref_animal,
    REALM = ref_REALM
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

# get predictions from the model
# Predict NDVI
predictions_ndvi <- predict(model, newdata = pred_ndvi_grid, se.fit = TRUE, re.form = NA)
pred_ndvi_grid$fit <- predictions_ndvi$fit
pred_ndvi_grid$se  <- predictions_ndvi$se.fit

# Predict Livestock
predictions_LS <- predict(model, newdata = pred_LS_grid, se.fit = TRUE, re.form = NA)
pred_LS_grid$fit <- predictions_LS$fit
pred_LS_grid$se  <- predictions_LS$se.fit

# Confidence intervals
pred_ndvi_grid <- pred_ndvi_grid %>%
  mutate(lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)

pred_LS_grid <- pred_LS_grid %>%
  mutate(lower = fit - 1.96 * se,
         upper = fit + 1.96 * se)

# Rename
smoothed_ndvi_ribbons <- pred_ndvi_grid %>%
  rename(fit_smooth = fit,
         lower_smooth = lower,
         upper_smooth = upper)

smoothed_LS_ribbons <- pred_LS_grid %>%
  rename(fit_smooth = fit,
         lower_smooth = lower,
         upper_smooth = upper)

# for 2 dryland groups
dryland_colors <- c(
  "Hyperarid" = "#E41A1C",
  "Arid" = "#FF7F00",
  "Semiarid" = "#984EA3",
  "Dry subhumid" = "#377EB8",
  "Non-dryland" = "#4DAF4A"
)

# summarise per species × HIX_DESC
smoothed_ndvi_dryland_summary <- smoothed_ndvi_ribbons %>%
  group_by(HIX_DESC, species, dsl_wmNDVI) %>%  # keep dsl_wmNDVI in group
  summarise(
    fit_smooth   = mean(fit_smooth),
    lower_smooth = mean(lower_smooth),
    upper_smooth = mean(upper_smooth),
    .groups = "drop"
  )

smoothed_LS_dryland_summary <- smoothed_LS_ribbons %>%
  group_by(HIX_DESC, species, wmLifestock) %>%
  summarise(
    fit_smooth   = mean(fit_smooth),
    lower_smooth = mean(lower_smooth),
    upper_smooth = mean(upper_smooth),
    .groups = "drop"
  )

# Plot the figure 5b

fig5b <- ggplot() +
  geom_ribbon(
    data = smoothed_ndvi_dryland_summary,
    aes(x = dsl_wmNDVI, ymin = lower_smooth, ymax = upper_smooth, fill = HIX_DESC),
    alpha = 0.15
  ) +
  geom_line(
    data = smoothed_ndvi_dryland_summary,
    aes(x = dsl_wmNDVI, y = fit_smooth, color = HIX_DESC),
    size = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = dryland_colors) +
  scale_fill_manual(values = dryland_colors) +
  theme_minimal() +
  labs(
    x = "Change in NDVI (weighted mean)",
    y = expression("Change in UD Size (km²)"),
    color = "Dryland",
    fill = "Dryland"
  ) +
  facet_wrap(~HIX_DESC)

figure_LS_dryland <- ggplot() +
  geom_ribbon(
    data = smoothed_LS_dryland_summary,
    aes(x = wmLifestock, ymin = lower_smooth, ymax = upper_smooth, fill = HIX_DESC),
    alpha = 0.15
  ) +
  geom_line(
    data = smoothed_LS_dryland_summary,
    aes(x = wmLifestock, y = fit_smooth, color = HIX_DESC),
    size = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = dryland_colors) +
  scale_fill_manual(values = dryland_colors) +
  theme_minimal() +
  labs(
    x = "Livestock density (weighted mean)",
    y = expression("Change in UD Size (km²)"),
    color = "Dryland",
    fill = "Dryland"
  ) +
  facet_wrap(~HIX_DESC)

# you can do the same framework for plotting the changes of UD sizes across the livestock ranges. 