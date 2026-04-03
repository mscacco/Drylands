# ---
# Title: Dryland indicator annotation
# Author: Dongmin (Dennis) Kim, UMN; Harvard
# Date: April 2026
# Description: Annotate and merge the dryland indicators to the final data with 
#             annotated NDVI and livestock values
# ---

# call libraries 
library(here)
library(MASS)
library(nlme)
library(mgcv)
library(dplyr)
library(tidyr)
library(lubridate)
library(terra)
library(sf)

genPath <- "/path_to_folder/"

# 1. load the dataset with annotated NDVI and livestock values 
final_df <- readRDS(here::here("/path_to_folder/", "Final_dryland_data_jan2026.rds"))

# factorize the cateogrical columns (animal_id, REALM, BIOME_NAME) 
final_df$REALM <- as.factor(final_df$REALM)
final_df$animal_id <- as.factor(final_df$animal_id)


# 2. call dryland shapefile
dryland <- sf::st_read(here::here("/path_to_folder/", "drylands_UNCCD_CBD_july2014.shp"))

# fix invalid geometries 
dryland <- st_make_valid(dryland)

# convert to final data to spatial points 
pts <- st_as_sf(final_df, coords = c("UDwMeanLongitude", "UDwMeanLatitude"), crs = 4326)

# Match CRS
pts <- st_transform(pts, st_crs(dryland))

# Spatial join
pts_joins <- st_join(pts, dryland)

# label non-dryland group
pts_joins$HIX_DESC[!pts_joins$HIX_DESC %in% c("Arid", "Dry subhumid", "Hyperarid", "Semiarid")] <- "Non-dryland"

# extract coordinates into separate columns
coords <- st_coordinates(pts_joins)
pts_joins$UDwMeanLongitude <- coords[,1]  # X
pts_joins$UDwMeanLatitude  <- coords[,2]  # Y

# 3. drop the geometry column to make it a regular data frame
final_df_clean <- st_set_geometry(pts_joins, NULL)

saveRDS(final_df_clean, "/Final_dryland_data_mar2026.rds")