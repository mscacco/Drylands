# ---
# Title: GAM fitting
# Author: Dongmin (Dennis) Kim, UMN; Harvard
# Date: April 2026
# Description: Fit the final models
# ---

# call libraries 
library(here)
library(MASS)
library(nlme)
library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)

genPath <- "/path_to_folder/"

final_df <- readRDS(here::here(genPath, "Final_dryland_data_mar2026.rds"))

# 1. Finalized Model format
vulture_model <- bam(
  dsl_UD ~
    # Fixed effects:
    # Species-specific intercepts (baseline UD differences)
    # and REALM-level differences shared across species
    # and dryland-level differences shared across species
    species + REALM+ HIX_DESC+
    
    # Environmental predictors:
    # Species-specific smooths allow each species
    # to respond differently (nonlinearly) to NDVI
    s(dsl_wmNDVI, by = species, k = 5) +
    
    # Species-specific nonlinear response to livestock density
    s(wmLifestock, by = species, k = 5) +
    
    # Sampling effort correction:
    # Nonlinear effect of number of locations per month,
    # allowed to differ by species (NOT a random effect)
    # This controls for effort-induced bias in UD estimation
    s(totNbLocs, by = species, k =5)+
    
    # Spatial structure:
    # 2D smooth over centroid longitude/latitude,
    # with separate spatial surfaces for each species
    # (accounts for broad-scale spatial autocorrelation)
    s(UDwMeanLongitude, UDwMeanLatitude, by = species, k = 100) +
    
    # Individual-level random intercept:
    # Accounts for repeated measures within animals
    # (IDs can repeat across species)
    s(animal_id, bs = "re"),
  
  data = final_df,
  family = gaussian(),
  method = "fREML",   # stable & efficient
  discrete = TRUE     # good for large datasets
)

# 2. Finalized Model format: dryland
dryland_model <- bam(
  dsl_UD ~
    # Fixed effects:
    # Species-specific intercepts (baseline UD differences)
    # and REALM-level differences shared across species
    # and dryland-level differences shared across species
    species + REALM+ HIX_DESC+
    
    # Environmental predictors:
    # Species-specific smooths allow each species
    # to respond differently (nonlinearly) to NDVI
    s(dsl_wmNDVI, by = interaction(species, HIX_DESC),  k = 5) +
    
    # Species-specific nonlinear response to livestock density
    s(wmLifestock, by = interaction(species, HIX_DESC),  k = 5) +
    
    # Sampling effort correction:
    # Nonlinear effect of number of locations per month,
    # allowed to differ by species (NOT a random effect)
    # This controls for effort-induced bias in UD estimation
    s(totNbLocs, by = species, k =5)+
    
    # Spatial structure:
    # 2D smooth over centroid longitude/latitude,
    # with separate spatial surfaces for each species
    # (accounts for broad-scale spatial autocorrelation)
    s(UDwMeanLongitude, UDwMeanLatitude, by = species, k = 100) +
    
    # Individual-level random intercept:
    # Accounts for repeated measures within animals
    # (IDs can repeat across species)
    s(animal_id, bs = "re"),
  
  data = final_df,
  family = gaussian(),
  method = "fREML",   # stable & efficient
  discrete = TRUE     # good for large datasets
)

# save the final model and modified data frame 
saveRDS(dryland_model, file="dryland_model_mar2026.rds")
saveRDS(vulture_model, file = "vulture_model_mar2026.rds")