
library(ggplot2)
library(viridis)
library(MASS)
library(mgcv)

# BOXCOX functions
BCTransform <- function(y, lambda=0) {
  if (lambda == 0L) { log(y) }
  else { (y^lambda - 1) / lambda }
}
BCTransformInverse <- function(yt, lambda=0) {
  if (lambda == 0L) { exp(yt) }
  else { exp(log(1 + lambda * yt)/lambda) }
}

dryPath <- "/home/mscacco/ownCloud/Martina/ProgettiVari/Drylands/DryLands/"
#dryPath <- "/home/mscacco/ownCloud - mscacco@ab.mpg.de@owncloud.gwdg.de/Martina/ProgettiVari/Drylands/DryLands/"

UD_ndvi_allYears <- readRDS(paste0(dryPath,"UDs_keeper/UDcentroidsWeight-monthlyNDVI_allIndividuals.rds"))

plot(monthMax_avgNdvi~UDsizeKm2, data=UD_ndvi_allYears[UD_ndvi_allYears$UDsizeKm2<=20000,])
unique(UD_ndvi_allYears$species[UD_ndvi_allYears$UDsizeKm2 > 80000])

### Scale covariates
UD_ndvi_allYears$individual_local_identifier <- as.factor(UD_ndvi_allYears$individual_local_identifier)
UD_ndvi_allYears$long_scale <- scale(UD_ndvi_allYears$UDwMeanLongitude_moll)[,1] #UDwMeanLongitude
UD_ndvi_allYears$lat_scale <- scale(UD_ndvi_allYears$UDwMeanLatitude_moll)[,1]
UD_ndvi_allYears$avgNdvi_scale <- scale(UD_ndvi_allYears$monthMean_avgNdvi)[,1]
UD_ndvi_allYears$ndvi_order_scale <- scale(UD_ndvi_allYears$ndvi_order)[,1]
UD_ndvi_allYears$locsPerMonth_scale <- scale(UD_ndvi_allYears$locsPerMonth)[,1]
UD_ndvi_allYears$UD_speciesAvg_scale <- scale(UD_ndvi_allYears$UD_speciesAvg)[,1]
UD_ndvi_allYears$nUds_perSpAvg_scale <- scale(UD_ndvi_allYears$nUds_perSpAvg)[,1]
  
### Remove observations with missing ndvi (here just 1)
UD_ndvi_allYears <- UD_ndvi_allYears[!is.na(UD_ndvi_allYears$monthMean_avgNdvi),]

#_____________________
### Model UD size ----

hist(UD_ndvi_allYears$UDsizeKm2, breaks="FD")
hist(log(UD_ndvi_allYears$UDsizeKm2), breaks="FD")
quantile(UD_ndvi_allYears$UDsizeKm2, seq(.9,1,0.0001))
quantile(UD_ndvi_allYears$UDsizeKm2, seq(0,.2,0.0001))
plot(ecdf(UD_ndvi_allYears$UDsizeKm2))
# not happy with log, find different transformation
bc_result <- boxcox(UD_ndvi_allYears$UDsizeKm2 ~ 1)
print(bc_result)
# Find the lambda that maximizes the likelihood
lambda_max <- bc_result$x[which.max(bc_result$y)]
# Apply the Box-Cox transformation with the found lambda
UD_ndvi_allYears$UDsizeKm2_boxc <- BCTransform(UD_ndvi_allYears$UDsizeKm2, lambda_max)
testBC <- BCTransformInverse(UD_ndvi_allYears$UDsizeKm2_boxc, lambda_max)
summary(round(UD_ndvi_allYears$UDsizeKm2 - testBC, 8))

hist(UD_ndvi_allYears$UDsizeKm2_boxc)

# GAMS (use bam, same as gam but for large datasets)

mod_udSize <- bam(UDsizeKm2_boxc ~ 
              s(avgNdvi_scale) + #, by=species
              s(long_scale, lat_scale, by=ndvi_order_scale) + #spatial autocorrelation
              ndvi_order_scale + #temporal autocorrelation
              locsPerMonth_scale + #to account for UD size varying depending on number of daily locations
              UD_speciesAvg_scale + # account for species differences in UD
             s(species, bs = 're') + s(species,individual_local_identifier, bs = 're'), #random effects, ind nested in species
           data=UD_ndvi_allYears,
           family=gaussian)
summary(mod_udSize)
plot(mod_udSize, page=1)
mod_udSize$sp
varcomp.gam(mod_udSize)

mod_udSize2 <- bam(UDsizeKm2_boxc ~ 
                    avgNdvi_scale * species +
                    s(long_scale, lat_scale, by=ndvi_order_scale) + #spatial autocorrelation
                    ndvi_order_scale + #temporal autocorrelation
                    locsPerMonth_scale + #to account for UD size varying depending on number of daily locations
                    UD_speciesAvg_scale + # account for species differences in UD
                    s(individual_local_identifier, bs = 're'), #random effects, ind nested in species
                  data=UD_ndvi_allYears,
                  family=gaussian)
summary(mod_udSize2)

#____________________________________________________________
### Model percentual difference from species' average UD ----

hist(UD_ndvi_allYears$diff_fromAvgUd_perc, breaks="FD")
hist(sqrt(UD_ndvi_allYears$diff_fromAvgUd_perc+100), breaks="FD")
plot(ecdf(UD_ndvi_allYears$diff_fromAvgUd_perc))
quantile(UD_ndvi_allYears$diff_fromAvgUd_perc, seq(.9,1,0.0001))
quantile(UD_ndvi_allYears$diff_fromAvgUd_perc, seq(0,.2,0.001))
# GAMS
mod_udDiff <- bam(sqrt(diff_fromAvgUd_perc +100) ~
                    s(avgNdvi_scale) + #, by=species
                    s(long_scale, lat_scale, by=ndvi_order_scale) + #spatial autocorrelation
                    ndvi_order_scale + #temporal autocorrelation
                    locsPerMonth_scale + #to account for UD size varying depending on number of daily locations
                    nUds_perSpAvg_scale + #to account for the fact that differences are likely to be bigger if the mean was calculated from more individuals
                    s(species, bs="re") + s(species, individual_local_identifier, bs="re"), #(random factors)
                    data=UD_ndvi_allYears,
                    #method="GCV.Cp" #default, otherwise method="REML"
                    na.action=na.fail) #by default
summary(mod_udDiff)
plot(mod_udDiff, page=1)
mod_udDiff$sp
varcomp.gam(mod_udDiff)

mod_udDiff2 <- bam(sqrt(diff_fromAvgUd_perc +100) ~
                    avgNdvi_scale * species +
                    s(long_scale, lat_scale, by=ndvi_order_scale) + #spatial autocorrelation
                    ndvi_order_scale + #temporal autocorrelation
                    locsPerMonth_scale + #to account for UD size varying depending on number of daily locations
                    nUds_perSpAvg_scale + #to account for the fact that differences are likely to be bigger if the mean was calculated from more individuals
                    s(individual_local_identifier, bs="re"), #(random factors)
                  data=UD_ndvi_allYears,
                  #method="GCV.Cp" #default, otherwise method="REML",
                  family=gaussian,
                  na.action=na.fail) #by default
summary(mod_udDiff2)
hist(residuals(mod_udDiff2))

### Questions:
# - should we filter out huge UD sizes? What is a reasonable UD size? 80'000 km2 seems like a lot
# - do we need both species and individual as random effect, even when calculating difference from specific mean?
# - are transformations ok?
# - does it make sense to compare percentual change? What about difference standardised between 0 and 1?


### Save both models
save(mod_udSize, mod_udDiff, lambda_max, file=paste0(dryPath,"Results&Figures/models_UD_ndviSmooth.RData"))
save(mod_udSize2, mod_udDiff2, lambda_max, file=paste0(dryPath,"Results&Figures/models_UD_ndviLinear.RData"))

#__________________
### Visualizations

library(data.table)
library(terra)
library(rasterVis)
# devtools::install_github("m-clark/gammit")
# library(gammit)

### First effect plot of NDVI for the different species

# Create dummy dataset with covariates equal for all species
dummyDF <- data.frame(avgNdvi=seq(-0.2,1,0.05), avgNdvi_scale=scale(seq(-0.2,1,0.05))) # sequence of scaled NDVI values
UD_ndvi_allYears[sample(1:nrow(UD_ndvi_allYears), 1),c("long_scale","lat_scale")] #sample a random location
dummyDF$long_scale <- 0.7323096
dummyDF$lat_scale <- -2.676833
dummyDF$ndvi_order_scale <- mean(UD_ndvi_allYears$ndvi_order_scale)
dummyDF$locsPerMonth_scale <- mean(UD_ndvi_allYears$locsPerMonth_scale)
dummyDF$nUds_perSpAvg_scale <- mean(UD_ndvi_allYears$nUds_perSpAvg_scale)
# now add species-specific variables
dummyDF_perSpec <- aggregate(UD_speciesAvg_scale~species, data=UD_ndvi_allYears, FUN=unique)
names(dummyDF_perSpec)[2] <- "UD_speciesAvg_scale"
dummyDF_final <- rbindlist(lapply(1:nrow(dummyDF_perSpec), function(row){
  return(cbind(dummyDF, dummyDF_perSpec[row,]))
}))
nrow(dummyDF_final) == nrow(dummyDF) * nrow(dummyDF_perSpec)
# Now we can predict the two response variables based on this dummy DF
# Check this page for predicting with random effects: 
# https://stackoverflow.com/questions/61595497/how-to-predict-gam-model-with-random-effect-in-r 
# By excluding the random effect column and adding the argument newdata.guaranteed=T
# there is no need to specify the random effect column in the newdata
dummyDF_final$UDsizeKm2_boxc <- predict.gam(mod_udSize2, dummyDF_final, #type="response",
                                             exclude =  's(individual_local_identifier)',
                                             newdata.guaranteed = TRUE)
dummyDF_final$diff_fromAvgUd_perc_sqrt100 <- predict.gam(mod_udDiff2, dummyDF_final, #type = "response",
                                            exclude =  's(individual_local_identifier)',
                                            newdata.guaranteed = TRUE)
# and backtransform
dummyDF_final$UDsizeKm2 <- BCTransformInverse(dummyDF_final$UDsizeKm2_boxc, lambda_max)
dummyDF_final$diff_fromAvgUd_perc <- (dummyDF_final$diff_fromAvgUd_perc_sqrt100^2)-100

summary(dummyDF_final$UDsizeKm2)
summary(UD_ndvi_allYears$UDsizeKm2)

summary(dummyDF_final$diff_fromAvgUd_perc)
summary(UD_ndvi_allYears$diff_fromAvgUd_perc)

## Finally the effect plot
unique(dummyDF_final$species)
### remove this species that for some reason is a big outlier, more exploration needed
dummySub <- dummyDF_final[dummyDF_final$species!="Torgos tracheliotus",]

ggplot(dummySub, aes(x=avgNdvi, y=UDsizeKm2, group=species, col=species)) +
  geom_line() +
  xlab("Average monthly NDVI") + ylab("UD size (Km2)") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  # + guides(color = FALSE)
ggsave(paste0(dryPath,"Results&Figures/effectPlot_NDVI-UDsize.pdf"))
  
ggplot(dummySub, aes(x=avgNdvi, y=diff_fromAvgUd_perc, group=species, col=species)) +
  geom_line() +
  xlab("Average monthly NDVI") + ylab("% different from average species' UD size") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  # + guides(color = FALSE)
ggsave(paste0(dryPath,"Results&Figures/effectPlot_NDVI-UDdiff.pdf"))


### Now prediction maps using NDVI time series for 2020

# Import template raster and monthly NDVI for year 2020
templ_rast <- rast("./TemplateRaster_100km2.tif")
#latlongDF <- xyFromCell(templ_rast, 1:ncell(templ_rast))

ndviPath <- "/media/mscacco/6EB4C1E7B4C1B23F/NDVI/NDVI_finalSummaries/"
ndviFls <- list.files(ndviPath, pattern="monthlyDF_year2019", full.names = T)

ndvi <- readRDS(ndviFls)
ndvi$avgNdvi_scale <- scale(ndvi$monthMean_avgNdvi)[,1]
ndvi$long_scale <- scale(ndvi$x)[,1]
ndvi$lat_scale <- scale(ndvi$y)[,1]
ndvi_ls <- split(ndvi, ndvi$month)
names(ndvi_ls)

# Create a prediction DF for each month of 2019, by adding the remaining variables to the dataframe
# dummy_lessCols is the same as above but without ndvi lat and long, as these will be taken from the real ndvi raster
dummy_lessCols <- aggregate(cbind(locsPerMonth_scale, nUds_perSpAvg_scale, UD_speciesAvg_scale)~species, 
                      data=dummySub, FUN=unique)

predDF_allMonths_allSpec <- rbindlist(lapply(1:length(ndvi_ls), function(i){
  ndvi_ls[[i]]$ndvi_order_scale <- i
  dfMonth <- rbindlist(lapply(1:nrow(dummy_lessCols), function(j){
    return(cbind(ndvi_ls[[i]], dummy_lessCols[j,]))
  }))
  return(dfMonth)
}))
# predict response variables
predDF_allMonths_allSpec$UDsizeKm2_boxc <- predict(mod_udSize2, predDF_allMonths_allSpec,
                                                   exclude =  's(individual_local_identifier)',
                                                   newdata.guaranteed = TRUE)
predDF_allMonths_allSpec$diff_fromAvgUd_perc_sqrt100 <- predict(mod_udDiff2, predDF_allMonths_allSpec,
                                                                exclude =  's(individual_local_identifier)',
                                                                newdata.guaranteed = TRUE)
# and backtransform
predDF_allMonths_allSpec$UDsizeKm2 <- BCTransformInverse(predDF_allMonths_allSpec$UDsizeKm2_boxc, lambda_max)
predDF_allMonths_allSpec$diff_fromAvgUd_perc <- (predDF_allMonths_allSpec$diff_fromAvgUd_perc_sqrt100^2)-100

# Rasterize only the two backtransformed response variables 
# and lat and long in their original scale for plotting the maps
# we rasterize per month and per species
predRast_SPlist <- split(predDF_allMonths_allSpec, as.character(predDF_allMonths_allSpec$species))

monthlyStack_PerSpecies_UDsize <- lapply(predRast_SPlist, function(sp){
  ls <- split(sp, sp$month)
  sp_stack <- rast(lapply(ls, function(month){
    v <- vect(month[,c("x","y","UDsizeKm2")], geom=c("x","y"))
    ndviRast <- rasterize(v, templ_rast, field="UDsizeKm2")
    #ndviRast <- rast(month[,c("x","y","UDsizeKm2")], type="xyz", crs=crs(templ_rast))
    return(ndviRast)
  }))
  return(sp_stack)
})

monthlyStack_PerSpecies_UDdiff <- lapply(predRast_SPlist, function(sp){
  ls <- split(sp, sp$month)
  sp_stack <- rast(lapply(ls, function(month){
    v <- vect(month[,c("x","y","diff_fromAvgUd_perc")], geom=c("x","y"))
    ndviRast <- rasterize(v, templ_rast, field="diff_fromAvgUd_perc")
    #ndviRast <- rast(month[,c("x","y","diff_fromAvgUd_perc")], type="xyz", crs=crs(templ_rast))
     return(ndviRast)
  }))
  return(sp_stack)
})

plot(monthlyStack_PerSpecies_UDsize[[7]])



