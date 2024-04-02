
setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/AnEnvi_paper/Rscripts/AnEnvIPaper_Rproj/data_prep/EnvironmentalData")

hddPath <- "/media/mscacco/6EB4C1E7B4C1B23F"


library(terra)
library(data.table)
library(sf)
library(lubridate)
library(plyr)
library(doParallel)
detectCores()

#MODIS/Terra Vegetation Indices 16-Day L3 Global 1km SIN Grid V061
#For years 2000-2022

#_____________________________
#### NDVI VALID VALUES??  ----
## Simple test on valid range and scaling factor on data
## one ndvi tile
# t <- rast(list.files(years_fld[1], pattern="hdf", full.names=T)[150], lyrs=1)
# plot(t)
# t_scaled <- t * 0.0001
# plot(t_scaled)
# table(values(t < -2000))
# table(values(t > 10000))
# t[t < -2000 | t > 10000] <- NA
# t
# plot(t)

#____________________________________________
#### STITCH NDVI TILES IN GLOBAL RASTERS ----

years_fld <- list.files(paste0(hddPath,"/NDVI/NDVI_raw"), pattern="[0-9]", full.names = T)

# List all files from all yearly folders, with their path
allNDVIfls <- unlist(sapply(years_fld, list.files, pattern="hdf", full.names=T))

# Extract date and tile number
# in the file name, date is specified as julian day between the first and the second "." in the file name
jday <- sapply(strsplit(allNDVIfls, ".", fixed=T), "[", 2)
table(table(jday))
unique(jday)

tileName <- sapply(strsplit(allNDVIfls, ".", fixed=T), "[", 3)
table(table(tileName))

productName <- gsub(".*/","",sapply(strsplit(allNDVIfls, ".", fixed=T), "[", 1))
table(productName)

# Check content of one of the files
# to know the order of layers in each file
meta <- describe(allNDVIfls[1], sds=T)
# date
doy <- meta$id[meta$var == grep("day of the year", meta$var, value=T)]
# vegetation indexes: valid range -2000, 10000 | scale factor for integer 0.0001
ndviID <- meta$id[meta$var == grep("NDVI", meta$var, value=T)] 
eviID <- meta$id[meta$var == grep("EVI", meta$var, value=T)]
# quality assessment
# 00 VI produced with good quality #include
# 01 VI produced, but check other QA #include
# 10 Pixel produced, but most probably cloudy #exclude
# 11 Pixel not produced due to other reasons than clouds #exclude
# qualityId <- meta$id[meta$var == grep("VI Quality", meta$var, value=T)] #??
relId <- meta$id[meta$var == grep("reliability", meta$var, value=T)] #keep only 00 and 01?


# Import files per doy and stitch them
registerDoParallel(6)
done <- gsub(".*_|.tif", "", grep(list.files(paste0(hddPath,"/NDVI/NDVI_globalPerDOY")), pattern = "100km", invert=T, value=T))
toDo <- unique(jday)[!unique(jday) %in% done]
t0=Sys.time()
err <- llply(toDo, function(oneDay)try({
  print(oneDay)
  # grep file
  modFls <- grep(oneDay, allNDVIfls, value=T)
  modFls <- modFls[!duplicated(gsub(".*/","",modFls))]
  # extract single layers
  ndviLs <- lapply(modFls, rast, lyrs=ndviID)
  relLs <- lapply(modFls, rast, lyrs=relId)
  
  # No idea what these DOY values represent
  # doyLs <- lapply(modFls, rast, lyrs=doy)

  # transform raster list in collection and then stitch
  ndvi <- terra::merge(sprc(ndviLs), gdal=c("BIGTIFF=YES"))
  pxRel <- terra::merge(sprc(relLs), gdal=c("BIGTIFF=YES"))
  #plot(ndvi); plot(pxRel)
  
  # stack and save
  rStack <- c(ndvi, pxRel)
  names(rStack) <- c("ndvi","pxReliability")
  writeRaster(rStack, f=paste0(hddPath,"/NDVI/NDVI_globalPerDOY/globalNDVI_doy_",oneDay,".tif"), overwrite=T)
}), .parallel = T)
Sys.time()-t0

#________________________________________________
#### RESAMPLE TO 100 km MEAN and IQ PER TILE ----

library(rnaturalearth)

# template raster and polygons
templ_rast <- rast("./TemplateRaster_100km2.tif")

# world boundaries
worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
worldMap <- st_transform(worldMap, crs(templ_rast))

# global ndvi files every 16 days
# pxRel: 2 = snow/ice = 0 ; 3 = cloud = NA
# vegetation indexes (NDVI EVI): scale factor for integer 0.0001 | ??valid range -2000, 10000??

fls <- list.files(paste0(hddPath,"/NDVI/NDVI_globalPerDOY"), full.names = T)
done <- gsub(".*doy_|_avg.*", "", list.files(paste0(hddPath,"/NDVI/NDVI_globalPerDOY"), pattern = "100kmTempl", full.names=T))
flsToDo <- grep(paste(done, collapse="|"), fls, invert=T, value=T)

registerDoParallel(5)
err <- lapply(flsToDo, function(f)try({
#err <- llply(flsToDo, function(f)try({
  rs <- rast(f)
  # remove values outside valid range ???
  # rs[["ndvi"]][rs[["ndvi"]] < -2000 | rs[["ndvi"]] > 10000] <- NA #valid range
  
  # mask snow and clouds
  rs[["ndvi"]][rs[["pxReliability"]] == 2] <- 0 # snow/ice
  rs[["ndvi"]][rs[["pxReliability"]] == 3] <- NA # cloud
  
  # metrics to compute (not the minimum because we would be selecting cloud)
  metrics <- c("q1","q3","average")
  # reproject global ndvi (1 km) to template raster (100 km) using above metrics and create a stack
  ndvi_proj <- rast(lapply(metrics, function(fun){
    project(rs[["ndvi"]], templ_rast, method = fun)}))
  # calculate inter quantile range
  IQ <- ndvi_proj[[2]] - ndvi_proj[[1]] # q3 - q1
  # bind and change names
  ndvi_proj <- c(ndvi_proj[[3]], IQ) # average and IQ
  names(ndvi_proj) <- c("ndvi_avg","ndvi_IQ")
  # mask values outside terrestrial regions with NA
  ndvi_proj <- mask(ndvi_proj, worldMap)
  # multiply by scale factor (bring between 0 and 1)
  ndvi_proj <- ndvi_proj*0.00000001
  # save reprojected stack with all metrics
  writeRaster(ndvi_proj, f=gsub(".tif", "_avg-IQ_100kmTempl.tif", f))
#}), .parallel=T)
}))

#_______________________________________________________________
#### CALCULATE METRICS PER YEAR/MONTH AND OUTPUT DATAFRAMES ----

fls <- list.files(paste0(hddPath,"/NDVI/NDVI_globalPerDOY"), pattern="100kmTempl", full.names = T)

# unique years
yrs <- unique(substr(gsub(".*doy_|_avg.*", "", fls), 2, 5))
table(substr(gsub(".*doy_|_avg.*", "", fls), 2, 5))

t0 <- Sys.time()
yearlyDF <- rbindlist(lapply(yrs, function(oneYear){
  yearFls <- grep(fls, pattern=oneYear, value=T)
  # extract images per month
  doys <- substr(gsub(".*doy_|_avg.*", "", yearFls), 6, 8)
  dates <- as.Date(as.numeric(doys), origin = as.Date(paste0(oneYear,"-01-01")))
  months <- month(dates)
  
  monthlyDF <- rbindlist(lapply(unique(months), function(m){
    monthStack <- rast(yearFls[months==m])
    # calculate monthly variables
    monthMean_avgNdvi <- mean(monthStack[[names(monthStack)=="ndvi_avg"]], na.rm=T)
    monthMin_avgNdvi <- min(monthStack[[names(monthStack)=="ndvi_avg"]], na.rm=T)
    monthMax_avgNdvi <- max(monthStack[[names(monthStack)=="ndvi_avg"]], na.rm=T)
    monthMean_avgIQ <- mean(monthStack[[names(monthStack)=="ndvi_IQ"]], na.rm=T)
    monthMin_avgIQ <- min(monthStack[[names(monthStack)=="ndvi_IQ"]], na.rm=T)
    monthMax_avgIQ <- max(monthStack[[names(monthStack)=="ndvi_IQ"]], na.rm=T)
    monthSummary <- c(monthMean_avgNdvi, monthMin_avgNdvi, monthMax_avgNdvi,
                      monthMean_avgIQ, monthMin_avgIQ, monthMax_avgIQ)
    names(monthSummary) <- c("monthMean_avgNdvi", "monthMin_avgNdvi", "monthMax_avgNdvi",
                             "monthMean_avgIQ", "monthMin_avgIQ", "monthMax_avgIQ")

    monthSummary_df <- as.data.frame(monthSummary, xy=T, cells=T)
    monthSummary_df$month <- m
    monthSummary_df$year <- oneYear
    return(monthSummary_df)
  }))
  saveRDS(monthlyDF, paste0(hddPath,"/NDVI/NDVI_finalSummaries/NDVI_monthlyDF_year",oneYear,".rds"))
  
  yearlyStack <- rast(yearFls)
  yearMean_avgNdvi <- mean(yearlyStack[[names(yearlyStack)=="ndvi_avg"]], na.rm=T)
  yearMin_avgNdvi <- min(yearlyStack[[names(yearlyStack)=="ndvi_avg"]], na.rm=T)
  yearMax_avgNdvi <- max(yearlyStack[[names(yearlyStack)=="ndvi_avg"]], na.rm=T)
  yearMean_avgIQ <- mean(yearlyStack[[names(yearlyStack)=="ndvi_IQ"]], na.rm=T)
  yearMin_avgIQ <- min(yearlyStack[[names(yearlyStack)=="ndvi_IQ"]], na.rm=T)
  yearMax_avgIQ <- max(yearlyStack[[names(yearlyStack)=="ndvi_IQ"]], na.rm=T)
  yearlySummary <- c(yearMean_avgNdvi, yearMin_avgNdvi, yearMax_avgNdvi,
                     yearMean_avgIQ, yearMin_avgIQ, yearMax_avgIQ)
  names(yearlySummary) <- c("yearMean_avgNdvi", "yearMin_avgNdvi", "yearMax_avgNdvi",
                           "yearMean_avgIQ", "yearMin_avgIQ", "yearMax_avgIQ")
  
  yearlySummary_df <- as.data.frame(yearlySummary, xy=T, cells=T)
  yearlySummary_df$year <- oneYear
  return(yearlySummary_df)
}))
saveRDS(yearlyDF, paste0(hddPath,"/NDVI/NDVI_finalSummaries/NDVI_yearlyDF_2000-2022.rds"))
Sys.time()-t0


