
ndviPath <- "/media/mscacco/6EB4C1E7B4C1B23F/NDVI/NDVI_finalSummaries/"
udPath <- "/home/mscacco/ownCloud/Martina/ProgettiVari/Drylands/DryLands/UDs_keeper/"

library(terra)
library(data.table)
library(stringr)
library(sf)
library(lubridate)
library(plyr)
library(doParallel)
detectCores()

# Load 100km template raster needed for later
templ_rast <- rast("/media/mscacco/6EB4C1E7B4C1B23F/TemplateRaster_100km2.tif")

# Load tables with UD centroids for all individuals and rbind them
refTB <- readRDS(paste0(udPath, "referenceTableStudies_ALL_excludedColumn.rds"))
allUDs <- rbindlist(lapply(list.files(paste0(udPath,"10.vultureIndv_monthlyUDtable"), full.names=T), readRDS))
allUDs <- merge(allUDs, refTB[,c("fileName","individual_local_identifier","species")], 
                by="fileName", all.x=T)

# Check in which range of years we have data available for matching with ndvi
years <- as.numeric(str_sub(allUDs$monthYear, start=-4))
range(years)
# Add to the UD dataset a column with the monthly order (will be used to know which ndvi images are temporally closer)
ndvi_seq_df <- data.frame(date=seq(as.Date("01-01-2006", format="%d-%m-%Y"), 
                               as.Date("01-12-2023", format="%d-%m-%Y"), by="1 month"))
ndvi_seq_df$ndvi_order <- 1:nrow(ndvi_seq_df)
ndvi_seq_df$monthYear <- paste0(as.integer(format(ndvi_seq_df$date, "%m")),"-",year(ndvi_seq_df$date))
table(unique(allUDs$monthYear) %in% unique(ndvi_seq_df$monthYear))
allUDs <- merge(allUDs, ndvi_seq_df[,c("ndvi_order","monthYear")], by="monthYear", all.x=T)

# Select only ndvi files for those years
ndviFls <- list.files(ndviPath, pattern="monthlyDF", full.names = T)
ndviFls_sub <- grep(paste(unique(years), collapse="|"), ndviFls, value=T)

## IMPORTANT!!
## I could not stitch some of the ndvi files on the disk. 
## And the last 3 years of NDVI are also missing (were not downloaded for previous project)
## This causes the number of monthly UDs to drop from 17'000 to 6'000
table(years) # about 11'000 UDs are only for the last three years, for which we don't have NDVI, we need to download it

# Split UD dataset by month-year combination for associating the ndvi
allUDs_monthLS <- split(allUDs, allUDs$monthYear)

UD_ndvi_allYears <- rbindlist(lapply(ndviFls_sub, function(yrF){ #for each of the ndvi years
  
  monthlyDF <- readRDS(yrF)
  monthlyDF$monthYear <- paste0(monthlyDF$month,"-",monthlyDF$year)
  
  monthLs <- split(monthlyDF, monthlyDF$monthYear)
  ndviMonthlyLs <- monthLs[names(monthLs) %in% names(allUDs_monthLS)]
  
  UD_ndvi_oneYear <- rbindlist(lapply(1:length(ndviMonthlyLs), function(i){ #for each month in that year
    # extract the UDs for the corresponding month-year, coerce to sf using centroids as coords and transform to same crs as ndvi
    ud <- allUDs_monthLS[[names(ndviMonthlyLs)[i]]]
    udSF <- st_as_sf(ud, coords = c("UDwMeanLongitude","UDwMeanLatitude"), crs=4326)
    udSF <- st_transform(udSF, crs=crs(templ_rast))
    
    # extract ndvi DF for the corresponding month-year and coerce to raster
    ndviDF <- ndviMonthlyLs[[i]]
    ndviRast <- rast(ndviDF[,c("x","y","monthMean_avgNdvi","monthMax_avgNdvi","monthMean_avgIQ")], 
                     type="xyz", crs=crs(templ_rast))
    
    # check that they overlap
    # plot(ndviRast[[1]], xlim=c(-7306489,-7230868), ylim=c(3902834,3953031))
    # plot(udSF$geometry, add=T)
    
    # extract ndvi values at the locations of the uds' centroids and return as dataframe
    udSF_ndvi <- extract(ndviRast, udSF, xy = T, method = "simple", bind=T)
    names(udSF_ndvi)[13:14] <- c("UDwMeanLongitude_moll","UDwMeanLatitude_moll")
    return(as.data.frame(udSF_ndvi))
  }))
  
  return(UD_ndvi_oneYear)
}))

UD_ndvi_allYears$monthYear <- as.character(UD_ndvi_allYears$monthYear)
table(UD_ndvi_allYears$monthYear)

# Now split df by SPECIES, and calculate UD size percentual difference from each species' average UD size
table(UD_ndvi_allYears$species)
sp_ls <- split(UD_ndvi_allYears, as.character(UD_ndvi_allYears$species))

UD_ndvi_allYears_spDiff <- as.data.frame(rbindlist(lapply(sp_ls, function(sp){
  sp$UD_speciesAvg <- mean(sp$UDsizeKm2, na.rm=T)
  sp$nUds_perSpAvg <- nrow(sp)
  sp$nInds_perSpAvg <- length(unique(sp$individual_local_identifier))
  sp$diff_fromAvgUd <- sp$UDsizeKm2 - mean(sp$UDsizeKm2, na.rm=T)
  sp$diff_fromAvgUd_norm <- (sp$diff_fromAvgUd - min(sp$diff_fromAvgUd, na.rm=T))/(max(sp$diff_fromAvgUd, na.rm=T)-min(sp$diff_fromAvgUd, na.rm=T))
  sp$diff_fromAvgUd_scale <- as.numeric(scale(sp$diff_fromAvgUd)) #(x - mean(x)) / sd(x)
  sp$diff_fromAvgUd_perc <- (sp$diff_fromAvgUd/sp$UD_speciesAvg)*100
  return(sp)
})))

saveRDS(UD_ndvi_allYears_spDiff, file = paste0(udPath,"UDcentroidsWeight-monthlyNDVI_allIndividuals.rds"))





# yearDF <- readRDS(list.files(ndviPath, pattern="yearlyDF", full.names = T))
# yearLs <- split(yearDF, yearDF$year)
# 
# lapply(yearLs, function(year){
#   
# })




