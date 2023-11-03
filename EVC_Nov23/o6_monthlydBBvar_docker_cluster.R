
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

#############
library('move')
library('move2')
library('lubridate')
# library("rgeos") #gCentroid
library(data.table)


rasterLayer <- 200 ## resolution in mts of raster
locationError <- 20 ## location error
# extExpansionInMts <- 50000 ## expansion in all 4 direction of the raster for dbb calculation
minLocationsMonth <- 360 # min locations required per month
functionmonthlyMotionVariance <- "mean" # how to sumarize the monthly variance
UDpercentage <- 0.99 ## UD used to calculate the area, centroid, coordinates extracted
##################

pathFolder <- "4.vultureIndv_mv2_1h_outlspeed/"
pthmonthlydBBvar <- "6.vultureIndv_monthlydBBvar/"

flsMV <- list.files(pathFolder, full.names = F)
indvFileRDS <- flsMV[i]
moveObj <- readRDS(paste0(pathFolder, indvFileRDS))

if((nrow(moveObj)/length(unique(mt_track_id(moveObj))))<360){stop(paste("individual contains less than 360 locations"))} 

## reading in the monthlyDisplacement 
# dipslTab <- readRDS(paste0(pathTomonthlyDisplacements,indvFileRDS))
# monthToExclude <- dipslTab$date[dipslTab$straightnessIndex>minSI & dipslTab$maxNetDispl_km>minKM]

monthYear <- paste0(month(mt_time(moveObj)),"-",year(mt_time(moveObj)))
locPerMonthDF <- data.frame(table(monthYear))

monthToInclude <- locPerMonthDF$monthYear[locPerMonthDF$Freq>=minLocationsMonth]
# monthToInclude <- monthToInclude[!monthToInclude%in%monthToExclude]
if(length(monthToInclude)==0){stop(paste("individual contains 0 month with min nb of locations"))}

aeqd_crs <- mt_aeqd_crs(moveObj, "center", "m")
moveObj_c <- sf::st_transform(moveObj, aeqd_crs)
moveObj_mv1 <- to_move(moveObj_c)

monthlyBurst <- burst(moveObj_mv1,f=as.character(monthYear[-length(monthYear)]))
monthlydBBvar <- brownian.motion.variance.dyn(monthlyBurst, location.error=locationError,margin=11, window.size=31)

saveRDS(monthlydBBvar, file=paste0(pthmonthlydBBvar,indvFileRDS))

