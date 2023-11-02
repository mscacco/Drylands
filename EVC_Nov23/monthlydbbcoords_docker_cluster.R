
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

#############
library('move')
library('move2')
library('lubridate')
library("rgeos") #gCentroid
library(data.table)
library(sf)


rasterLayer <- 200 ## resolution in mts of raster
locationError <- 20 ## location error
# extExpansionInMts <- 50000 ## expansion in all 4 direction of the raster for dbb calculation
minLocationsMonth <- 360 # min locations required per month
functionmonthlyMotionVariance <- "mean" # how to sumarize the monthly variance
UDpercentage <- 0.99 ## UD used to calculate the area, centroid, coordinates extracted
##################

pathFolder <- "4.vultureIndv_mv2_1h_outlspeed/"
flsMV <- list.files(pathFolder, full.names = F)
pthmonthlydBBvar <- "6.vultureIndv_monthlydBBvar/"
pthmonthlydBBL <- "8.vultureIndv_monthlydBBL/"
pthmonthlyUDL <- "9.vultureIndv_monthlyUDL/"
monthlyUDtable <- "10.vultureIndv_monthlyUDtable/"
pthmonthlyDBBcoordinatesSF <- "11.vultureIndv_monthlyDBBcoordinatesSF/"

flsMV5 <- list.files(pthmonthlyUDL, full.names = F)
indvFileRDS <- flsMV5[i]
###################################
## monthly dBB coordinates as spdf ##
###################################

monthlyUDL <- readRDS(paste0(pthmonthlyUDL, indvFileRDS))
monthlydBBL <- readRDS(paste0(pthmonthlydBBL, indvFileRDS))
moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))
moveObj <- to_move(moveObj_mv2)
dBBcoordinatesL <- lapply(1:length(monthlydBBL), function(x){ 
  ud <- monthlyUDL[[x]]
  dbb <- monthlydBBL[[x]]
  dbb[ud>UDpercentage] <- NA
  dbb_spdf <- as(dbb,"SpatialPointsDataFrame")
  dbb_spdf_ll <- spTransform(dbb_spdf,projection(moveObj))
  dbb_spdf_ll$monthYear <- gsub("\\.","-",(as.character(gsub("X","",names(monthlydBBL[x])))))
  dbb_spdf_ll$fileName <- indvFileRDS
  dbb_sf_ll <- st_as_sf(dbb_spdf_ll)
  return(dbb_sf_ll)
})

dBBcoordinates_sf <- do.call("rbind",dBBcoordinatesL)
names(dBBcoordinates_sf)[1] <- "dBBvalue"

saveRDS(dBBcoordinates_sf, file=paste0(pthmonthlyDBBcoordinatesSF,indvFileRDS))

