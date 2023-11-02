
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

#############
library('move')
library('move2')
library('lubridate')
library("rgeos") #gCentroid
library(data.table)


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

flsMV5 <- list.files(pthmonthlyUDL, full.names = F)
indvFileRDS <- flsMV5[i]
monthlyUDL <- readRDS(paste0(pthmonthlyUDL,indvFileRDS))

moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))
moveObj <- to_move(moveObj_mv2)
monthYear <- paste0(month(timestamps(moveObj)),"-",year(timestamps(moveObj)))
locPerMonthDF <- data.frame(table(monthYear))


UDsizeL <- lapply(monthlyUDL, function(ud){
  UDsel <- ud<=UDpercentage
  # UDsizeKm2 <- cellStats(UDsel, 'sum') # this only works if rasterLayer==1km
  UDsizem2 <- cellStats(UDsel, 'sum')*rasterLayer*rasterLayer
  UDsizeKm2 <- UDsizem2/1000000
  return(UDsizeKm2)
})

dateschr <- names(monthlyUDL)
dateschr <- gsub("X","",dateschr)
datesDF <- data.frame(monthYear=gsub("\\.","-",dateschr))
includedDatesNbLoc <- merge(datesDF, locPerMonthDF, by="monthYear", all.x=T)

#######################
## monthly UD centroid ##
#######################
# ud <- monthlyUDL[[100]]
centroidsL <- lapply(monthlyUDL, function(ud){
  UDsel <- ud<=UDpercentage
  ud_spdf <- as(UDsel,"SpatialPointsDataFrame")
  centr <- gCentroid(ud_spdf)
  centr_ll <- spTransform(centr,projection(moveObj))
  centr_coor <- coordinates(centr_ll)
  return(centr_coor)
})
centroids <- do.call("rbind",centroidsL)

########################################
## monthly dBB weighted mean coordinates ##
########################################
monthlydBBL <- readRDS(paste0(pthmonthlydBBL,indvFileRDS))
dBBwCoordinatesL <- lapply(1:length(monthlydBBL), function(x){ 
  print(x)
  ud <- monthlyUDL[[x]]
  dbb <- monthlydBBL[[x]]
  dbb[ud>UDpercentage] <- NA
  dbb_spdf <- as(dbb,"SpatialPointsDataFrame")
  dbb_spdf_ll <- spTransform(dbb_spdf,projection(moveObj))
  dbb_df <- as.data.frame(dbb_spdf_ll)
  wMeanLong <-  weighted.mean(dbb_df[,2],dbb_df$layer) 
  wMeanLat <-  weighted.mean(dbb_df[,3],dbb_df$layer) 
  wMeanLL <- data.frame(date=names(monthlydBBL[x]), wMeanLong=wMeanLong, wMeanLat=wMeanLat)
  return(wMeanLL)
})
dBBwCoordinates <- do.call("rbind", dBBwCoordinatesL)
# dBBwCoordinates$date <- as_datetime(as.character(gsub("X","",dBBwCoordinates$date)), tz="UTC", format="%Y.%m.%d")


###############################################
## table of all results of UD calculations ##
###############################################

monthlyUDcalc <- data.frame(fileName=rep(indvFileRDS, length(includedDatesNbLoc$monthYear)),
                            monthYear=includedDatesNbLoc$monthYear,
                            locsPerMonth=includedDatesNbLoc$Freq,
                            UDsizeKm2=unlist(UDsizeL),
                            UDcentroidsLongitude=centroids[,1],
                            UDcentroidsLatitude=centroids[,2],
                            UDwMeanLongitude=dBBwCoordinates$wMeanLong,
                            UDwMeanLatitude=dBBwCoordinates$wMeanLat,
                            row.names = NULL)

saveRDS(monthlyUDcalc, file=paste0(monthlyUDtable,indvFileRDS))
