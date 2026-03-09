
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

#############
library('move')
library('move2')
# library("rgeos") #gCentroid
library('lubridate')
library(data.table)
library(stars)
library(sf)
library(doParallel)
library(plyr)
registerDoParallel(4)

rasterLayer <- 1000 ## resolution in mts of raster
locationError <- 20 ## location error
# extExpansionInMts <- 50000 ## expansion in all 4 direction of the raster for dbb calculation
minLocationsMonth <- 360 # min locations required per month
functionmonthlyMotionVariance <- "mean" # how to sumarize the monthly variance
UDpercentage <- 0.99 ## UD used to calculate the area, centroid, coordinates extracted
##################

pathFolder <- "./drylands/5.vultureIndv_mv2_1h_outlspeed_dist/"
pthmonthlydBBvar <- "./drylands/11.vultureIndv_monthlydBBvar/"
pthmonthlydBBL <- "./drylands/12.vultureIndv_monthlydBBL/"
pthmonthlyUDL <- "./drylands/13.vultureIndv_monthlyUDL/"  
monthlyUDtable <- "./drylands/14.vultureIndv_monthlyUDtable/" ## manually create on cluster
# migratory_month <- readRDS("./drylands/migratory_month.rds")

# pathFolder <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/5.vultureIndv_mv2_1h_outlspeed_dist/"
# pthmonthlydBBvar <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/11.vultureIndv_monthlydBBvar/"
# pthmonthlydBBL <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/12.vultureIndv_monthlydBBL/"
# pthmonthlyUDL <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/13.vultureIndv_monthlyUDL/"  ## manually create on cluster
# migratory_month <- readRDS("/home/ascharf/Documents/Projects/Drylands/UDsizeChange/migratory_month.rds")

# flsMV <- list.files(pthmonthlyUDL, full.names = F)
# flsMV_seq_split<- round(seq(1, length(flsMV), length.out=301))
# seqInd <- flsMV_seq_split[i]:flsMV_seq_split[i+1]
# indvFileRDS_L <- flsMV[seqInd]
# indvFileRDS <- "481458__Andres__20769287.rds"
# indvFileRDS <- flsMV#[3]

indvFileRDS_L <- readRDS("./drylands/missing_tbls.rds")

month_UDtab <- function(indvFileRDS){

monthlyUDL <- readRDS(paste0(pthmonthlyUDL,indvFileRDS))

moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))
moveObj <- to_move(moveObj_mv2)
monthYear <- paste0(year(timestamps(moveObj)),"-",sprintf("%02d",month(timestamps(moveObj))))
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
## monthly UD centroid 
#######################
# ud <- monthlyUDL[[1]]
centroidsL <- lapply(monthlyUDL, function(ud){
  UDsel <- ud<=UDpercentage
  UDsel[UDsel==0] <- NA
  stars_ud <- st_as_stars(UDsel)
  sf_ud <- st_as_sf(stars_ud)
  sf_ud_u <- st_union(sf_ud)
  centr <- st_centroid(sf_ud_u) 
  centr_ll <- st_transform(centr,projection(moveObj))
  centr_coor <- st_coordinates(centr_ll)
  return(centr_coor)
})
centroids <- do.call("rbind",centroidsL)

########################################
## monthly dBB weighted mean coordinates ##
########################################
monthlydBBL <- readRDS(paste0(pthmonthlydBBL,indvFileRDS))
dBBwCoordinatesL <- lapply(1:length(monthlydBBL), function(x){ 
  # print(x)
  ud <- monthlyUDL[[x]]
  dbb <- monthlydBBL[[x]]
  dbb[ud>UDpercentage] <- NA
  stars_dbb <- st_as_stars(dbb)
  sf_dbb <- st_as_sf(stars_dbb)
  sf_dbb_c <- st_centroid(sf_dbb)
  dbb_spdf_ll <- st_transform(sf_dbb_c,projection(moveObj))
  wMeanLong <-  weighted.mean(st_coordinates(dbb_spdf_ll)[,1],dbb_spdf_ll$layer) 
  wMeanLat <-  weighted.mean(st_coordinates(dbb_spdf_ll)[,2],dbb_spdf_ll$layer) 
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
}

# llply(indvFileRDS_L, function(x){try(month_UDtab(indvFileRDS=x))},.parallel =T)
lapply(indvFileRDS_L[i], function(x){try(month_UDtab(indvFileRDS=x))})

