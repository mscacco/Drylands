
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

#############
library('move')
library('move2')
library('lubridate')
# library("rgeos") #gCentroid
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
pthmonthlydBBvar <- "6.vultureIndv_monthlydBBvar/"
pthmonthlydBBL <- "8.vultureIndv_monthlydBBL/"
pthmonthlyUDL <- "9.vultureIndv_monthlyUDL/"
monthlyUDtable <- "10.vultureIndv_monthlyUDtable/"
pthmonthlyDBBcoordinatesSF <- "11.vultureIndv_monthlyDBBcoordinatesSF/"

#### local!!! just for understanding were "missingFiles" file comes from
# genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
# monthlyUDtable <- paste0(genPath,"10.vultureIndv_monthlyUDtable/")
# pthmonthlyDBBcoordinatesSF <- paste0(genPath,"11.vultureIndv_monthlyDBBcoordinatesSF/")
# 
# udt <- list.files(monthlyUDtable)
# coordst <- list.files(pthmonthlyDBBcoordinatesSF)
# miss <- udt[!udt%in%coordst]
# saveRDS(miss, paste0(genPath,"missingFiles.rds"))
######

flsMV5 <- list.files(pthmonthlyUDL, full.names = F)
# flsMV5 <- readRDS("vulturescripts/missingFiles.rds")
indvFileRDS <- flsMV5[i]

###################################
## monthly dBB coordinates as spdf ##
###################################

monthlyUDL <- readRDS(paste0(pthmonthlyUDL, indvFileRDS))
monthlydBBL <- readRDS(paste0(pthmonthlydBBL, indvFileRDS))
moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))

dBBcoordinatesL <- lapply(1:length(monthlydBBL), function(x){ 
  ud <- monthlyUDL[[x]]
  dbb <- monthlydBBL[[x]]
  dbb[ud>UDpercentage] <- NA
  dbb_spdf <- as(dbb,"SpatialPointsDataFrame")
  dbb_spdf$monthYear <- gsub("\\.","-",(as.character(gsub("X","",names(monthlydBBL[x])))))
  dbb_spdf$fileName <- indvFileRDS
  dbb_spdf <- st_as_sf(dbb_spdf)
  dbb_sf_ll <- sf::st_transform(dbb_spdf,crs=st_crs(moveObj_mv2))
  return(dbb_sf_ll)
})

dBBcoordinates_sf <- dplyr::bind_rows(dBBcoordinatesL) 
names(dBBcoordinates_sf)[1] <- "dBBvalue"

saveRDS(dBBcoordinates_sf, file=paste0(pthmonthlyDBBcoordinatesSF,indvFileRDS))

