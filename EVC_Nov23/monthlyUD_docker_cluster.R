
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
flsMV <- list.files(pathFolder, full.names = F)
pthmonthlydBBvar <- "6.vultureIndv_monthlydBBvar/"
pthmonthlydBBL <- "8.vultureIndv_monthlydBBL/"
pthmonthlyUDL <- "9.vultureIndv_monthlyUDL/"

flsMV4 <- list.files(pthmonthlydBBL, full.names = F)
indvFileRDS <- flsMV4[i]
  monthlydBBL <- readRDS(paste0(pthmonthlydBBL,indvFileRDS))
  
  monthlyUDL <-  lapply(monthlydBBL, function(dBB){  
    getVolumeUD(dBB)
  })
  saveRDS(monthlyUDL, file=paste0(pthmonthlyUDL,indvFileRDS))


