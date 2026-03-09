
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

#############
library('move')
library('move2')
library('lubridate')
# library("rgeos") #gCentroid
library(data.table)
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
pthmonthlyUDL <- "./drylands/13.vultureIndv_monthlyUDL/"  ## manually create on cluster
# migratory_month <- readRDS("./drylands/migratory_month.rds")

flsMV <- list.files(pthmonthlydBBL, full.names = F)
flsMV_seq_split<- round(seq(1, length(flsMV), length.out=301))
seqInd <- flsMV_seq_split[i]:flsMV_seq_split[i+1]
indvFileRDS_L <- flsMV[seqInd]
# indvFileRDS <- "481458__Andres__20769287.rds"
# indvFileRDS <- flsMV[3]
# indvFileRDS <- "481458__Leo__37900997.rds"
month_UD <- function(indvFileRDS){
  monthlydBBL <- readRDS(paste0(pthmonthlydBBL,indvFileRDS))
  
  monthlyUDL <-  lapply(monthlydBBL, function(dBB){  
    getVolumeUD(dBB)
  })
  saveRDS(monthlyUDL, file=paste0(pthmonthlyUDL,indvFileRDS))
}

llply(indvFileRDS_L, function(x){try(month_UD(indvFileRDS=x))},.parallel =T)



