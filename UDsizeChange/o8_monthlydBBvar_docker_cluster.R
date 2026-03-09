
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
# setwd("/home/ascharf/Documents/Projects/Drylands/UDsizeChange/")
# i <- 100
pathFolder <- "./drylands/5.vultureIndv_mv2_1h_outlspeed_dist/"
pthmonthlydBBvar <- "./drylands/11.vultureIndv_monthlydBBvar/" ## manually create on cluster
# migratory_month <- readRDS("./drylands/migratory_month.rds")

flsMV <- list.files(pathFolder, full.names = F)
flsMV_seq_split<- round(seq(1, length(flsMV), length.out=301))
# i <- 1

seqInd <- flsMV_seq_split[i]:flsMV_seq_split[i+1]
indvFileRDS_L <- flsMV[seqInd]
# indvFileRDS <- "481458__Andres__20769287.rds"

month_dBBvar <- function(indvFileRDS){
  moveObj <- readRDS(paste0(pathFolder, indvFileRDS))
  
  if((nrow(moveObj)/length(unique(mt_track_id(moveObj))))<360){stop(paste("individual contains less than 360 locations"))} 
  
  monthYear <- paste0(year(mt_time(moveObj)),"-",sprintf("%02d",month(mt_time(moveObj))))
  locPerMonthDF <- data.frame(table(monthYear))
  
  monthToInclude <- as.character(locPerMonthDF$monthYear[locPerMonthDF$Freq>=minLocationsMonth])
  # migrMnth <- as.character(migratory_month$monthYear[migratory_month$fileName==indvFileRDS & migratory_month$migratoryMonth==T])
  # monthToInclude <- monthToInclude[!monthToInclude%in%migrMnth]
  
  # monthToInclude <- monthToInclude[!monthToInclude%in%monthToExclude]
  if(length(monthToInclude)==0){stop(paste("individual contains 0 month with min nb of locations and no migration"))}
  
  aeqd_crs <- mt_aeqd_crs(moveObj, "center", "m")
  moveObj_c <- sf::st_transform(moveObj, aeqd_crs)
  moveObj_mv1 <- to_move(moveObj_c)
  
  monthlyBurst <- burst(moveObj_mv1,f=as.character(monthYear[-length(monthYear)]))
  monthlydBBvar <- brownian.motion.variance.dyn(monthlyBurst, location.error=locationError,margin=11, window.size=31)
  
  saveRDS(monthlydBBvar, file=paste0(pthmonthlydBBvar,indvFileRDS))
}

llply(indvFileRDS_L, function(x){try(month_dBBvar(indvFileRDS=x))},.parallel =T)

