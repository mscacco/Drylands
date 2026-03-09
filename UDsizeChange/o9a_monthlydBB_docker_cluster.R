
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


rasterLayer <- 50*1000 ## resolution in mts of raster
locationError <- 20 ## location error
# extExpansionInMts <- 50000 ## expansion in all 4 direction of the raster for dbb calculation
minLocationsMonth <- 360 # min locations required per month
functionmonthlyMotionVariance <- "mean" # how to sumarize the monthly variance
UDpercentage <- 0.99 ## UD used to calculate the area, centroid, coordinates extracted
##################
# setwd("/home/ascharf/Documents/Projects/Drylands/UDsizeChange/")

pathFolder <- "./drylands/5.vultureIndv_mv2_1h_outlspeed_dist/"
# pthmonthlydBBvar <- "./drylands/11.vultureIndv_monthlydBBvar/"
# pthmonthlydBBL <- "./drylands/12.vultureIndv_monthlydBBL/" ## manually create on cluster
pthmonthlydBBL <- "./drylands/11.vultureIndv_monthlydBBL_10Km/" ## manually create on cluster
# migratory_month <- readRDS("./drylands/migratory_month.rds")


flsMV <- list.files(pthmonthlydBBvar, full.names = F)
# i <- 1
# indvFileRDS <- flsMV[i]

flsMV_seq_split<- round(seq(1, length(flsMV), length.out=301))
seqInd <- flsMV_seq_split[i]:flsMV_seq_split[i+1]
indvFileRDS_L <- flsMV[seqInd]
# indvFileRDS <- "481458__Andres__20769287.rds"

month_dBB <- function(indvFileRDS){
  moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))
  moveObj_mv2$monthYear <- paste0(year(mt_time(moveObj_mv2)),"-",sprintf("%02d",month(mt_time(moveObj_mv2))))
  moveObj_mv1 <- to_move(moveObj_mv2)
  # monthlydBBvar <- readRDS(paste0(pthmonthlydBBvar,indvFileRDS))
  moveObj_mv1$monthYear <- paste0(year(mt_time(moveObj_mv2)),"-",sprintf("%02d",month(mt_time(moveObj_mv2))))
  
  moveObj_mv1_prj <- spTransform(moveObj_mv1,"+proj=aeqd +lat_0=-18.470889 +lon_0=16.793974 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  moveObj_mv1_month_L <- split(moveObj_mv1_prj,moveObj_mv1_prj$monthYear)
  
  moveObj_mv1_month_L_sel <- lapply(moveObj_mv1_month_L, function(x){if(n.locs(x)>=minLocationsMonth){x}else{x <- NULL}})
  moveObj_mv1_month_L_sel <- moveObj_mv1_month_L_sel[which(!sapply(moveObj_mv1_month_L_sel, is.null))] # removing "NULL" elements from list
 
  
  mbbox <- bbox(moveObj_mv1_prj)
  extExpansionInMts <- round((max(mbbox[,2]-mbbox[,1]))/2)
  
  db_r <- raster(ext=extent(moveObj_mv1_prj)+c(-extExpansionInMts,extExpansionInMts,-extExpansionInMts,extExpansionInMts), resolution=rasterLayer, crs=projection(moveObj_mv1_prj),vals=1) #creating an empty raster, as this is the only option not giving constantly error because of the extent being to small
  bb_all <- brownian.bridge.dyn(moveObj_mv1_prj, raster=db_r,location.error=rep(locationError,length(moveObj_mv1_prj)),margin=11, window.size=31, time.step=45/15, verbose=F)
  #################
  ## monthly dBBMM ##
  #################
  dBB_L <- lapply(1:length(moveObj_mv1_month_L_sel), function(x){
    print(x)
    mv <- moveObj_mv1_month_L_sel[[x]]
    brownian.bridge.dyn(mv, raster=db_r,location.error=rep(locationError,length(mv)),margin=11, window.size=31, time.step=45/15, verbose=F)
  })
  saveRDS(dBB_L, file=paste0(pthmonthlydBBL,indvFileRDS))
}

llply(indvFileRDS_L, function(x){try(month_dBB(indvFileRDS=x))},.parallel =T)


