
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

pathFolder <- "./drylands/5.vultureIndv_mv2_1h_outlspeed_dist/"
pthmonthlydBBvar <- "./drylands/11.vultureIndv_monthlydBBvar/"
pthmonthlydBBL <- "./drylands/12.vultureIndv_monthlydBBL/" ## manually create on cluster
# migratory_month <- readRDS("./drylands/migratory_month.rds")


# flsMV <- list.files(pthmonthlydBBvar, full.names = F)
# # i <- 1
# # indvFileRDS <- flsMV[i]
# 
# flsMV_seq_split<- round(seq(1, length(flsMV), length.out=301))
# seqInd <- flsMV_seq_split[i]:flsMV_seq_split[i+1]
# indvFileRDS_L <- flsMV[seqInd]
# indvFileRDS <- "481458__Andres__20769287.rds"
indvFileRDS_L <- readRDS("./drylands/missing_dBBL_2.rds")

month_dBB <- function(indvFileRDS){
  moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))
  moveObj_mv1 <- to_move(moveObj_mv2)
  monthlydBBvar <- readRDS(paste0(pthmonthlydBBvar,indvFileRDS))
  monthlydBBvar$monthYear <- paste0(year(mt_time(moveObj_mv2)),"-",sprintf("%02d",month(mt_time(moveObj_mv2))))
  locPerMonthDF <- data.frame(table(monthlydBBvar$monthYear))
  monthlydBBvar@interest[timeLag(moveObj_mv1,"hours")>6] <- FALSE ## excluding segments longer than 5 hours from the dbbmm
  monthToInclude <- as.character(locPerMonthDF$Var1[locPerMonthDF$Freq>=minLocationsMonth])
  # migrMnth <- as.character(migratory_month$monthYear[migratory_month$fileName==indvFileRDS & migratory_month$migratoryMonth==T])
  # monthToInclude <- monthToInclude[!monthToInclude%in%migrMnth]
  
  monthlydBBvar@interest[!monthlydBBvar$monthYear %in% monthToInclude] <- FALSE # calc UD only on days with enough points and not migrating
  monthlydBBvarL <- split(monthlydBBvar)
  ## removing days with all @interest==FALSE
  monthlydBBvarL_sel <- lapply(monthlydBBvarL, function(x){if(any(x@interest)){x}else{x <- NULL}})
  monthlydBBvarL_sel <- monthlydBBvarL_sel[which(!sapply(monthlydBBvarL_sel, is.null))] # removing "NULL" elements from list
  
  #################
  ## monthly dBBMM ##
  #################
  mbbox <- bbox(monthlydBBvar)
  extExpansionInMts <- round((max(mbbox[,2]-mbbox[,1])))#/2)
  monthlydBBL0 <- lapply(monthlydBBvarL_sel, function(dBBvar){try({
    
    db_r <- raster(ext=extent(dBBvar)+c(-extExpansionInMts,extExpansionInMts,-extExpansionInMts,extExpansionInMts), resolution=rasterLayer, crs=projection(dBBvar),vals=1) #creating an empty raster, as this is the only option not giving constantly error because of the extent being to small
    brownian.bridge.dyn(dBBvar, raster=db_r,location.error=rep(locationError,length(dBBvar)),margin=11, window.size=31, time.step=45/15, verbose=F)
  })
  })
  is.error <- function(x) inherits(x, "try-error")
  monthlydBBL <- monthlydBBL0[!vapply(monthlydBBL0, is.error, logical(1))]
  
  saveRDS(monthlydBBL, file=paste0(pthmonthlydBBL,indvFileRDS))
}

# llply(indvFileRDS_L, function(x){try(month_dBB(indvFileRDS=x))},.parallel =T)
lapply(indvFileRDS_L[i], function(x){try(month_dBB(indvFileRDS=x))})


