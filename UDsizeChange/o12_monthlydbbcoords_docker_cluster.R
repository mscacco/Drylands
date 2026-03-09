
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
pthmonthlyDBBcoordinatesSF <- "./drylands/15.vultureIndv_monthlyDBBcoordinatesSF/" ## manually create on cluster

######
flsMV <- list.files(pthmonthlyUDL, full.names = F)
flsMV_seq_split<- round(seq(1, length(flsMV), length.out=301))
seqInd <- flsMV_seq_split[i]:flsMV_seq_split[i+1]
indvFileRDS_L <- flsMV[seqInd]
# indvFileRDS <- "481458__Andres__20769287.rds"
# indvFileRDS <- flsMV#[3]
###################################
## monthly dBB coordinates as spdf ##
###################################

month_DBBcoords <- function(indvFileRDS){
  
  monthlyUDL <- readRDS(paste0(pthmonthlyUDL, indvFileRDS))
  monthlydBBL <- readRDS(paste0(pthmonthlydBBL, indvFileRDS))
  moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))
  
  dBBcoordinatesL <- lapply(1:length(monthlydBBL), function(x){ 
    ud <- monthlyUDL[[x]]
    dbb <- monthlydBBL[[x]]
    dbb[ud>UDpercentage] <- NA
    stars_dbb <- st_as_stars(dbb)
    sf_dbb <- st_as_sf(stars_dbb)
    sf_dbb_c <- st_centroid(sf_dbb)
    dbb_sf_ll <- st_transform(sf_dbb_c,projection(moveObj_mv2))
    dbb_sf_ll$monthYear <- gsub("\\.","-",(as.character(gsub("X","",names(monthlydBBL[x])))))
    dbb_sf_ll$fileName <- indvFileRDS
    return(dbb_sf_ll)
  })
  
  dBBcoordinates_sf <- dplyr::bind_rows(dBBcoordinatesL) 
  names(dBBcoordinates_sf)[1] <- "dBBvalue"
  
  saveRDS(dBBcoordinates_sf, file=paste0(pthmonthlyDBBcoordinatesSF,indvFileRDS))
}
llply(indvFileRDS_L, function(x){try(month_DBBcoords(indvFileRDS=x))},.parallel =T)
