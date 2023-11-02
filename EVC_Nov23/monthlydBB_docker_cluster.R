
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

flsMV3 <- list.files(pthmonthlydBBvar, full.names = F)

indvFileRDS <- flsMV3[i]
moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))
moveObj <- to_move(moveObj_mv2)
monthlydBBvar <- readRDS(paste0(pthmonthlydBBvar,indvFileRDS))
monthYear <- paste0(month(timestamps(monthlydBBvar)),"-",year(timestamps(monthlydBBvar)))
locPerMonthDF <- data.frame(table(monthYear))
monthlydBBvar@interest[timeLag(moveObj,"hours")>6] <- FALSE ## excluding segments longer than 5 hours from the dbbmm
monthToInclude <- locPerMonthDF$monthYear[locPerMonthDF$Freq>=minLocationsMonth]
monthlydBBvar@interest[!paste0(month(timestamps(monthlydBBvar)),"-",year(timestamps(monthlydBBvar))) %in% monthToInclude] <- FALSE # calc UD only on days with enough points
monthlydBBvarL <- split(monthlydBBvar)
## removing days with all @interest==FALSE
monthlydBBvarL_sel <- lapply(monthlydBBvarL, function(x){if(any(x@interest)){x}else{x <- NULL}})
monthlydBBvarL_sel <- monthlydBBvarL_sel[which(!sapply(monthlydBBvarL_sel, is.null))] # removing "NULL" elements from list

#################
## monthly dBBMM ##
#################
mbbox <- bbox(monthlydBBvar)
extExpansionInMts <- round((max(mbbox[,2]-mbbox[,1]))/2)
monthlydBBL0 <- lapply(monthlydBBvarL_sel, function(dBBvar){try({
  
  db_r <- raster(ext=extent(dBBvar)+c(-extExpansionInMts,extExpansionInMts,-extExpansionInMts,extExpansionInMts), resolution=rasterLayer, crs=projection(dBBvar),vals=1) #creating an empty raster, as this is the only option not giving constantly error because of the extent being to small
  brownian.bridge.dyn(dBBvar, raster=db_r,location.error=rep(locationError,length(dBBvar)),margin=11, window.size=31, time.step=45/15, verbose=F)
})
})
is.error <- function(x) inherits(x, "try-error")
monthlydBBL <- monthlydBBL0[!vapply(monthlydBBL0, is.error, logical(1))]

saveRDS(monthlydBBL, file=paste0(pthmonthlydBBL,indvFileRDS))


