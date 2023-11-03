
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
pthmonthlydBBvar <- "6.vultureIndv_monthlydBBvar/"
pthmonthlyMotionVar <- "7.vultureIndv_monthlyMotionVar/"

flsMV2 <- list.files(pthmonthlydBBvar, full.names = F)
indvFileRDS <- flsMV2[i]
monthlydBBvar <- readRDS(paste0(pthmonthlydBBvar, indvFileRDS))

monthYear <- paste0(month(timestamps(monthlydBBvar)),"-",year(timestamps(monthlydBBvar)))
motionVar <- data.frame(monthYear=monthYear, motVar = getMotionVariance(monthlydBBvar))
aggMotionVar <- aggregate(motionVar$motVar, by=list(motionVar$monthYear), FUN=functionmonthlyMotionVariance, na.rm=T)

locPerMonthDF <- data.frame(table(monthYear))

aggMotionVarNbLoc <- merge(aggMotionVar, locPerMonthDF, by.x="Group.1",by.y="monthYear", all.x=T)

monthlyMotionVar <- data.frame(fileName=indvFileRDS,
                               monthYear=aggMotionVarNbLoc$Group.1,
                               locsPermonth=aggMotionVarNbLoc$Freq,
                               motionVariance=aggMotionVarNbLoc$x,
                               aggregationMotionVariance=functionmonthlyMotionVariance, ## included this column for sanity check, just to make sure we know what we are doing without having to go back to the code
                               row.names = NULL)

saveRDS(monthlyMotionVar, file=paste0(pthmonthlyMotionVar,indvFileRDS))
