library('move')
library('move2')
library('lubridate')
library("rgeos") #gCentroid
library(data.table)

########
# FOR NEXT RUN, SPLIT UP CODE IN STEPS, AND SAVE EACH STEP. hopefully that will work without constantily breaking the code
########

# bursted move object by days
# calculates dbb variance on bursted move object
# calculates monthly motion variance summarized as indicated by "functionmonthlyMotionVariance", saved as "monthlyMotionVar_.....rds"
# removes days that have less locations than "minLocationsMonth", by setting @interest==F
# calculates dBB per day looping through the splitted dbbvarburst object
# calculates ud size, centroid and weighted lat/long coords, saved as "monthlyUDcalc_...rds" 
# extracts coordinates and values from dbb, in SPDF, saved as "monthlyDBBcoordinatesSPDF_...rds"

# pathToMV <- "/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/927364554_Silas + - DER A7N66 (e-obs 7045).rds" #
# 18957668_Fanti_111.rds
# pathToMV <- flsMV[470]

genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
pathFolder <- paste0(genPath,"4.vultureIndv_mv2_1h_outlspeed/")
flsMV <- list.files(pathFolder, full.names = F)

rasterLayer <- 200 ## resolution in mts of raster
locationError <- 20 ## location error
extExpansionInMts <- 20000 ## expansion in all 4 direction of the raster for dbb calculation
# pathToOutputFolder <- "~/GIT_SYNC/Grace/NoPush/outputTestRuns/"
minLocationsMonth <- 360 # min locations required per month
functionmonthlyMotionVariance <- "mean" # how to sumarize the monthly variance
UDpercentage <- 0.99 ## UD used to calculate the area, centroid, coordinates extracted
pathToReferenceTables <- paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds")
pathTomonthlyDisplacements <- paste0(genPath,"5.monthlyDisplacementsTables/")
# minKM <- 50 # max maxNetDispl in KM
# minSI <- 0.7 # max straightnessIndex
##################

indvFileRDS <- flsMV[100]
# monthlydBBud <-  function(pathToMV,pathToOutputFolder, rasterLayer,locationError, extExpansionInMts,minLocationsMonth,functionmonthlyMotionVariance,UDpercentage,pathToReferenceTables,pathTomonthlyDisplacements,minKM,minSI){

dir.create(paste0(genPath,"6.vultureIndv_monthlydBBvar"))
pthmonthlydBBvar <- paste0(genPath,"6.vultureIndv_monthlydBBvar/")

tictoc::tic()
results <- lapply(flsMV, function(indvFileRDS)try({
  print(indvFileRDS)
  moveObj <- readRDS(paste0(pathFolder, indvFileRDS))
  
  if((nrow(moveObj)/length(unique(mt_track_id(moveObj))))<360){stop(paste("individual contains less than 360 locations"))} 
  
  ## reading in the monthlyDisplacement 
  # dipslTab <- readRDS(paste0(pathTomonthlyDisplacements,indvFileRDS))
  # monthToExclude <- dipslTab$date[dipslTab$straightnessIndex>minSI & dipslTab$maxNetDispl_km>minKM]
  
  monthYear <- paste0(month(mt_time(moveObj)),"-",year(mt_time(moveObj)))
  locPerMonthDF <- data.frame(table(monthYear))
  
  monthToInclude <- locPerMonthDF$monthYear[locPerMonthDF$Freq>=minLocationsMonth]
  # monthToInclude <- monthToInclude[!monthToInclude%in%monthToExclude]
  if(length(monthToInclude)==0){stop(paste("individual contains 0 month with min nb of locations"))}
  
  moveObj_mv1 <- to_move(moveObj)
  
  monthlyBurst <- burst(moveObj_mv1,f=as.character(monthYear[-length(monthYear)]))
  monthlyBurst_c <- spTransform(monthlyBurst,center=T)
  monthlydBBvar <- brownian.motion.variance.dyn(monthlyBurst_c, location.error=locationError,margin=11, window.size=31)
  
  saveRDS(monthlydBBvar, file=paste0(pthmonthlydBBvar,indvFileRDS))
}))
tictoc::toc() #~

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]

###########################
## monthly motion variance ## 
###########################
dir.create(paste0(genPath,"6.vultureIndv_monthlyMotionVar"))
pthmonthlyMotionVar <- paste0(genPath,"6.vultureIndv_monthlyMotionVar/")

flsMV2 <- list.files(pthmonthlydBBvar, full.names = F)
monthlydBBvar <- readRDS("/home/ascharf/ownCloud/DryLands/EVC23_scripts/1027758675_L763.rds")
moveObj_mv2 <- readRDS("/home/ascharf/ownCloud/DryLands/EVC23_scripts/1027758675_L763 (1).rds")
indvFileRDS <- "1027758675_L763.rds"
tictoc::tic()
results <- lapply(flsMV2, function(indvFileRDS)try({
  print(indvFileRDS)
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
}))
tictoc::toc() #~

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]

#####################################################################
## selecting days with minimum nb of locations (=minLocationsMonth)  ##
#####################################################################
dir.create(paste0(genPath,"7.vultureIndv_monthlydBBL"))
pthmonthlydBBL <- paste0(genPath,"7.vultureIndv_monthlydBBL/")

flsMV3 <- list.files(pthmonthlydBBvar, full.names = F)

tictoc::tic()
results <- lapply(flsMV3, function(indvFileRDS)try({
  print(indvFileRDS)
  moveObj_mv2 <- readRDS(paste0(pathFolder, indvFileRDS))
  moveObj <- to_move(moveObj_mv2)
  monthlydBBvar <- readRDS(paste0(pthmonthlydBBvar,indvFileRDS))
  
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
  monthlydBBL0 <- lapply(monthlydBBvarL_sel, function(dBBvar){try({
    # monthlydBBL0 <- lapply(1:length(monthlydBBvarL_sel), function(x){try({ # removes names from putput list! and code does not work downstream!!
    #   print(x)
    #   dBBvar <- monthlydBBvarL_sel[[x]]
    if(max(dBBvar@means,na.rm=T)>1000000){stop("to high variance estimates")}
    db_r <- raster(ext=extent(dBBvar)+c(-extExpansionInMts,extExpansionInMts,-extExpansionInMts,extExpansionInMts), resolution=rasterLayer, crs=projection(dBBvar),vals=1) #creating an empty raster, as this is the only option not giving constantly error because of the extent being to small
    brownian.bridge.dyn(dBBvar, raster=db_r,location.error=rep(locationError,length(dBBvar)),margin=11, window.size=31, time.step=45/15, verbose=F)
  })
  })
  is.error <- function(x) inherits(x, "try-error")
  monthlydBBL <- monthlydBBL0[!vapply(monthlydBBL0, is.error, logical(1))]
  
  saveRDS(monthlydBBL, file=paste0(pthmonthlydBBL,indvFileRDS))
}))
tictoc::toc() #~

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]



##############
## monthly UD ##
##############
dir.create(paste0(genPath,"7.vultureIndv_monthlyUDL"))
pthmonthlyUDL <- paste0(genPath,"7.vultureIndv_monthlyUDL/")

flsMV4 <- list.files(pthmonthlydBBL, full.names = F)

tictoc::tic()
results <- lapply(flsMV4, function(indvFileRDS)try({
  print(indvFileRDS)
  monthlydBBL <- readRDS(paste0(pthmonthlydBBL,indvFileRDS))
  
  monthlyUDL <-  lapply(monthlydBBL, function(dBB){  
    getVolumeUD(dBB)
  })
  saveRDS(monthlyUDL, file=paste0(pthmonthlyUDL,indvFileRDS))
}))
tictoc::toc() #~

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
##############HERE###############
###################
## monthly UD size ##
###################
UDsizeL <- lapply(monthlyUDL, function(ud){
  UDsel <- ud<=UDpercentage
  # UDsizeKm2 <- cellStats(UDsel, 'sum') # this only works if rasterLayer==1km
  UDsizem2 <- cellStats(UDsel, 'sum')*rasterLayer*rasterLayer
  UDsizeKm2 <- UDsizem2/1000000
  return(UDsizeKm2)
})

dateschr <- names(monthlyUDL)
dateschr <- gsub("X","",dateschr)
datesDF <- data.frame(date=as_datetime(as.character(dateschr), tz="UTC", format="%Y.%m.%d"))
includedDatesNbLoc <- merge(datesDF, locPerDayDF, by.x="date",by.y="roundTS", all.x=T)

#######################
## monthly UD centroid ##
#######################
# ud <- monthlyUDL[[100]]
centroidsL <- lapply(monthlyUDL, function(ud){
  UDsel <- ud<=UDpercentage
  ud_spdf <- as(UDsel,"SpatialPointsDataFrame")
  centr <- gCentroid(ud_spdf)
  centr_ll <- spTransform(centr,projection(moveObj))
  centr_coor <- coordinates(centr_ll)
  return(centr_coor)
})
centroids <- do.call("rbind",centroidsL)

########################################
## monthly dBB weighted mean coordinates ##
########################################
dBBwCoordinatesL <- lapply(1:length(monthlydBBL), function(x){ 
  # print(x)
  ud <- monthlyUDL[[x]]
  dbb <- monthlydBBL[[x]]
  dbb[ud>UDpercentage] <- NA
  dbb_spdf <- as(dbb,"SpatialPointsDataFrame")
  dbb_spdf_ll <- spTransform(dbb_spdf,projection(moveObj))
  dbb_df <- as.data.frame(dbb_spdf_ll)
  wMeanLong <-  weighted.mean(dbb_df$x,dbb_df$layer) 
  wMeanLat <-  weighted.mean(dbb_df$y,dbb_df$layer) 
  wMeanLL <- data.frame(date=names(monthlydBBL[x]), wMeanLong=wMeanLong, wMeanLat=wMeanLat)
  return(wMeanLL)
})
dBBwCoordinates <- do.call("rbind", dBBwCoordinatesL)
# dBBwCoordinates$date <- as_datetime(as.character(gsub("X","",dBBwCoordinates$date)), tz="UTC", format="%Y.%m.%d")


###############################################
## table of all results of UD calculations ##
###############################################
# indiv <- moveObj@idData$individual.local.identifier
# if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}

monthlyUDcalc <- data.frame(commonID=paste(moveObj@idData$study.id, indiv, includedDatesNbLoc$date, sep="_"),
                            individual=indiv,
                            date=includedDatesNbLoc$date,
                            locsPerDay=includedDatesNbLoc$Freq,
                            UDsizeKm2=unlist(UDsizeL),
                            UDcentroidsLongitude=centroids[,1],
                            UDcentroidsLatitude=centroids[,2],
                            UDwMeanLongitude=dBBwCoordinates$wMeanLong,
                            UDwMeanLatitude=dBBwCoordinates$wMeanLat,
                            row.names = NULL)

saveRDS(monthlyUDcalc, file=paste0(pathToOutputFolder,"monthlyUDcalc_",moveObj@idData$study.id,"_",indiv,".rds"))

###################################
## monthly dBB coordinates as spdf ##
###################################
dBBcoordinatesL <- lapply(1:length(monthlydBBL), function(x){ 
  ud <- monthlyUDL[[x]]
  dbb <- monthlydBBL[[x]]
  dbb[ud>UDpercentage] <- NA
  dbb_spdf <- as(dbb,"SpatialPointsDataFrame")
  dbb_spdf_ll <- spTransform(dbb_spdf,projection(moveObj))
  dbb_spdf_ll$date <- as_datetime(as.character(gsub("X","",names(monthlydBBL[x]))), tz="UTC", format="%Y.%m.%d")
  dbb_spdf_ll$commonID <- paste(moveObj@idData$study.id, indiv, as_datetime(as.character(gsub("X","",names(monthlydBBL[x]))), tz="UTC", format="%Y.%m.%d"), sep="_")
  return(dbb_spdf_ll)
})

dBBcoordinates_spdf <- do.call("rbind",dBBcoordinatesL)
names(dBBcoordinates_spdf)[1] <- "dBBvalue"

saveRDS(dBBcoordinates_spdf, file=paste0(pathToOutputFolder,"monthlyDBBcoordinatesSPDF_",moveObj@idData$study.id,"_",indiv,".rds"))
# end_time <- Sys.time()
# end_time - start_time
}



## this is how one do the raster to spdf in sf, but it probably does not make sense in this case as 
# library(stars)
# dbbSel_t_stars <- st_as_stars(dbbSel_t)
# library(sf)
# st_as_sf(dbbSel_t_stars, as_points = TRUE, merge = FALSE)






