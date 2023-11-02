## script calculates different daily displacement measures (from these migration is identified), 

###########################
### daily displacements ###
###########################
#_____start function_______#
library('move2')
library('lubridate')
library("units")

## daily Displacements tables ###
## function calculates:
# - cumulativeDist: sum of all step lenghts (in Km) per day.
# - maxNetDispl: maximum distance (in Km) between any 2 locations per day.
# - distFirstLast: distance (in Km) between the 1st and last location of each day.
# - straightnessIndex: maxNetDispl/cumulativeDist. between 0-1, 1 is moving in straight line
## saves table per individual called "dailyDisplacement__MBid_indiv.name.rds"
## assumptions:
# - each track is saved a s single rds file and is a move2 object
# - tracks have the same regular sampling. In my case I downsampled to 1h (mt_filter_per_interval(srk, criterion = "first",unit = "hour")), so distances are comparable (specially the cumulative one)

moveObj <- readRDS("/home/ascharf/ownCloud/DryLands/EVC23_scripts/15869951_Alex.rds")

monthlyDispl <- function(pathFolder, indvFileRDS){ 
  moveObj <- readRDS(paste0(pathFolder, indvFileRDS))
  
  monthPresent <- paste0(month(mt_time(moveObj)),"-",year(mt_time(moveObj)))
  
  
  roundTS <- floor_date(mt_time(moveObj), "day") 
  locPerDayDF <- data.frame(table(roundTS))
  locPerDayDF$roundTS <- as_datetime(as.character(locPerDayDF$roundTS), tz="UTC", format="%Y-%m-%d")
  
  moveObjSplitTime <- split(moveObj, roundTS)
  cumDistDayL <- lapply(moveObjSplitTime, function(x){sum(mt_distance(x),na.rm=T)})
  cumDistDay <- do.call("rbind",cumDistDayL)
  maxNetDispL <- lapply(moveObjSplitTime, function(x){max(sf::st_distance(x))})
  maxNetDisp <- do.call("rbind",maxNetDispL)
  distFirstLastL <- lapply(moveObjSplitTime, function(x){sf::st_distance(x[1,],x[nrow(x),])})
  distFirstLast <- do.call("rbind",distFirstLastL)
  
  dailyDisplacement <- data.frame(
    individual=indiv,
    date=locPerDayDF$roundTS,
    locsPerDay=locPerDayDF$Freq,
    cumulativeDist=cumDistDay[,1]/1000,
    maxNetDispl=maxNetDisp[,1]/1000,
    distFirstLast=distFirstLast[,1]/1000,
    row.names = NULL)
  dailyDisplacement$straightnessIndex <- dailyDisplacement$maxNetDispl/dailyDisplacement$cumulativeDist
  
  units(dailyDisplacement$cumulativeDist) <- "km"
  units(dailyDisplacement$maxNetDispl) <- "km"
  units(dailyDisplacement$distFirstLast) <- "km"
  
  saveRDS(dailyDisplacement, file=unique(paste0(pathToOutputFolder,mt_track_data(moveObj)$study_id,"_",indiv,".rds"))) ##unique as some indiv have multiple deployments
}
#_____end function_______#

library(doParallel)
library(plyr)
mycores <- detectCores()-1 
registerDoParallel(mycores) 

genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
flsMV <- list.files(paste0(genPath,"4.vultureIndv_mv2_1h_outlspeed"), full.names = T)
dir.create(paste0(genPath,"5.dailyDisplacementsTables"))
pathToOutputFolder <- paste0(genPath,"5.dailyDisplacementsTables/")

start_time <- Sys.time()
results <- llply(flsMV, function(f)try({dailyDispl(f)}) #pathFolder, indvFileRDS
                 ,.parallel = T)

end_time <- Sys.time()
end_time - start_time # ~

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1))) # Check potential errors:
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
