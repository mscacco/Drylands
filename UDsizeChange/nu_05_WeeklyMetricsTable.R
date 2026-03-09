## script calculates different weekly displacement measures (from these migration is identified), 

###########################
### weekly displacements ###
###########################
#_____start function_______#
library('move2')
library('lubridate')
library("units")
library(geosphere)
library(sf)
library(raster)
library(adehabitatHR)
# library(flowcatchR) #devtools::install_github("federicomarini/flowcatchR")

## weekly Displacements tables ###
## function calculates:
# - cumulativeDist: sum of all step lenghts (in Km) per Week.
# - maxNetDispl: maximum distance (in Km) between any 2 locations per Week.
# - distFirstLast: distance (in Km) between the 1st and last location of each Week.
# - straightnessIndex: maxNetDispl/cumulativeDist. between 0-1, 1 is moving in straight line
## saves table per individual called "weeklyDisplacement__MBid_indiv.name.rds"
## assumptions:
# - each track is saved a s single rds file and is a move2 object
# - tracks have the same regular sampling. In my case I downsampled to 1h (mt_filter_per_interval(srk, criterion = "first",unit = "hour")), so distances are comparable (specially the cumulative one)

# pathFolder <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/5.vultureIndv_mv2_1h_outlspeed_dist/"
# indvFileRDS <- "327161259_Bubu.rds"
# indvFileRDS <- flsMV[2822] #2822
weeklyDispl <- function(indvFileRDS){ 
  moveObj <- readRDS(paste0(pathFolder, indvFileRDS))
  
  moveObj$weekYear <- paste0(year(mt_time(moveObj)),"-",sprintf("%02d",week(mt_time(moveObj))))
  locPerWeekDF <- data.frame(table(moveObj$weekYear))
  
  moveObjSplitTime <- split(moveObj, moveObj$weekYear)
  meanSpeedWeekL <- lapply(moveObjSplitTime, function(x){mean(mt_speed(x, units="m/s"),na.rm=T)})
  meanSpeedWeek <- do.call("rbind",meanSpeedWeekL)
  medianSpeedWeekL <- lapply(moveObjSplitTime, function(x){median(mt_speed(x, units="m/s"),na.rm=T)})
  medianSpeedWeek <- do.call("rbind",medianSpeedWeekL)
  cumDistWeekL <- lapply(moveObjSplitTime, function(x){sum(mt_distance(x, units="km"),na.rm=T)})
  cumDistWeek <- do.call("rbind",cumDistWeekL)
  maxNetDispL <- lapply(moveObjSplitTime, function(x){max(sf::st_distance(x))})
  maxNetDisp <- do.call("rbind",maxNetDispL)
  distFirstLastL <- lapply(moveObjSplitTime, function(x){sf::st_distance(x[1,],x[nrow(x),])})
  distFirstLast <- do.call("rbind",distFirstLastL)
  # centroidWeekL <- lapply(moveObjSplitTime, function(x){centroid(st_coordinates(x))})
  centroidWeekL <- lapply(moveObjSplitTime, function(x){
    tryCatch({
      cen <- centroid(st_coordinates(x))
      if(any(is.nan(cen))){st_coordinates(x)[1,]}else{cen} ## if all coords are the same result is NaN
    }
      , error = function(e) st_coordinates(x)[1,])
  })
  centroidWeek <- data.frame(do.call("rbind",centroidWeekL))
  colnames(centroidWeek) <- c("lon","lat")
  centroidWeek_sf <- st_as_sf(data.frame(centroidWeek),coords=c("lon","lat"), crs = st_crs(4326))
  centroidDist <- st_distance(centroidWeek_sf[-(nrow(centroidWeek_sf)),],centroidWeek_sf[-1,],by_element=T)
  centroidNSD <- (sf::st_distance(centroidWeek_sf,centroidWeek_sf[1,]))^2
  
  lastPos7dL <- lapply(moveObjSplitTime, function(x){st_coordinates(x)[nrow(x),]})
  lastPos7d <- do.call("rbind",lastPos7dL)
  colnames(lastPos7d) <- c("lon","lat")
  lastPos7d_sf <- st_as_sf(data.frame(lastPos7d),coords=c("lon","lat"), crs = st_crs(4326))
  lastPos7d_NSD <- (sf::st_distance(lastPos7d_sf,lastPos7d_sf[1,]))^2
  
  
  areaMCP95L <- lapply(moveObjSplitTime, function(vultr){
    tryCatch({
      vultr_p <- sf::st_transform(vultr, "ESRI:54009") # using mollweide so all have same projection
      vultr_p$id <- mt_track_id(vultr_p)
      vultr_p_sp <- as_Spatial(vultr_p[,'id'])
      ## function mcp is very particular about the input object, it must only contain 1 column, and names of the individuals names have to follow the validNames() rules
      vultr_p_sp <- vultr_p_sp[,(names(vultr_p_sp) %in% "id")] 
      levels(vultr_p_sp$id) <- validNames(levels(vultr_p_sp$id))
      mcp95 <- mcp(vultr_p_sp, percent=95,unin = "m",unout="km2")
      mcp95$area
    }
    , error = function(e)  0)
  })
  areaMCP95 <- do.call("rbind",areaMCP95L)
  units(areaMCP95) <- "km2"
  
  weeklyDisplacement <- data.frame(
    fileName=indvFileRDS,
    weekYear=locPerWeekDF$Var1,
    locsPerWeek=locPerWeekDF$Freq,
    centroidLon=centroidWeek[,1],
    centroidLat=centroidWeek[,2],
    centroidDist=c(centroidDist,set_units(NA, "m")),
    centroidNSD=centroidNSD,
    lastPos7d_NSD=lastPos7d_NSD,
    meanSpeedWeek=meanSpeedWeek,
    medianSpeedWeek=medianSpeedWeek,
    cumulativeDist=cumDistWeek[,1],
    maxNetDispl=maxNetDisp[,1]/1000,
    distFirstLast=distFirstLast[,1]/1000,
    areaMCP95=areaMCP95[,1],
    row.names = NULL)
  weeklyDisplacement$straightnessIndex <- weeklyDisplacement$maxNetDispl/weeklyDisplacement$cumulativeDist
  
  units(weeklyDisplacement$maxNetDispl) <- "km"
  units(weeklyDisplacement$distFirstLast) <- "km"
  units(weeklyDisplacement$centroidDist) <- units::make_units(km)
  units(weeklyDisplacement$centroidNSD) <- units::make_units(km^2)
  units(weeklyDisplacement$lastPos7d_NSD) <- units::make_units(km^2)
  
  # plot(weeklyDisplacement$centroidNSD~weeklyDisplacement$weekYear)
  # plot(abs(diff(weeklyDisplacement$centroidNSD))~weeklyDisplacement$weekYear[-1])
  # plot(abs(diff(weeklyDisplacement$centroidNSD))~weeklyDisplacement$areaMCP95[-1])
  # plot(weeklyDisplacement$areaMCP95~weeklyDisplacement$maxNetDispl)
  
  saveRDS(weeklyDisplacement, file=paste0(pathToOutputFolder,indvFileRDS)) 
}
#_____end function_______#

library(doParallel)
library(plyr)
mycores <- detectCores()-1 
registerDoParallel(mycores) 

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pathFolder <- paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/")
flsMV <- list.files(pathFolder, full.names = F)
dir.create(paste0(genPath,"6.weeklyDisplacementsTables"))
pathToOutputFolder <- paste0(genPath,"6.weeklyDisplacementsTables/")

done <- list.files(pathToOutputFolder, full.names = F)
flsMV <- flsMV[!flsMV%in%done]

# flsMV <- c("327161259_Deborah-0HA.rds","327161259_Bubu.rds","481458_Andres.rds","327161259_Cid.rds")

start_time <- Sys.time()
results <- llply(flsMV, function(f)try({weeklyDispl(f)}) #pathFolder,pathToOutputFolder - need to exist, are hardcoded
                 ,.parallel = T)

end_time <- Sys.time()
end_time - start_time # ~ 2.2h

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1))) # Check potential errors:
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]


##########
## some exploratory plots 
#####
library(ggplot2)
library(units)
genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
flsDipl <- list.files(paste0(genPath,"6.weeklyDisplacementsTables/"), full.names = T)

displL <- lapply(flsDipl, function(x){
  readRDS(x)
})

displDF <- dplyr::bind_rows(displL) 
# head(displDF)

refIndvDF <- readRDS(file=paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
refIndvDF <- refIndvDF[refIndvDF$excluded%in%"no",]
# head(refIndvDF)
# 
# 
bigDF <- merge(refIndvDF,displDF, by="fileName")
# head(bigDF)
# summary(bigDF)
# hist(bigDF$locsPerWeek)
7*12 # days*locs/day
#84

# subDFkeep <- bigDF[bigDF$locsPerWeek>=84,]
# nrow(bigDF)-nrow(subDFkeep)
# length(unique(subDFkeep$fileName))
# 2492
bigDF$species <- droplevels(bigDF$species)
# sort(table(bigDF$species[bigDF$locsPerWeek>=84]))
# summary(bigDF[bigDF$species=="Cathartes burrovianus",])
# Cathartes burrovianus        Gyps bengalensis         Gyps rueppellii       Sarcoramphus papa 
# 0                      21                      24                     142 
# Vultur gryphus Trigonoceps occipitalis       Gyps himalayensis     Torgos tracheliotus 
# 1372                    1502                    1764                    2060 
# Necrosyrtes monachus        Coragyps atratus       Gypaetus barbatus        Gyps coprotheres 
# 2957                    4179                    9832                   10442 
# Cathartes aura          Gyps africanus       Aegypius monachus   Neophron percnopterus 
# 13798                   19194                   34039                   42437 
# Gyps fulvus 
# 65674 


## with migration like movement:
# YES: "Aegypius monachus","Cathartes aura","Gyps fulvus","Neophron percnopterus" 
## maybe: "Gypaetus barbatus","Gyps africanus","Gyps himalayensis"
# NO: "Cathartes burrovianus","Coragyps atratus","Gyps bengalensis","Gyps coprotheres","Gyps rueppellii", "Necrosyrtes monachus", "Sarcoramphus papa","Torgos tracheliotus","Trigonoceps occipitalis","Vultur gryphus"       

####################################
#      "centroidLon"                 "centroidLat"                
# [23] "centroidDist"                "centroidNSD"                
# [25] "meanSpeedWeek"               "medianSpeedWeek"            
# [27] "cumulativeDist"              "maxNetDispl"                
# [29] "distFirstLast"               "areaMCP95"                  
# [31] "straightnessIndex"           "absDifNSD" km2

wper <- aggregate(bigDF$areaMCP95, by=list(bigDF$species), FUN=function(x) quantile(x, probs = 0.95))
wmx <- aggregate(bigDF$areaMCP95, by=list(bigDF$species), FUN="max",na.rm=T)

##########
## (abs diff) NSD last loc of 7 days, 15 days => to classify migratory from non migratory
## monthly mcp for all
## table(bigDF$animal_life_stage[bigDF$species=="Gyps fulvus"])
####  check divide less/more than 3yrs - adult/juvenile using above
##########

library(ggplot2)
library(units)

bigDF$absDifCentrNSD <- c(NA,abs(diff(bigDF$centroidNSD)))
bigDF$absDif7daysNSD <- c(NA,abs(diff(bigDF$lastPos7d_NSD)))

subtb <- bigDF[bigDF$locsPerWeek>=84 & bigDF$species%in%c("Aegypius monachus","Cathartes aura","Gyps fulvus","Neophron percnopterus"),]

ggplot(subtb )+ geom_point(aes(weekYear,lastPos7d_NSD))+facet_wrap(~species,scales="free")
ggplot(subtb )+ geom_point(aes(weekYear,absDif7daysNSD))+facet_wrap(~species,scales="free")
ggplot(subtb )+ geom_histogram(aes(absDif7daysNSD),bins=100)+facet_wrap(~species,scales="free")


ggplot(subtb )+ geom_point(aes(weekYear,absDifNSD))+facet_wrap(~species,scales="free")

ggplot(subtb[subtb$absDifNSD<1000^2,]) + geom_histogram(aes(absDifNSD),bins=100)+facet_wrap(~species,scales="free")
ggplot(subtb[subtb$absDif7daysNSD>1000^2,]) + geom_histogram(aes(absDif7daysNSD),bins=100)+facet_wrap(~species,scales="free")

ggplot(subtb[!is.na(subtb$centroidDist) & drop_units(subtb$centroidDist)<500,]) + geom_histogram(aes(centroidDist),bins=100)+facet_wrap(~species,scales="free")

ggplot(subtb[drop_units(subtb$areaMCP95)<5000,]) + geom_histogram(aes(areaMCP95),bins=100)+facet_wrap(~species,scales="free")

ggplot(subtb[drop_units(subtb$maxNetDispl)<500,]) + geom_histogram(aes(maxNetDispl),bins=100)+facet_wrap(~species,scales="free")


# "327161259_Deborah-0HA.rds" # es/fr
# "327161259_Bubu.rds" # es
# "327161259_Cid.rds" #eu/afri
# "327161259_Fuelle.rds" #eu/afri

indL <-lapply(indvSps, function(ind){
  readRDS(paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/",ind))
})

subtbin <- bigDF[bigDF$locsPerWeek>=84 & bigDF$fileName%in%c("327161259_Bubu.rds","327161259_Deborah-0HA.rds","327161259_Cid.rds","327161259_Fuelle.rds"),]

ggplot(subtbin )+ geom_point(aes(weekYear,centroidNSD))+facet_wrap(~fileName,scales="fixed")

ggplot(subtbin) + geom_histogram(aes(absDifNSD),bins=100, show.legend = F)+facet_wrap(~fileName, ncol=1)
ggplot(subtbin[!is.na(subtbin$centroidDist) ,]) + geom_histogram(aes(centroidDist),bins=100)+facet_wrap(~fileName, ncol=1)
ggplot(subtbin) + geom_histogram(aes(areaMCP95),bins=100)+facet_wrap(~fileName, ncol=1)
ggplot(subtbin) + geom_histogram(aes(maxNetDispl),bins=100)+facet_wrap(~fileName, ncol=1)
ggplot(subtbin) + geom_histogram(aes(medianSpeedWeek),bins=100)+facet_wrap(~fileName, ncol=1)

##################################


library(lubridate)
library(move2)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(units)
library(EMbC)
genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
spsel <- "Neophron percnopterus"
bigDF_sub <- bigDF[bigDF$locsPerWeek>=84 & bigDF$species==spsel,]
indvSps <- unique(bigDF_sub$fileName)
indL <-lapply(indvSps, function(ind)try({
  indmv <- readRDS(paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/",ind))
  indmv <- mt_as_event_attribute(indmv,individual_local_identifier,.keep=T)
  mt_track_id(indmv) <- "deployment_id"
  return(indmv)
}))

mydat <- data.frame(absNSD=abs(diff(bigDF_sub$centroidNSD)),#maxd=bigDF_sub$maxNetDispl[-nrow(bigDF_sub)], 
                    mcparea=bigDF_sub$areaMCP95[-nrow(bigDF_sub)])
mymat <- as.matrix(mydat)
mymat <- mymat[complete.cases(mymat),]
bc <- embc(mymat)
# lkhp(bc)
# lkhp(bc,15)
lblp(bc)

# obtain 4 categories, resulting from the combinations of high/low 
# par(mfrow=c(1,1))
sctr(bc)
stts(bc)
# sctr(bc, showVars=c(1, 2, 3))

Cat <- bc@A
# Cat[Cat %in% c(2,3)] <- "stationary"
# Cat[Cat %in% c(4)] <- "migrating"
# Cat[Cat %in% c(1,4)] <- "stationary"
# Cat[Cat %in% c(3,5:8)] <- "migrating"

bigDF_sub$embcCat <- c(Cat,NA)

## error comes from indiv loc identif not beeing in data, have to fix that somewhere before
cl <- lapply(indL,class)
cl2 <- unlist(lapply(1:length(cl), function(x){cl[[x]][1]}))
indices_to_remove <- which(cl2 == "try-error")
if(length(indices_to_remove)>0){
  indL <- indL[-indices_to_remove]
}

indL1d <- lapply(indL, function(ind){
  mt_filter_per_interval(ind,criterion = "first",unit="day")
})
indvMV <- mt_stack(indL1d)
indvMV <- select(indvMV,c("sensor_type_id","individual_local_identifier", "deployment_id" ))
indvMV$weekYear <-  paste0(year(mt_time(indvMV)),"-",sprintf("%02d",week(mt_time(indvMV))))
indvMV <- left_join(indvMV, bigDF_sub, by = c("individual_local_identifier","weekYear"))
indvMV$ind_week <- paste0(indvMV$individual_local_identifier, "_", indvMV$weekYear)
mt_track_id(indvMV) <- "ind_week"
length(unique(mt_track_id(indvMV)))
indvMV <-  dplyr::arrange(indvMV, mt_track_id(indvMV), mt_time(indvMV))

table(is.na(indvMV$embcCat))

indvMV_fil <- dplyr::filter(indvMV, !is.na(embcCat))


ggplot() + theme_void() +
  # geom_sf(data = worldMap) +
  geom_sf(data = indvMV_fil,aes(color = embcCat), show.legend = F) +
  # geom_sf(data = mt_track_lines(indvMV_fil), aes(color = ind_week), show.legend = F)+
  facet_wrap(~embcCat)

#######
indvMV_fil_L <- split(indvMV_fil,indvMV_fil$embcCat)

ic1 <- indvMV_fil_L[[1]]
ic1 <- dplyr::arrange(ic1, mt_track_id(ic1), mt_time(ic1))

ic2 <- indvMV_fil_L[[2]]
ic2 <- dplyr::arrange(ic2, mt_track_id(ic2), mt_time(ic2))

ic3 <- indvMV_fil_L[[3]]
ic3 <- dplyr::arrange(ic3, mt_track_id(ic3), mt_time(ic3))

worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
bb <- st_bbox(indvMV_fil)
exp <- 2

c1 <- ggplot() + theme_void() +
  geom_sf(data = worldMap, fill="white", col="grey80") +
  # geom_sf(data = indvMV_fil,aes(color = embcCat), show.legend = F) +
  geom_sf(data = mt_track_lines(ic1), aes(color = ind_week), show.legend = F)+
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)

c2 <- ggplot() + theme_void() +
  # geom_sf(data = worldMap, fill="white", col="grey80") +
  # geom_sf(data = indvMV_fil,aes(color = embcCat), show.legend = F) +
  geom_sf(data = mt_track_lines(ic2), aes(color = ind_week), show.legend = F)#+
  # coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)
# 
c3 <- ggplot() + theme_void() +
  geom_sf(data = worldMap, fill="white", col="grey80") +
  # geom_sf(data = indvMV_fil,aes(color = embcCat), show.legend = F) +
  geom_sf(data = mt_track_lines(ic3), aes(color = ind_week), show.legend = F)+
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)

c1+c2+c3
############


indvMV_fil$embcCat2 <- NA
indvMV_fil <- indvMV_fil %>%
  mutate(embcCat2 = case_when(
    embcCat %in% c(2, 3) ~ "stationary",
    TRUE ~ embcCat2  # Keep existing values for other categories
  ))
indvMV_fil <- indvMV_fil %>%
  mutate(embcCat2 = case_when(
    embcCat %in% c(4) ~ "migrating",
    TRUE ~ embcCat2  # Keep existing values for other categories
  ))
# indvMV_fil$embcCat2[indvMV_fil$embcCat %in% c(2,3)] <- "stationary"
# indvMV_fil$embcCat2[indvMV_fil$embcCat %in% c(4)] <- "migrating"

indvMV_fil_L <- split(indvMV_fil,indvMV_fil$embcCat2)

ic1 <- indvMV_fil_L[[1]]
ic1 <- dplyr::arrange(ic1, mt_track_id(ic1), mt_time(ic1))

ic2 <- indvMV_fil_L[[2]]
ic2 <- dplyr::arrange(ic2, mt_track_id(ic2), mt_time(ic2))


# ic3 <- indvMV_fil_L[[3]]
# ic3 <- dplyr::arrange(ic3, mt_track_id(ic3), mt_time(ic3))

worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
bb <- st_bbox(indvMV_fil)
exp <- 2

c1 <- ggplot() + theme_void() +
  geom_sf(data = worldMap, fill="white", col="grey80") +
  # geom_sf(data = indvMV_fil,aes(color = embcCat), show.legend = F) +
  geom_sf(data = mt_track_lines(ic1), aes(color = ind_week), show.legend = F)+
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)

c2 <- ggplot() + theme_void() +
  geom_sf(data = worldMap, fill="white", col="grey80") +
  # geom_sf(data = indvMV_fil,aes(color = embcCat), show.legend = F) +
  geom_sf(data = mt_track_lines(ic2), aes(color = ind_week), show.legend = F)+
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)
# 
# c3 <- ggplot() + theme_void() +
#   geom_sf(data = worldMap, fill="white", col="grey65") +
#   # geom_sf(data = indvMV_fil,aes(color = embcCat), show.legend = F) +
#   geom_sf(data = mt_track_lines(ic3), aes(color = ind_week), show.legend = F)+
#   coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)

library(patchwork)
c1+c2 + plot_annotation(title = spsel) & theme(plot.title = element_text(hjust = 0.5))
spselPT <- gsub(" ", "_",spsel)
ggsave(paste0(genPath, spselPT,"_split_behav",".jpg"))

######















hist(bigDF$straightnessIndex[bigDF$straightnessIndex<=1])#, breaks="FD")
plot(bigDF$maxNetDispl~bigDF$straightnessIndex)

ggplot(bigDF[bigDF$locsPerWeek>=360,])+geom_histogram(aes(straightnessIndex),bins=100)
ggplot(bigDF[bigDF$locsPerWeek>=360,])+geom_histogram(aes(cumulativeDist),bins=100)
ggplot(bigDF[bigDF$locsPerWeek>=360,])+geom_histogram(aes(maxNetDispl),bins=100)
ggplot(bigDF[bigDF$locsPerWeek>=360,])+geom_histogram(aes(distFirstLast),bins=100)
ggplot(bigDF[bigDF$straightnessIndex>=0.5 & bigDF$locsPerWeek>=360,])+geom_histogram(aes(cumulativeDist),bins=100)
ggplot(bigDF[bigDF$straightnessIndex>=0.5 & bigDF$locsPerWeek>=360,])+geom_histogram(aes(maxNetDispl),bins=100)
ggplot(bigDF[bigDF$straightnessIndex>=0.5 & bigDF$locsPerWeek>=360,])+geom_histogram(aes(distFirstLast),bins=100)

quantile(bigDF$maxNetDispl, seq(0.75,1,0.05) )
quantile(bigDF$distFirstLast, seq(0.75,1,0.05) )

quantile(bigDF$maxNetDispl[bigDF$locsPerWeek>=360], seq(0.75,1,0.05) )
quantile(bigDF$distFirstLast[bigDF$locsPerWeek>=360], seq(0.75,1,0.05) )

# > quantile(bigDF$maxNetDispl, seq(0.75,1,0.05) )
# Units: [km]
# 75%       80%       85%       90%       95%      100% 
#   181.1277  221.6397  282.0318  422.5346  731.0266 8356.8447 
# > quantile(bigDF$distFirstLast, seq(0.75,1,0.05) )
# Units: [km]
# 75%        80%        85%        90%        95%       100% 
#   64.30593   89.73251  137.61005  235.29256  554.44356 7975.83161 
# > quantile(bigDF$maxNetDispl[bigDF$locsPerWeek>=360], seq(0.75,1,0.05) )
# Units: [km]
# 75%       80%       85%       90%       95%      100% 
#   209.4008  258.5166  339.0199  488.9854  833.0662 4486.8337 
# > quantile(bigDF$distFirstLast[bigDF$locsPerWeek>=360], seq(0.75,1,0.05) )
# Units: [km]
# 75%        80%        85%        90%        95%       100% 
#   74.55913  106.57998  163.92286  286.06959  658.87739 3774.68721 
# > 


hist(bigDF$straightnessIndex[bigDF$straightnessIndex<2&bigDF$species=="Neophron percnopterus"])

quantile(bigDF$maxNetDispl[bigDF$locsPerWeek>=360&bigDF$species=="Neophron percnopterus"], seq(0.5,1,0.01) )
quantile(bigDF$distFirstLast[bigDF$locsPerWeek>=360&bigDF$species=="Neophron percnopterus"], seq(0.5,1,0.01) )


subDFdrop <- bigDF[bigDF$maxNetDispl>=set_units(800,km) & bigDF$locsPerWeek>=360,]
sort(table(subDFdrop$species))

subDFkeep <- bigDF[bigDF$maxNetDispl<=set_units(800,km) & bigDF$locsPerWeek>=360,]
sort(table(subDFkeep$species))

sort(table(bigDF$species))

##################################
##
library(lubridate)
library(move2)
library(dplyr)
# sort(table(bigDF$fileName[bigDF$species=="Neophron percnopterus" & bigDF$locsPerWeek>=360]))# 1500
# indp <- "3238087490_MONTEJO - 181641 - 95N.rds"#"149548138_EV_BAR_2019_IMM_R_5T-_Buoux.rds" #"572056515_Divshon_S I15 Red.rds"
sort(table(bigDF$fileName[bigDF$species=="Gyps fulvus" & bigDF$locsPerWeek>=360]))
indp <- "1252551761_A01w.rds"  # "1252551761_J39w.rds"

indp <- "481458_Andres.rds" #"327161259_Cid.rds" #"327161259_Bubu.rds" #"327161259_Deborah-0HA.rds"

ind <- readRDS(paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/",indp))
#plot(ind,max.plot=1)

mothdispl <- readRDS(paste0(genPath,"6.weeklyDisplacementsTables/", indp))

ind$weekYear <- paste0(month(mt_time(ind)),"-",year(mt_time(ind)))

ind <- left_join(ind, mothdispl, by = "weekYear")

indMth <- ind
mt_track_id(indMth) <- "weekYear"

indMth <- mt_as_track_attribute(indMth,locsPerWeek)

indMth360 <- filter_track_data(indMth,locsPerWeek>=360)

library(ggplot2)
worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
bb <- st_bbox(indMth360)
exp <- 2

ggplot() + theme_void() +
  geom_sf(data = worldMap) +
  # geom_sf(data = indMth360) +
  geom_sf(data = mt_track_lines(indMth360), aes(color = weekYear), show.legend = F) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)


hist(indMth360$maxNetDispl)
hist(indMth360$distFirstLast)
hist(indMth360$straightnessIndex)

plot(indMth360$maxNetDispl~indMth360$distFirstLast)


ggplot(bigDF[bigDF$locsPerWeek>=84&bigDF$species=="Cathartes aura",])+
  geom_histogram(aes(maxNetDispl),bins=100)+ 
  geom_vline(xintercept = units::set_units(100,"km"), color = "red", linetype = "dashed")+
  geom_vline(xintercept = units::set_units(500,"km"), color = "blue", linetype = "dashed")+
  geom_vline(xintercept = units::set_units(1000,"km"), color = "magenta", linetype = "dashed")#+
  facet_wrap(~species,scales = "free")

ggplot(bigDF[bigDF$locsPerWeek>=84&bigDF$species=="Cathartes aura",])+
  geom_point(aes(distFirstLast,maxNetDispl))+ 
  geom_vline(xintercept = units::set_units(100,"km"), color = "red", linetype = "dashed")+
  geom_vline(xintercept = units::set_units(500,"km"), color = "blue", linetype = "dashed")+
  geom_vline(xintercept = units::set_units(1000,"km"), color = "magenta", linetype = "dashed")+
  facet_wrap(~species,scales = "free")


ggplot(bigDF[bigDF$locsPerWeek>=84& bigDF$maxNetDispl>units::set_units(50,"km")&bigDF$species=="Cathartes aura",])+
  geom_point(aes(distFirstLast,maxNetDispl))+ 
  geom_vline(xintercept = units::set_units(100,"km"), color = "red", linetype = "dashed")+
  geom_vline(xintercept = units::set_units(500,"km"), color = "blue", linetype = "dashed")+
  geom_vline(xintercept = units::set_units(1000,"km"), color = "magenta", linetype = "dashed")+
  facet_wrap(~fileName,scales = "free")

# 1] Gypaetus barbatus       Torgos tracheliotus     Neophron percnopterus   Gyps himalayensis       Gyps africanus       
# [6] Trigonoceps occipitalis Necrosyrtes monachus    Gyps rueppellii         Aegypius monachus       Vultur gryphus      
# [11] Gyps fulvus             Cathartes aura          Coragyps atratus        Cathartes burrovianus   Sarcoramphus papa  
# [16] Gyps coprotheres        Gyps bengalensis 

sort(table(bigDF$species[bigDF$locsPerWeek>=360]))


# ###################################
# #### identifying migration days ###
# ####################################
# # flsReftInd <- list.files("/home/anne/Documents/GRACEdata/ReferenceTables/", full.names = T)
# # refIndvL <- lapply(flsReftInd, function(x){
# #   readRDS(x)
# # })
# # refIndvDF <- dplyr::bind_rows(refIndvL)
# # head(refIndvDF)
# 
# flsDipl <- list.files(paste0(genPath,"5.weeklyDisplacementsTables/"), full.names = T)
# displL <- lapply(flsDipl, function(x){
#   readRDS(x)
# })
# displDF <- dplyr::bind_rows(displL)
# head(displDF)
# 
# # refAndDispl <- data.frame(merge(refIndvDF,displDF, by=c("commonID", "individual","date", "locsPerDay")))
# #
# # rm(refIndvDF)
# # rm(displDF)
# 
# refAndDispl <- displDF
# ##### trying to find a pattern across all flying sps ###
# 
# # refAndDisplSUB <- refAndDispl[ refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 & refAndDispl$Locomotion=="flying" ,]
# refAndDisplSUB <- refAndDispl[ refAndDispl$locsPerWeek>360 ,]
# 
# SIcut <- seq(0,1,0.05)
# 
# medianLall <- lapply(split(refAndDisplSUB, refAndDisplSUB$fileName), function(mb){
#   lapply(SIcut, function(x){
#     data.frame(med_mNDkm=median(mb$maxNetDispl[mb$straightnessIndex>x],na.rm = T),SIcut=x)
#   })
# })
# dfSImNDall <- dplyr::bind_rows(medianLall)
# 
# ggplot(dfSImNDall)+geom_boxplot(aes(y=med_mNDkm, x=SIcut, group=SIcut)) #+ ylim(0,100)
# 
# #### looking at specific genus
# # Ciconia,Eidolon, Gruidae, Gyps, 
# # Aquila, Buteo, Milvus
# 
# # genusSel <- "Milvus"
# # genusX <- refAndDispl[refAndDispl$genus%in%genusSel & refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 ,] #refAndDispl$tracking_duration_days>730 & refAndDispl$straightnessIndex>0.8 &
# 
# ## or just selecting randomly from all flying
# genusSel <- "random"
# genusX <- refAndDispl[refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 & refAndDispl$Locomotion=="flying" ,]
# 
# genusXnames <- genusX[!duplicated(paste0(genusX$individual,genusX$MBid)),]
# head(genusXnames)
# 
# indivNamesMBid <- paste0(genusXnames$MBid,"_",genusXnames$individual)
# 
# indivNamesMBidSelect <- sample(indivNamesMBid, 20, replace=F)
# 
# minKM <- 50
# minSI <- 0.7
# 
# pdf(paste0("/home/anne/Documents/GRACEdata/plots/", genusSel,"_",minSI,"_",minKM, ".pdf"), width=11.69, height=8.27) #dinA4
# lapply(indivNamesMBidSelect, function(indivSel){
#   library("move")
#   pthTOindiv <- paste0("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", indivSel,".rds")
#   mv <- readRDS(pthTOindiv)     
#   
#   library(lubridate)
#   indiv <- mv@idData$individual.local.identifier
#   if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
#   tsmv <- data.frame(ts=timestamps(mv), date= floor_date(timestamps(mv), "day"), individual=indiv)
#   
#   refAndDisplSub <- refAndDispl[refAndDispl$individual== indiv & refAndDispl$MBid==mv@idData$study.id, ]
#   
#   df <- merge(tsmv, refAndDisplSub, by=c("individual","date"), all.x=T)
#   # head(df)
#   mv$straightnessIndex <- df$straightnessIndex
#   mv$maxNetDispl_km <- df$maxNetDispl_km
#   mv$locsPerDay <- df$locsPerDay
#   
#   dfmv <- as.data.frame(mv)
#   # head(dfmv)
#   
#   
#   library("rnaturalearth")
#   library("rnaturalearthdata")
#   library("ggplot2")
#   library("patchwork")
#   
#   ## plot for sanity check
#   world <- ne_countries(scale = "medium", returnclass = "sf")
#   p1 <- ggplot(data = world) + geom_sf(color="grey50", fill="grey95") +  
#     ggtitle("scale of movement")+
#     geom_path(data=dfmv,aes(coords.x1, coords.x2), color="red")+
#     coord_sf(xlim = range(dfmv$coords.x1)+c(-1,1), ylim = range(dfmv$coords.x2)+c(-1,1))+ # Limit the map
#     ggspatial::annotation_scale(location = 'br')
#   
#   ## plot maxNetDispl_km ~ straightnessIndex
#   SIcut <- seq(0,1,0.05)
#   medianL <- lapply(SIcut, function(x){
#     median(df$maxNetDispl_km[df$straightnessIndex>x & df$locsPerDay>8],na.rm = T)
#   })
#   
#   medianmND <- unlist(medianL)
#   dfSImND <- data.frame(SIcut=SIcut, med_mNDkm=medianmND)
#   
#   p2 <- ggplot(dfSImND)+geom_point(aes(SIcut,med_mNDkm)) + geom_hline(yintercept = minKM, col="red")+geom_vline(xintercept = minSI, col="firebrick") +ggtitle("maxNetDispl_km ~ straightnessIndex")
#   
#   
#   # summary(dfmv$straightnessIndex)
#   p3 <- ggplot(dfmv)+geom_path(aes(coords.x1, coords.x2, color=straightnessIndex))+geom_point(aes(coords.x1, coords.x2, color=straightnessIndex), size=.1)+scale_colour_viridis_c(option = "viridis",direction=1,na.value="white")+coord_fixed() + ggtitle("all straightnessIndex")#+ ggspatial::annotation_scale(location = 'br')
#   
#   dfmv2 <- dfmv
#   dfmv2$straightnessIndex[dfmv2$straightnessIndex>minSI &dfmv2$maxNetDispl_km>minKM] <- NA
#   dfmv2$straightnessIndex[dfmv2$locsPerDay<7 ] <- NA
#   p4 <- ggplot(dfmv2)+geom_path(aes(coords.x1, coords.x2, color=straightnessIndex))+geom_point(aes(coords.x1, coords.x2, color=straightnessIndex), size=.1)+scale_colour_viridis_c(option = "viridis",direction=1,na.value="white")+coord_fixed()  + ggtitle(paste0("remove straightnessIndex>",minSI," & maxNetDispl_km>", minKM))#+     ggspatial::annotation_scale(location = 'br')
#   
#   allnas <- data.frame(table(is.na(dfmv2$straightnessIndex)))
#   
#   p1 + p2 + p3 + p4 + labs(subtitle = paste0("NAs: ",allnas$Var1[1],": ", allnas$Freq[1] , " & ", allnas$Var1[2],": ", allnas$Freq[2] )) + 
#     plot_annotation(title = paste0("sps: ",mv@idData$individual.taxon.canonical.name))
# })
# dev.off()
# 
# ## DECISION: 
# # minKM <- 50 (to be considered migration day, animal has to travel more then 50km in a straight line (maxNetDispl)) and 
# # minSI <- 0.7 (the straightnessIndex has to be over 0.7 to be considered as migration day)
# # seem to make sense and work the best
