## script calculates different monthly displacement measures (from these migration is identified), 

###########################
### monthly displacements ###
###########################
#_____start function_______#
library('move2')
library('lubridate')
library("units")
library(raster)
library(sf)
library(adehabitatHR)

## monthly Displacements tables ###
## function calculates:
# - cumulativeDist: sum of all step lenghts (in Km) per Month.
# - maxNetDispl: maximum distance (in Km) between any 2 locations per Month.
# - distFirstLast: distance (in Km) between the 1st and last location of each Month.
# - straightnessIndex: maxNetDispl/cumulativeDist. between 0-1, 1 is moving in straight line
## saves table per individual called "monthlyDisplacement__MBid_indiv.name.rds"
## assumptions:
# - each track is saved a s single rds file and is a move2 object
# - tracks have the same regular sampling. In my case I downsampled to 1h (mt_filter_per_interval(srk, criterion = "first",unit = "hour")), so distances are comparable (specially the cumulative one)

# pathFolder <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/4.vultureIndv_mv2_1h_outlspeed/"
# indvFileRDS <- "327161259__Deborah-0HA__1283254784.rds"
monthlyDispl <- function(indvFileRDS){ 
  moveObj <- readRDS(paste0(pathFolder, indvFileRDS))
  
  moveObj$monthYear <- paste0(year(mt_time(moveObj)),"-",sprintf("%02d",month(mt_time(moveObj))))
  locPerMonthDF <- data.frame(table(moveObj$monthYear))
  
  moveObjSplitTime <- split(moveObj, moveObj$monthYear)
  cumDistMonthL <- lapply(moveObjSplitTime, function(x){sum(mt_distance(x, units="km"),na.rm=T)})
  cumDistMonth <- do.call("rbind",cumDistMonthL)
  maxNetDispL <- lapply(moveObjSplitTime, function(x){max(sf::st_distance(x))})
  maxNetDisp <- do.call("rbind",maxNetDispL)
  distFirstLastL <- lapply(moveObjSplitTime, function(x){sf::st_distance(x[1,],x[nrow(x),])})
  distFirstLast <- do.call("rbind",distFirstLastL)
  
  # vultr <- moveObjSplitTime[[1]]
  MCP95L <- lapply(moveObjSplitTime, function(vultr){
    tryCatch({
      vultr_p <- sf::st_transform(vultr, "ESRI:54009") # using mollweide so all have same projection
      vultr_p$id <- mt_track_id(vultr_p)
      vultr_p_sp <- as_Spatial(vultr_p[,'id'])
      ## function mcp is very particular about the input object, it must only contain 1 column, and names of the individuals names have to follow the validNames() rules
      vultr_p_sp <- vultr_p_sp[,(names(vultr_p_sp) %in% "id")] 
      levels(vultr_p_sp$id) <- validNames(levels(vultr_p_sp$id))
      mcp95 <- mcp(vultr_p_sp, percent=95,unin = "m",unout="km2")
      mcp95sf <- st_as_sf(mcp95,crs="ESRI:54009")
      coords <- st_coordinates(mcp95sf)
      width <- min(dist(coords))
      length <- max(dist(coords))
      max_width <- max(width, length)
      data.frame(areamcp= mcp95$area, mcpmaxWidthKM=max_width/1000)
     
    } , error = function(e)  0)
  })
  MCP95 <- do.call("rbind",MCP95L)
  units(MCP95$areamcp) <- "km2"
  units(MCP95$mcpmaxWidthKM) <- "km"
  
  monthlyDisplacement <- data.frame(
    fileName=indvFileRDS,
    monthYear=locPerMonthDF$Var1,
    locsPerMonth=locPerMonthDF$Freq,
    cumulativeDist=cumDistMonth[,1],
    maxNetDispl=maxNetDisp[,1]/1000,
    distFirstLast=distFirstLast[,1]/1000,
    areaMCP95=MCP95$areamcp,
    mcpmaxWidthKM=MCP95$mcpmaxWidthKM,
    row.names = NULL)
  monthlyDisplacement$straightnessIndex <- monthlyDisplacement$maxNetDispl/monthlyDisplacement$cumulativeDist
  
  units(monthlyDisplacement$maxNetDispl) <- "km"
  units(monthlyDisplacement$distFirstLast) <- "km"
  
  saveRDS(monthlyDisplacement, file=paste0(pathToOutputFolder,indvFileRDS)) 
}
#_____end function_______#

library(doParallel)
library(plyr)
mycores <- detectCores()-1 
registerDoParallel(mycores) 

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pathFolder <- paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/")
flsMV <- list.files(pathFolder, full.names = F)
dir.create(paste0(genPath,"6.monthlyDisplacementsTables"))
pathToOutputFolder <- paste0(genPath,"6.monthlyDisplacementsTables/")

done <- list.files(pathToOutputFolder, full.names = F)
flsMV <- flsMV[!flsMV%in%done]

start_time <- Sys.time()
results <- llply(flsMV, function(f)try({monthlyDispl(f)}) #pathFolder,pathToOutputFolder - need to exist, are hardcoded
                 ,.parallel = T)

end_time <- Sys.time()
end_time - start_time # ~ 1.2h

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1))) # Check potential errors:
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]


########## (See down creating of table) ##
## some exploratory plots 
#####
library(ggplot2)
library(units)
genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
flsDipl <- list.files(paste0(genPath,"6.monthlyDisplacementsTables/"), full.names = T)

displL <- lapply(flsDipl, function(x){
  readRDS(x)
})

displDF <- dplyr::bind_rows(displL) 
head(displDF)

refIndvDF <- readRDS(file=paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
refIndvDF <- refIndvDF[refIndvDF$excluded%in%"no",]
head(refIndvDF)
# 
# 
bigDF <- merge(refIndvDF,displDF, by="fileName")
head(bigDF)
summary(bigDF)
subDFkeep <- bigDF[bigDF$locsPerMonth>=360,]
nrow(bigDF)-nrow(subDFkeep)
length(unique(subDFkeep$fileName))
# 1855
hist(bigDF$locsPerMonth)
30*12 # days*locs/day
#360

bigDF$species <- droplevels(bigDF$species)


wper <- aggregate(round(bigDF$areaMCP95/1000), by=list(bigDF$species), FUN=function(x) quantile(x, probs = c(0.85, 0.9,0.95, 1)))
wmx <- aggregate(round(bigDF$areaMCP95/1000), by=list(bigDF$species), FUN="max",na.rm=T)

wLper <- aggregate(round(bigDF$mcpmaxWidthKM), by=list(bigDF$species), FUN=function(x) quantile(x, probs = c(0.85, 0.9,0.95, 1)))
wLper
## keeping 90% of mcpmaxWidthKM for migratory sps seems reasonable
wLmx <- aggregate(round(bigDF$mcpmaxWidthKM), by=list(bigDF$species), FUN="max",na.rm=T)

plot(areaMCP95~mcpmaxWidthKM, bigDF[bigDF$locsPerMonth>=360&bigDF$species=="Gyps fulvus",])

###################################################
##### marking the "migratory" month ###########
################################################
genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
flsDipl <- list.files(paste0(genPath,"6.monthlyDisplacementsTables/"), full.names = T)
spsTb <- read.csv("/home/ascharf/Documents/Projects/Drylands/vultureSps.csv")
migr_sps <- spsTb$sps[spsTb$migratory%in%"y"]

displL <- lapply(flsDipl, function(x){
  readRDS(x)
})
displDF <- dplyr::bind_rows(displL) 
# head(displDF)
refIndvDF <- readRDS(file=paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
refIndvDF <- refIndvDF[refIndvDF$excluded%in%"no",]
# head(refIndvDF)
displDF_sps <- merge(refIndvDF[,c("fileName","species")],displDF, by="fileName")
displDF_sps$species <- as.character(displDF_sps$species)
displDF_sps$migratoryMonth <- F

# sps <- migr_sps[4]
for(sps in migr_sps){
  migr_sps_sub <- displDF_sps[displDF_sps$species==sps,]
  qntl <- quantile(migr_sps_sub$mcpmaxWidthKM, probs = 0.9)
  displDF_sps$migratoryMonth[displDF_sps$species==sps&displDF_sps$mcpmaxWidthKM>units::set_units(qntl,"km")] <- T
  # qntl <- quantile(migr_sps_sub$areaMCP95, probs = 0.9)
  # displDF_sps$migratoryMonth[displDF_sps$species==sps&displDF_sps$areaMCP95>units::set_units(qntl,"km2")] <- T
}
table(displDF_sps$migratoryMonth)
saveRDS(displDF_sps, file=paste0(genPath,"migratory_month.rds"))
# saveRDS(displDF_sps, file=paste0(genPath,"migratory_month_9area.rds")) 



# ###################################################################
# hist(bigDF$straightnessIndex[bigDF$straightnessIndex<2])#, breaks="FD")
# plot(bigDF$maxNetDispl~bigDF$straightnessIndex)
# 
# ggplot(bigDF[bigDF$locsPerMonth>=360,])+geom_histogram(aes(straightnessIndex),bins=100)
# ggplot(bigDF[bigDF$locsPerMonth>=360,])+geom_histogram(aes(cumulativeDist),bins=100)
# ggplot(bigDF[bigDF$locsPerMonth>=360,])+geom_histogram(aes(maxNetDispl),bins=100)
# ggplot(bigDF[bigDF$locsPerMonth>=360,])+geom_histogram(aes(distFirstLast),bins=100)
# ggplot(bigDF[bigDF$straightnessIndex>=0.5 & bigDF$locsPerMonth>=360,])+geom_histogram(aes(cumulativeDist),bins=100)
# ggplot(bigDF[bigDF$straightnessIndex>=0.5 & bigDF$locsPerMonth>=360,])+geom_histogram(aes(maxNetDispl),bins=100)
# ggplot(bigDF[bigDF$straightnessIndex>=0.5 & bigDF$locsPerMonth>=360,])+geom_histogram(aes(distFirstLast),bins=100)
# 
# quantile(bigDF$maxNetDispl, seq(0.75,1,0.05) )
# quantile(bigDF$distFirstLast, seq(0.75,1,0.05) )
# 
# quantile(bigDF$maxNetDispl[bigDF$locsPerMonth>=360], seq(0.75,1,0.05) )
# quantile(bigDF$distFirstLast[bigDF$locsPerMonth>=360], seq(0.75,1,0.05) )
# 
# # > quantile(bigDF$maxNetDispl, seq(0.75,1,0.05) )
# # Units: [km]
# # 75%       80%       85%       90%       95%      100% 
# #   181.1277  221.6397  282.0318  422.5346  731.0266 8356.8447 
# # > quantile(bigDF$distFirstLast, seq(0.75,1,0.05) )
# # Units: [km]
# # 75%        80%        85%        90%        95%       100% 
# #   64.30593   89.73251  137.61005  235.29256  554.44356 7975.83161 
# # > quantile(bigDF$maxNetDispl[bigDF$locsPerMonth>=360], seq(0.75,1,0.05) )
# # Units: [km]
# # 75%       80%       85%       90%       95%      100% 
# #   209.4008  258.5166  339.0199  488.9854  833.0662 4486.8337 
# # > quantile(bigDF$distFirstLast[bigDF$locsPerMonth>=360], seq(0.75,1,0.05) )
# # Units: [km]
# # 75%        80%        85%        90%        95%       100% 
# #   74.55913  106.57998  163.92286  286.06959  658.87739 3774.68721 
# # > 
# 
# 
# hist(bigDF$straightnessIndex[bigDF$straightnessIndex<2&bigDF$species=="Neophron percnopterus"])
# 
# quantile(bigDF$maxNetDispl[bigDF$locsPerMonth>=360&bigDF$species=="Neophron percnopterus"], seq(0.5,1,0.01) )
# quantile(bigDF$distFirstLast[bigDF$locsPerMonth>=360&bigDF$species=="Neophron percnopterus"], seq(0.5,1,0.01) )
# 
# 
# subDFdrop <- bigDF[bigDF$maxNetDispl>=set_units(800,km) & bigDF$locsPerMonth>=360,]
# sort(table(subDFdrop$species))
# 
# subDFkeep <- bigDF[bigDF$maxNetDispl<=set_units(800,km) & bigDF$locsPerMonth>=360,]
# sort(table(subDFkeep$species))
# 
# sort(table(bigDF$species))
# 
# ##################################
# ##
# library(lubridate)
# library(move2)
# library(dplyr)
# # sort(table(bigDF$fileName[bigDF$species=="Neophron percnopterus" & bigDF$locsPerMonth>=360]))# 1500
# # indp <- "3238087490_MONTEJO - 181641 - 95N.rds"#"149548138_EV_BAR_2019_IMM_R_5T-_Buoux.rds" #"572056515_Divshon_S I15 Red.rds"
# sort(table(bigDF$fileName[bigDF$species=="Gyps fulvus" & bigDF$locsPerMonth>=360]))
# indp <- "1252551761_A01w.rds"  # "1252551761_J39w.rds"
# 
# indp <- "481458_Andres.rds" #"327161259_Cid.rds" #"327161259_Bubu.rds" #"327161259_Deborah-0HA.rds"
# 
# ind <- readRDS(paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/",indp))
# #plot(ind,max.plot=1)
# 
# mothdispl <- readRDS(paste0(genPath,"6.monthlyDisplacementsTables/", indp))
# 
# ind$monthYear <- paste0(month(mt_time(ind)),"-",year(mt_time(ind)))
# 
# ind <- left_join(ind, mothdispl, by = "monthYear")
# 
# indMth <- ind
# mt_track_id(indMth) <- "monthYear"
# 
# indMth <- mt_as_track_attribute(indMth,locsPerMonth)
# 
# indMth360 <- filter_track_data(indMth,locsPerMonth>=360)
# 
# library(ggplot2)
# worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
# bb <- st_bbox(indMth360)
# exp <- 2
# 
# ggplot() + theme_void() +
#   geom_sf(data = worldMap) +
#   # geom_sf(data = indMth360) +
#   geom_sf(data = mt_track_lines(indMth360), aes(color = monthYear), show.legend = F) +
#   coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)
# 
# 
# hist(indMth360$maxNetDispl)
# hist(indMth360$distFirstLast)
# hist(indMth360$straightnessIndex)
# 
# plot(indMth360$maxNetDispl~indMth360$distFirstLast)
# 
# 
# ggplot(bigDF[bigDF$locsPerMonth>=360,])+
#   geom_histogram(aes(maxNetDispl),bins=100)+ 
#   geom_vline(xintercept = units::set_units(1000,"km"), color = "red", linetype = "dashed")+
#   geom_vline(xintercept = units::set_units(1500,"km"), color = "blue", linetype = "dashed")+
#   geom_vline(xintercept = units::set_units(2000,"km"), color = "magenta", linetype = "dashed")+
#   facet_wrap(~species,scales = "free")
# 
# ggplot(bigDF[bigDF$locsPerMonth>=360,])+
#   geom_point(aes(distFirstLast,maxNetDispl))+ 
#   geom_vline(xintercept = units::set_units(1000,"km"), color = "red", linetype = "dashed")+
#   geom_vline(xintercept = units::set_units(1500,"km"), color = "blue", linetype = "dashed")+
#   geom_vline(xintercept = units::set_units(2000,"km"), color = "magenta", linetype = "dashed")+
#   facet_wrap(~species,scales = "free")
# 
# 
# ggplot(bigDF[bigDF$locsPerMonth>=360& bigDF$maxNetDispl>units::set_units(800,"km")&bigDF$species=="Cathartes aura",])+
#   geom_point(aes(distFirstLast,maxNetDispl))+ 
#   geom_vline(xintercept = units::set_units(1000,"km"), color = "red", linetype = "dashed")+
#   geom_vline(xintercept = units::set_units(1500,"km"), color = "blue", linetype = "dashed")+
#   geom_vline(xintercept = units::set_units(2000,"km"), color = "magenta", linetype = "dashed")+
#   facet_wrap(~fileName,scales = "free")
# 
# # 1] Gypaetus barbatus       Torgos tracheliotus     Neophron percnopterus   Gyps himalayensis       Gyps africanus       
# # [6] Trigonoceps occipitalis Necrosyrtes monachus    Gyps rueppellii         Aegypius monachus       Vultur gryphus      
# # [11] Gyps fulvus             Cathartes aura          Coragyps atratus        Cathartes burrovianus   Sarcoramphus papa  
# # [16] Gyps coprotheres        Gyps bengalensis 
# 
# sort(table(bigDF$species[bigDF$locsPerMonth>=360]))
# # ####################################
# # #### identifying migration days ###
# # ####################################
# # # flsReftInd <- list.files("/home/anne/Documents/GRACEdata/ReferenceTables/", full.names = T)
# # # refIndvL <- lapply(flsReftInd, function(x){
# # #   readRDS(x)
# # # })
# # # refIndvDF <- dplyr::bind_rows(refIndvL)
# # # head(refIndvDF)
# # 
# # flsDipl <- list.files(paste0(genPath,"5.monthlyDisplacementsTables/"), full.names = T)
# # displL <- lapply(flsDipl, function(x){
# #   readRDS(x)
# # })
# # displDF <- dplyr::bind_rows(displL)
# # head(displDF)
# # 
# # # refAndDispl <- data.frame(merge(refIndvDF,displDF, by=c("commonID", "individual","date", "locsPerDay")))
# # #
# # # rm(refIndvDF)
# # # rm(displDF)
# # 
# # refAndDispl <- displDF
# # ##### trying to find a pattern across all flying sps ###
# # 
# # # refAndDisplSUB <- refAndDispl[ refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 & refAndDispl$Locomotion=="flying" ,]
# # refAndDisplSUB <- refAndDispl[ refAndDispl$locsPerMonth>360 ,]
# # 
# # SIcut <- seq(0,1,0.05)
# # 
# # medianLall <- lapply(split(refAndDisplSUB, refAndDisplSUB$fileName), function(mb){
# #   lapply(SIcut, function(x){
# #     data.frame(med_mNDkm=median(mb$maxNetDispl[mb$straightnessIndex>x],na.rm = T),SIcut=x)
# #   })
# # })
# # dfSImNDall <- dplyr::bind_rows(medianLall)
# # 
# # ggplot(dfSImNDall)+geom_boxplot(aes(y=med_mNDkm, x=SIcut, group=SIcut)) #+ ylim(0,100)
# # 
# # #### looking at specific genus
# # # Ciconia,Eidolon, Gruidae, Gyps, 
# # # Aquila, Buteo, Milvus
# # 
# # # genusSel <- "Milvus"
# # # genusX <- refAndDispl[refAndDispl$genus%in%genusSel & refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 ,] #refAndDispl$tracking_duration_days>730 & refAndDispl$straightnessIndex>0.8 &
# # 
# # ## or just selecting randomly from all flying
# # genusSel <- "random"
# # genusX <- refAndDispl[refAndDispl$GPSpts_total>31 & refAndDispl$locsPerDay>8 & refAndDispl$Locomotion=="flying" ,]
# # 
# # genusXnames <- genusX[!duplicated(paste0(genusX$individual,genusX$MBid)),]
# # head(genusXnames)
# # 
# # indivNamesMBid <- paste0(genusXnames$MBid,"_",genusXnames$individual)
# # 
# # indivNamesMBidSelect <- sample(indivNamesMBid, 20, replace=F)
# # 
# # minKM <- 50
# # minSI <- 0.7
# # 
# # pdf(paste0("/home/anne/Documents/GRACEdata/plots/", genusSel,"_",minSI,"_",minKM, ".pdf"), width=11.69, height=8.27) #dinA4
# # lapply(indivNamesMBidSelect, function(indivSel){
# #   library("move")
# #   pthTOindiv <- paste0("/home/anne/Documents/GRACEdata/MoveObjects_1hour_noOutliers/", indivSel,".rds")
# #   mv <- readRDS(pthTOindiv)     
# #   
# #   library(lubridate)
# #   indiv <- mv@idData$individual.local.identifier
# #   if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
# #   tsmv <- data.frame(ts=timestamps(mv), date= floor_date(timestamps(mv), "day"), individual=indiv)
# #   
# #   refAndDisplSub <- refAndDispl[refAndDispl$individual== indiv & refAndDispl$MBid==mv@idData$study.id, ]
# #   
# #   df <- merge(tsmv, refAndDisplSub, by=c("individual","date"), all.x=T)
# #   # head(df)
# #   mv$straightnessIndex <- df$straightnessIndex
# #   mv$maxNetDispl_km <- df$maxNetDispl_km
# #   mv$locsPerDay <- df$locsPerDay
# #   
# #   dfmv <- as.data.frame(mv)
# #   # head(dfmv)
# #   
# #   
# #   library("rnaturalearth")
# #   library("rnaturalearthdata")
# #   library("ggplot2")
# #   library("patchwork")
# #   
# #   ## plot for sanity check
# #   world <- ne_countries(scale = "medium", returnclass = "sf")
# #   p1 <- ggplot(data = world) + geom_sf(color="grey50", fill="grey95") +  
# #     ggtitle("scale of movement")+
# #     geom_path(data=dfmv,aes(coords.x1, coords.x2), color="red")+
# #     coord_sf(xlim = range(dfmv$coords.x1)+c(-1,1), ylim = range(dfmv$coords.x2)+c(-1,1))+ # Limit the map
# #     ggspatial::annotation_scale(location = 'br')
# #   
# #   ## plot maxNetDispl_km ~ straightnessIndex
# #   SIcut <- seq(0,1,0.05)
# #   medianL <- lapply(SIcut, function(x){
# #     median(df$maxNetDispl_km[df$straightnessIndex>x & df$locsPerDay>8],na.rm = T)
# #   })
# #   
# #   medianmND <- unlist(medianL)
# #   dfSImND <- data.frame(SIcut=SIcut, med_mNDkm=medianmND)
# #   
# #   p2 <- ggplot(dfSImND)+geom_point(aes(SIcut,med_mNDkm)) + geom_hline(yintercept = minKM, col="red")+geom_vline(xintercept = minSI, col="firebrick") +ggtitle("maxNetDispl_km ~ straightnessIndex")
# #   
# #   
# #   # summary(dfmv$straightnessIndex)
# #   p3 <- ggplot(dfmv)+geom_path(aes(coords.x1, coords.x2, color=straightnessIndex))+geom_point(aes(coords.x1, coords.x2, color=straightnessIndex), size=.1)+scale_colour_viridis_c(option = "viridis",direction=1,na.value="white")+coord_fixed() + ggtitle("all straightnessIndex")#+ ggspatial::annotation_scale(location = 'br')
# #   
# #   dfmv2 <- dfmv
# #   dfmv2$straightnessIndex[dfmv2$straightnessIndex>minSI &dfmv2$maxNetDispl_km>minKM] <- NA
# #   dfmv2$straightnessIndex[dfmv2$locsPerDay<7 ] <- NA
# #   p4 <- ggplot(dfmv2)+geom_path(aes(coords.x1, coords.x2, color=straightnessIndex))+geom_point(aes(coords.x1, coords.x2, color=straightnessIndex), size=.1)+scale_colour_viridis_c(option = "viridis",direction=1,na.value="white")+coord_fixed()  + ggtitle(paste0("remove straightnessIndex>",minSI," & maxNetDispl_km>", minKM))#+     ggspatial::annotation_scale(location = 'br')
# #   
# #   allnas <- data.frame(table(is.na(dfmv2$straightnessIndex)))
# #   
# #   p1 + p2 + p3 + p4 + labs(subtitle = paste0("NAs: ",allnas$Var1[1],": ", allnas$Freq[1] , " & ", allnas$Var1[2],": ", allnas$Freq[2] )) + 
# #     plot_annotation(title = paste0("sps: ",mv@idData$individual.taxon.canonical.name))
# # })
# # dev.off()
# # 
# # ## DECISION: 
# # # minKM <- 50 (to be considered migration day, animal has to travel more then 50km in a straight line (maxNetDispl)) and 
# # # minSI <- 0.7 (the straightnessIndex has to be over 0.7 to be considered as migration day)
# # # seem to make sense and work the best
