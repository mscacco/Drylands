### in this script data are subset to 1h interval, and ouliers according to speed are removed ###

#ToDo: remove 1st location as his will not be 1h apart, or adjust to the new mt_filter_lag() or similar when it exists

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
referenceTableStudies <- readRDS(paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
referenceTableStudiesUsed <- referenceTableStudies[referenceTableStudies$excluded=="no",]
head(referenceTableStudiesUsed)
summary(referenceTableStudiesUsed)

referenceTableStudiesUsed <- referenceTableStudiesUsed[!is.na(referenceTableStudiesUsed$species),]

# allindv <- referenceTableStudiesUsed$individual_local_identifier
# referenceTableStudiesUsed$individual_local_identifier_files <- unlist(lapply(allindv, function(x){if(grepl("/", x)==T){gsub("/","-",x)}else{x}}))

filepath <- paste0(genPath,"2.vultureIndv_mv2_clean_empty_duply/")
dir.create(paste0(genPath,"3.vultureIndv_mv2_1h"))
savePath <- paste0(genPath,"3.vultureIndv_mv2_1h/")
dir.create(paste0(genPath,"4.vultureIndv_mv2_1h_outlspeed"))
savePathOutl <- paste0(genPath,"4.vultureIndv_mv2_1h_outlspeed/")
dir.create(paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist"))
savePathOutlDist <- paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/")


flsMV2 <- as.character(referenceTableStudiesUsed$fileName)
flsMV2[1]

done <- list.files(savePath, full.names = F)
flsMV2 <- flsMV2[!flsMV2%in%done]

# ###############
# thisCalc <- "2025-02-24"
# library(R.utils)
# tb_pth <- data.frame(pth=list.files(filePath, full.names=T),filenm=list.files(filePath, full.names=F), mbid=sub("_.*", "", list.files(filePath)))
# tb_pth$lastSaved <- do.call(c,(lapply(tb_pth$pth, lastModified)))
# FLSs_doneMdl <- unique(tb_pth$filenm[tb_pth$lastSaved >= as.POSIXct(thisCalc)])
# 
# thisCalc <- "2025-02-24"
# tb_pth_c <- data.frame(pth=list.files(savePath, full.names=T),filenm=list.files(savePath, full.names=F), mbid=sub("_.*", "", list.files(savePath)))
# tb_pth_c$lastSaved <- do.call(c,(lapply(tb_pth_c$pth, lastModified)))
# FLSs_done <- unique(tb_pth_c$filenm[tb_pth_c$lastSaved >= as.POSIXct(thisCalc)])
# 
# flsMV2 <- FLSs_doneMdl[!FLSs_doneMdl%in%FLSs_done]
# 
# #############


# library(amt)
# library(lubridate)
library(move2)
library(units)

# ind <- flsMV2[283] #c(283,284,951,952)

is.error <- function(x) inherits(x, "try-error")
start_time<- Sys.time()
results <- lapply(flsMV2, function(ind)try({
  vultr <- readRDS(paste0(filepath,ind))
  vultr_thinned <- mt_filter_per_interval(vultr,criterion = "first",unit="hour")
  vultr_thinned <- vultr_thinned[-1,] ## just in case
  
  saveRDS(vultr_thinned, file=paste0(savePath,ind))
}))
end_time <- Sys.time() 
end_time-start_time # ~47mins

table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]

library(units)
## check distribution of speeds
flsMVs <- list.files(savePath, full.names = T)
# indPth <- flsMVs[200]
start_time <- Sys.time()
speedL <- lapply(flsMVs, function(indPth){
  vultr <- readRDS(indPth)
  vultr_speed <- mt_speed(vultr, units="m/s")
  return(vultr_speed)
})
end_time <- Sys.time()
end_time - start_time # ~10mins

saveRDS(speedL, file=paste0(genPath,"speed_all_list",".rds"))

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
speedL <- readRDS(paste0(genPath,"speed_all_list",".rds"))
speedAll <- unlist(speedL)
speedAll <- speedAll[!is.na(speedAll)]
hist(speedAll)
hist(speedAll[speedAll<19])
round(quantile(speedAll, seq(0.99,1,0.0001)),2)

# #### quick fix ###
# Ids_toDo <- as.character(c(560810760,232133400,66002314,81273742,149548138,1781483507))
# flsMVs <- do.call(c,(lapply(Ids_toDo, function(x) {list.files(savePath,pattern = x, full.names = F)})))
# #########

## remove speeds higher than threshold 20 -- remove top 0.15%
library(dplyr)
library(move2)
flsMVs <- list.files(savePath, full.names = F)
# indPth <- flsMVs[1000]
start_time <- Sys.time()
maxspeed <- 20
results <- lapply(flsMVs, function(indPth)try({
  print(indPth)
  vultr <- readRDS(paste0(savePath,indPth))
  while(any(mt_speed(vultr, units="m/s")>set_units(maxspeed, m/s), na.rm = TRUE)){
    vultr <- vultr %>% filter(mt_speed(., units="m/s")<=set_units(maxspeed, m/s) | is.na(mt_speed(., units="m/s")))
  }
  saveRDS(vultr, file=paste0(savePathOutl,indPth))
}))
end_time <- Sys.time()
end_time - start_time # 40min

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]

### remove outliers based on distance
## check distribution of speeds
flsMVs <- list.files(savePathOutl, full.names = T)
indPth <- flsMVs[1]
start_time <- Sys.time()
distL <- lapply(flsMVs, function(indPth){
  vultr <- readRDS(indPth)
  vultr_dist <- mt_distance(vultr, units="m")
  return(vultr_dist)
})
end_time <- Sys.time()
end_time - start_time #10min

distAll <- unlist(distL)
distAll <- distAll[!is.na(distAll)]
hist(distAll)
round(quantile(distAll, seq(0.9,1,0.01)),2)
hist(distAll[distAll<50000])
round(quantile(distAll, seq(0.999,1,0.00001)))

dl <- distAll[distAll>1000000]


## remove distances higher than threshold 1000K km -- remove top 0.0001%
# #### quick fix ###
# Ids_toDo <- as.character(c(560810760,232133400,66002314,81273742,149548138,1781483507))
# flsMVs <- do.call(c,(lapply(Ids_toDo, function(x) {list.files(savePathOutl,pattern = x, full.names = F)})))
# #########
library(dplyr)
library(move2)
flsMVs <- list.files(savePathOutl, full.names = F)
# indPth <- flsMVs[1000]
start_time <- Sys.time()
maxdist <- 1000000
results <- lapply(flsMVs, function(indPth)try({
  print(indPth)
  vultr <- readRDS(paste0(savePathOutl,indPth))
  vultr <- vultr %>% filter(mt_distance(., units="m")<=set_units(maxdist, m) | is.na(mt_distance(., units="m")))
  saveRDS(vultr, file=paste0(savePathOutlDist,indPth))
}))
end_time <- Sys.time()
end_time - start_time # 25min

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
