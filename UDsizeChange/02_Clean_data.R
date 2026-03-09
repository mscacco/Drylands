### in this script data are cleaned, empty locations and duplicated timestamps are removed ###

library(move2)
library(units)


library(doParallel)
library(plyr)
mycores <- detectCores()-1
registerDoParallel(mycores)
library(dplyr)

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pthDownld <- "1.vultureIndv_mv2/"
filePath <- paste0(genPath,pthDownld)
dir.create(paste0(genPath,"2.vultureIndv_mv2_clean_empty_duply"))
pthClean <- "2.vultureIndv_mv2_clean_empty_duply/"
savePath <- paste0(genPath,pthClean)


flsMV <- list.files(filePath, full.names = F)
done <- list.files(savePath, full.names = F)
flsMV <- flsMV[!flsMV%in%done]

###############
lastDwld <- "2024-12-13"
thisDwnl <- "2025-02-21"
library(R.utils)
tb_pth <- data.frame(pth=list.files(filePath, full.names=T),filenm=list.files(filePath, full.names=F), mbid=sub("_.*", "", list.files(filePath)))
tb_pth$lastSaved <- do.call(c,(lapply(tb_pth$pth, lastModified)))
FLSs_doneMdl <- unique(tb_pth$filenm[tb_pth$lastSaved >= as.POSIXct(thisDwnl)])

thisCalc <- "2025-02-24"
tb_pth_c <- data.frame(pth=list.files(savePath, full.names=T),filenm=list.files(savePath, full.names=F), mbid=sub("_.*", "", list.files(savePath)))
tb_pth_c$lastSaved <- do.call(c,(lapply(tb_pth_c$pth, lastModified)))
FLSs_done <- unique(tb_pth_c$filenm[tb_pth_c$lastSaved >= as.POSIXct(thisCalc)])

flsMV <- FLSs_doneMdl[!FLSs_doneMdl%in%FLSs_done]

#############




# ##### quick fix #####
# lastDwld <- "2024-10-08"
# library(R.utils)
# tb_pth <- data.frame(pth=list.files(pthDownld, full.names=T), mbid=sub("_.*", "", list.files(pthDownld)))
# tb_pth$lastSaved <- do.call(c,(lapply(tb_pth$pth, lastModified)))
# IDs_doneMdl <- unique(tb_pth$mbid[tb_pth$lastSaved >= as.POSIXct(lastDwld)])
# 
# Ids_toDo <- as.character(c(560810760,232133400,66002314,81273742,149548138,1781483507))
# flsMV <- do.call(c,(lapply(Ids_toDo, function(x) {list.files(filePath,pattern = x, full.names = F)})))
# #########


# indPth <- flsMV[1]
# indPth <- "2038660251_G31258 - B536 - 7216 - Wild.rds"
# indPth <- "2038601973_G26474 - Grey Black 04 - 7096 - Captive bred 2018.rds"

## remove empty locs
## remove duplicated ts
start_time <- Sys.time()
lapply(flsMV, function(indPth){
  # llply(flsMV, function(indPth){
  print(indPth)
  vultr <- readRDS(paste0(filePath,indPth))
  ## remove empty locs
  vultr <- vultr[!sf::st_is_empty(vultr),]
  ## sometimes only lat or long are NA
  crds <- sf::st_coordinates(vultr)
  rem <- unique(c(which(is.na(crds[,1])),which(is.na(crds[,2]))))
  if(length(rem)>0){vultr <- vultr[-rem,]}
  
  ## remove 0,0 coordinates
  rem0 <- which(crds[,1]==0 & crds[,2]==0)
  if(length(rem0)>0){vultr <- vultr[-rem0,]}
  
  ##retain the duplicate entry which contains the least number of columns with NA values
  vultr <- vultr %>%
    mutate(n_na = rowSums(is.na(pick(everything())))) %>%
    arrange(n_na) %>%
    mt_filter_unique(criterion='first') %>% # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA. 
    dplyr::arrange(mt_track_id()) %>%
    dplyr::arrange(mt_track_id(),mt_time())
  # vultr <- mt_filter_unique(vultr,criterion='first')
  ## ensure timestamps are ordered within tracks
  # vultr <- dplyr::arrange(vultr, mt_track_id(vultr), mt_time(vultr))
  
  saveRDS(vultr, file=paste0(savePath,indPth))
  rm(vultr)
  gc()
} )
# } ,.parallel = T)
end_time <- Sys.time()
end_time - start_time # 4.5h

################# FIXES ##############
# ## animals of study 1109284853 are missing taxon: Vultur gryphus - emiliy addded the sps in the study
# lfls <- list.files(savePath, pattern = "1109284853", full.names = T )
# lapply(lfls, function(mv2){
#   vultr <- readRDS(mv2)
#   vultr <- mutate_track_data(vultr, taxon_canonical_name = "Vultur gryphus")
#   saveRDS(vultr, file=mv2)
# })


lfls <- list.files(savePath, pattern = "2770553714", full.names = T )
lapply(lfls, function(mv2){
  vultr <- readRDS(mv2)
  vultr <- mutate_track_data(vultr, taxon_canonical_name = "Gyps africanus")
  saveRDS(vultr, file=mv2)
})

## this should be done here, but the large individulas crash the pc due to RAM-> doing filtering on 1h tracks
# ## check distribution of speeds
# flsMVs <- list.files(savePath, full.names = T)
# indPth <- flsMVs[200]
# start_time <- Sys.time()
# speedL <- lapply(flsMVs, function(indPth){
#   vultr <- readRDS(indPth)
#   vultr_speed <- mt_speed(vultr)
#   vultr_speed <- set_units(vultr_speed, m/s)
#   return(vultr_speed)
# })
# end_time <- Sys.time()
# end_time - start_time #50mins
# 
# saveRDS(speedL, file=paste0(genPath,"speed_all_list",".rds"))
# 
# speedL <- readRDS(paste0(genPath,"speed_all_list",".rds"))
# speedAll <- unlist(speedL)
# speedAll <- speedAll[!is.na(speedAll)]
# hist(speedAll[speedAll<30])
# round(quantile(speedAll, seq(0.99,1,0.001)),2)
# # 99%       99.1%       99.2%       99.3%       99.4%       99.5%       99.6%       99.7%       99.8%       99.9%        100% 
# # 20.37       20.64       20.94       21.28       21.67       22.13       22.71       23.47       24.59       26.68 20015118.21 
# 
# ## remove speeds higher than threshold 25 -- remove top 0.2%
# flsMVs <- list.files(savePath, full.names = T)
# done <- paste0(genPath,"2.storkIndv_mt_clean_empty_duply/",list.files(paste0(genPath,"3.storkIndv_mt_clean_empty_duply_outlspeed/")))
# flsMVs <- flsMVs[!flsMVs%in%done]
# 
# # indPth <- flsMVs[1]
# start_time <- Sys.time()
# results <- lapply(flsMVs, function(indPth)try({
#   print(indPth)
#   vultr <- readRDS(indPth)
#   vultr_speed <- mt_speed(vultr)
#   vultr_speed <- set_units(vultr_speed, m/s)
#   maxspeed <- max(vultr_speed[!is.na(vultr_speed)])
#   
#   while(maxspeed>set_units(25, m/s)){
#     # vultr_speed <- mt_speed(vultr)
#     # vultr_speed <- set_units(vultr_speed, m/s)
#     # vultr_speed <- vultr_speed[!is.na(vultr_speed)]
#     # vultr <-  vultr[c(T,vultr_speed<=set_units(25, m/s)),]
#     # vultr <- vultr %>% group_by(mt_track_id())
#     # vultr <- filter(vultr,!mt_speed(vultr)>set_units(25, m/s))
#     vultr <- vultr %>% group_by(mt_track_id()) %>% filter(!mt_speed(vultr)>set_units(25, m/s))
#     vultr_speed <- mt_speed(vultr)
#     vultr_speed <- set_units(vultr_speed, m/s)
#     maxspeed <- max(vultr_speed[!is.na(vultr_speed)])
#     print(maxspeed)
#   }
#   indiv <- unique(mt_track_data(vultr)$individual_local_identifier)
#   if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
#   saveRDS(vultr, file=paste0(genPath,"3.storkIndv_mt_clean_empty_duply_outlspeed/",unique(mt_track_data(vultr)$study_id),"_",indiv,".rds")) 
# }))
# end_time <- Sys.time()
# end_time - start_time # +170ind
# 
# is.error <- function(x) inherits(x, "try-error")
# table(vapply(results, is.error, logical(1)))
# names(results) <- seq_along(results)
# results[vapply(results, is.error, logical(1))]