### in this script data are cleaned, empty locations and duplicated timestamps are removed ###

library(move2)
library(units)
library(dplyr)

genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
pthDownld <- "1.vultureIndv_mv2/"
filePath <- paste0(genPath,pthDownld)
dir.create(paste0(genPath,"2.vultureIndv_mv2_clean_empty_duply"))
pthClean <- "2.vultureIndv_mv2_clean_empty_duply/"
savePath <- paste0(genPath,pthClean)


flsMV <- list.files(filePath, full.names = T)

# indPth <- paste0(genPath,"/1.storkIndv_mt/","21231406_Zozu + - DER AL581 (eobs2541).rds")

## remove empty locs
## remove duplicated ts
start_time <- Sys.time()
results <- lapply(flsMV, function(indPth)try({
  vultr <- readRDS(indPth)
  ## remove empty locs
  vultr <- vultr[!sf::st_is_empty(vultr),]
  ## sometimes only lat or long are NA
  crds <- sf::st_coordinates(vultr)
  rem <- unique(c(which(is.na(crds[,1])),which(is.na(crds[,2]))))
  if(length(rem)>0){vultr <- vultr[-rem,]}
  ##retain the duplicate entry which contains the least number of columns with NA values
  vultr <- vultr %>% rowwise() %>%
    mutate(n_na = sum(is.na(cur_data()))) %>%
     arrange(n_na) %>%
     mt_filter_unique(criterion='first')

  indiv <- unique(mt_track_data(vultr)$individual_local_identifier)
  if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
  saveRDS(vultr, file=paste0(savePath,unique(mt_track_data(vultr)$study_id),"_",indiv,".rds"))
}))
end_time <- Sys.time()
end_time - start_time # 1.14h

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]


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