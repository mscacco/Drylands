### in this script data are subset to 1h interval, and ouliers according to speed are removed ###

#ToDo: remove 1st location as his will not be 1h apart, or adjust to the new mt_filter_lag() or similar when it exists

genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
referenceTableStudies <- readRDS(paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
referenceTableStudiesUsed <- referenceTableStudies[referenceTableStudies$excluded=="no",]
head(referenceTableStudiesUsed)
summary(referenceTableStudiesUsed)

# allindv <- referenceTableStudiesUsed$individual_local_identifier
# referenceTableStudiesUsed$individual_local_identifier_files <- unlist(lapply(allindv, function(x){if(grepl("/", x)==T){gsub("/","-",x)}else{x}}))

filepath <- paste0(genPath,"2.vultureIndv_mv2_clean_empty_duply/")
dir.create(paste0(genPath,"3.vultureIndv_mv2_1h"))
savePath <- paste0(genPath,"3.vultureIndv_mv2_1h/")
dir.create(paste0(genPath,"4.vultureIndv_mv2_1h_outlspeed"))
savePathOutl <- paste0(genPath,"4.vultureIndv_mv2_1h_outlspeed/")

flsMV2 <- as.character(referenceTableStudiesUsed$fileName)
flsMV2[1]

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
end_time-start_time # ~20mins

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
end_time - start_time # ~5mins

saveRDS(speedL, file=paste0(genPath,"speed_all_list",".rds"))

genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
speedL <- readRDS(paste0(genPath,"speed_all_list",".rds"))
speedAll <- unlist(speedL)
speedAll <- speedAll[!is.na(speedAll)]
hist(speedAll[speedAll<19])
round(quantile(speedAll, seq(0.99,1,0.001)),2)
# 99%     99.1%     99.2%     99.3%     99.4%     99.5%     99.6%     99.7%     99.8%     99.9%      100% 
# 11.07     11.38     11.74     12.14     12.62     13.22     14.01     15.24     18.56   1119.42 177413.50 

## remove speeds higher than threshold 18 -- remove top 0.2%
library(dplyr)
library(move2)
flsMVs <- list.files(savePath, full.names = F)
# indPth <- flsMVs[1000]
start_time <- Sys.time()
maxspeed <- 18
results <- lapply(flsMVs, function(indPth)try({
  print(indPth)
  vultr <- readRDS(paste0(savePath,indPth))
  while(any(mt_speed(vultr, units="m/s")>set_units(maxspeed, m/s), na.rm = TRUE)){
    vultr <- vultr %>% filter(mt_speed(., units="m/s")<=set_units(maxspeed, m/s) | is.na(mt_speed(., units="m/s")))
  }
  saveRDS(vultr, file=paste0(savePathOutl,indPth))
}))
end_time <- Sys.time()
end_time - start_time # 22min

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]

