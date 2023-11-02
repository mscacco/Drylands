### in this script data is downloaded from movebank, each individual is downloaded separately ###

library(move2)
library(bit64)
library(units)
genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
dir.create(paste0(genPath,"1.vultureIndv_mv2"))
pthDownld <- paste0(genPath,"1.vultureIndv_mv2/")


#setwd("/home/mscacco/ownCloud/DryLands/scripts")
dir.create("licenseTerms")

# # list accounts
# keyring::key_list()

# specify account to use in the R session
options("move2_movebank_key_name" = "drylands")

# download list of studies available through this account
all <- movebank_download_study_info(study_permission=c("data_manager","collaborator"))
# Remove potential studies not having deployed locations
all <- all[all$number_of_deployed_locations > set_units(0, "count"),]

vroom::problems(all) #years in weird formats

names(all)
all$name
all$contact_person_name
all$id
all$timestamp_first_deployed_location
sort(all$timestamp_last_deployed_location)
sort(all$number_of_deployed_locations)

# some summary info on the available data (updated July 13th)
library(lubridate)
range(c(year(all$timestamp_first_deployed_location), #from 2006
        year(all$timestamp_last_deployed_location)))
sum(all$number_of_individuals) #1768 individuals
length(unique(all$contact_person_name)) #32 data owners
unique(unlist(strsplit(all$taxon_ids, ","))) #16 species
sum(all$number_of_deployed_locations) #236'510'519 locations

# ## check which studies I already have downloaded and if number of indiv. are the same as the available on MB, only applys after 1st round. For 1st round directly run ~L28
# IDs_done <- sub("_[^_]+$", "", list.files(paste0(genPath,pthDownld)))
# indvPerStud <- data.frame(table(IDs_done))
# colnames(indvPerStud)[1] <- "id"
# indvPerStud$id <- bit64::as.integer64(as.character(indvPerStud$id))
# allIndvStudy <- all[,c("id", "number_of_individuals")]
# compareDF <- merge(allIndvStudy,indvPerStud, by= "id", all=T)
# compareDF$Freq <- units::set_units(compareDF$Freq, "count")
# compareDF <- compareDF[!compareDF$number_of_individuals-compareDF$Freq==units::set_units(0,"count") |is.na(compareDF$Freq),]

Ids_toDo <- all$id ## first round
# Ids_toDo <- compareDF$id

start_time <- Sys.time()

# studyId <- Ids_toDo[1]
# studyId <-1498452485

results <- lapply(Ids_toDo, function(studyId)try({
  ## create table with individuals per study, to be able to download per indivdual
  class(studyId) <- "integer64" ## lapply changes the class when looping though it
  print(studyId)
  reftb <- movebank_download_deployment(study_id=studyId, omit_derived_data=F )
  # vroom::problems(reftb)
  # reftb <- reftb[reftb$taxon_canonical_name%in%c("Ciconia ciconia"),]
  reftb <- reftb[reftb$number_of_events > units::set_units(0,"count"),]
  # reftb <- reftb[grep("gps",reftb$sensor_type_ids),]
  allindv <- as.character(unique(reftb$individual_local_identifier)) 
  
  ## check which individuals have already been downloaded
  indivds <- unlist(lapply(allindv, function(x){if(grepl("/", x)==T){gsub("/","-",x)}else{x}}))
  allindvPth <- paste0(studyId, "_",indivds,".rds")
  dwnIndv <- list.files(paste0(genPath,pthDownld), pattern = as.character(studyId))
  allindv2 <- allindv[which(!allindvPth%in%dwnIndv)]
  
  ## download each individual separatly
  # ind <- allindv2[2]
  results2 <- lapply(allindv2, function(ind)try({
    mv2 <- movebank_download_study(studyId,
                                   sensor_type_id=c("gps","argos-doppler-shift"),
                                   individual_local_identifier= ind,
                                   timestamp_end=as.POSIXct(Sys.time(), tz="UTC")) # to avoid locations in the future

    
    indiv <- ind
    if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
    print(indiv)
    # saveRDS(mv2, file=paste0(genPath,pthDownld,studyId,"_",indiv,".rds"))
  }))
}))
end_time <- Sys.time()
end_time-start_time # ~ 

# saveRDS(results,paste0(saveErrorPath,"all",".rds"))

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
# Check studies that returned errors:
giveError <- Ids_toDo[vapply(results, is.error, logical(1))]

tberr <- all[as.numeric(all$id)%in%giveError,]


#_____________________________
## Study summary table: ####
# Create a summary table per study where we write which studies were downloaded, and how many of the available individuals were actually downloaded? 

studsT <- read.csv("MovementData/availableWikelskiStudies_accessed29July2022.csv", as.is=T)

didNotDownload <- read.csv("MovementData/studiesThatDidNotDownaloadViaAPI.csv", as.is=T)
didNotDownload$studyDownloaded <- "missing individual id - re-assigned after manual download"
didNotDownload$studyDownloaded[c(4,7,9,10,11,13)] <- "yes manually"

fls <- list.files("MovementData/RawData", pattern="onlyGps.rds", full.names = T)
studies_downloadTable <- as.data.frame(rbindlist(lapply(fls, function(f){
  gps <- readRDS(f)
  print(gps$study.id[1])
  df <- data.frame(id=gps$study.id[1], 
                   studyDownloaded = "yes from API",
                   downloadedIndividuals=length(unique(gps$individual.local.identifier)), stringsAsFactors = F)
  return(df)
})))

downloadedStudies <- as.data.frame(rbindlist(list(didNotDownload[,c("id","studyDownloaded")], studies_downloadTable), fill=T))

studies_summaryTable <- merge(studsT[,c("id","name","contact_person_name","number_of_individuals")], downloadedStudies, by="id", all.x=T)
studies_summaryTable$studyDownloaded[is.na(studies_summaryTable$studyDownloaded)] <- "no - no individual with GPS data available"

studies_summaryTable <- studies_summaryTable[order(studies_summaryTable$id),c("id","name","number_of_individuals","downloadedIndividuals","studyDownloaded","contact_person_name")]

write.csv(studies_summaryTable, file="MovementData/studies_summaryTable_downloadOrNot.csv", row.names = F)

