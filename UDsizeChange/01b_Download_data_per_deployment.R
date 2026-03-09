### in this script data is downloaded from movebank, each individual is downloaded separately ###

library(move2)
library(bit64)
library(units)
genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
dir.create(paste0(genPath,"1.vultureIndv_mv2"))
pthDownld <- paste0(genPath,"1.vultureIndv_mv2/")


#setwd("/home/mscacco/ownCloud/DryLands/scripts")
# dir.create("licenseTerms")

# # list accounts
# keyring::key_list()

# specify account to use in the R session
options("move2_movebank_key_name" = "Drylands")

### studies to download
sharedStudies <- readRDS(paste0(genPath,"table_shared_studies_20250221.rds"))
publicStudies <- readRDS(paste0(genPath,"table_public_studies_20250221.rds"))

all <- rbind(sharedStudies,publicStudies) # 99 13.12.24

###############################
### save a table with license type and license terms to have them all in one place
##################################
license_table <- all[,c("id","name","taxon_ids","principal_investigator_name","contact_person_name","license_type","license_terms")]
write.csv(license_table, paste0(genPath,"license_terms_table.csv"))

names(all)
# all$name
# unique(all$contact_person_name)
# all$id
# all$timestamp_first_deployed_location
# sort(all$timestamp_last_deployed_location)
# sort(all$number_of_deployed_locations)

# some summary info on the available data (updated Dec 13th)(with pub studies)
library(lubridate)
range(c(year(all$timestamp_first_deployed_location), #from 2006 (1994)
        year(all$timestamp_last_deployed_location)),na.rm=T)
sum(all$number_of_individuals) #3211 individuals
length(unique(all$id)) #99 MB studies 
length(unique(all$contact_person_name)) #51 data owners 
sort(unique(unlist(strsplit(all$taxon_ids, ",")))) #17 species
sum(all$number_of_deployed_locations) #659253086 locations 

## check which studies are already downloaded. If last timestamp after last download date, download again, if not, jump. For 1st round directly run ~L28

lastDwld <- "2024-12-13"
thisDwnl <- "2025-02-21" ## if interrupted

##################
## when download is interrupted in the middle (internet connection in Bückle)
library(R.utils)

tb_pth <- data.frame(pth=list.files(pthDownld, full.names=T),filenm=list.files(pthDownld, full.names=F), mbid=sub("_.*", "", list.files(pthDownld)))

tb_pth$lastSaved <- do.call(c,(lapply(tb_pth$pth, lastModified)))

IDs_doneMdl <- unique(tb_pth$mbid[tb_pth$lastSaved >= as.POSIXct(thisDwnl)])
###################


IDs_done <- unique(sub("_.*", "", list.files(pthDownld)))
compareDF <- all
compareDF$status <- NA
compareDF$status[compareDF$id%in%IDs_done] <- "done"
compareDF$status[compareDF$status=="done" & compareDF$timestamp_last_deployed_location>as.POSIXct(lastDwld, tz="UTC")] <- "live"
table(compareDF$status)
table(is.na(compareDF$status))

Ids_toDo <- all$id ## first round
# and # subsequent rounds
Ids_toDo <- compareDF$id[!compareDF$status%in%c("done")]
# and ## interrupted in the middle
Ids_toDo <- Ids_toDo[!Ids_toDo%in%IDs_doneMdl]
# Ids_toDo_later <- as.integer64(c(560810760,232133400,66002314,81273742,149548138,1781483507))
# Ids_toDo <- Ids_toDo[!Ids_toDo%in%Ids_toDo_later]

start_time <- Sys.time()

# studyId <- Ids_toDo[1]
# studyId <-560810760

# Ids_toDo <- as.integer64(c(560810760,232133400,66002314,81273742,149548138,1781483507,4838072015))

# Ids_toDo <- refTableWeb$StudyID
newerToday <- tb_pth$filenm[tb_pth$lastSaved >= as.POSIXct(thisDwnl)] ## to exclude those downloaded today already
# lastDownload <- tb_pth$filenm[tb_pth$lastSaved >= as.POSIXct(lastDwld)] ## to download missed last time

results <- lapply(Ids_toDo, function(studyId)try({
  ## create table with individuals per study, to be able to download per indivdual
  class(studyId) <- "integer64" ## lapply changes the class when looping though it
  print(studyId)
    ## if license terms have to be accepted, this is done here. These license terms are recorded in the metadata_table of script 1a
  reftb <-  tryCatch({
    movebank_download_deployment(study_id=studyId, omit_derived_data=F)
  }, error = function(e) {
    movebank_download_deployment(study_id=studyId, omit_derived_data=F,
                                 'license-md5'= sub('...Alternat.*','',sub('.*se-md5.=.','',as.character(rlang::catch_cnd(movebank_download_study(studyId))))))
  })
  # vroom::problems(reftb)
  reftb <- reftb[reftb$number_of_events > units::set_units(0,"count"),]
  indiv <- reftb$individual_local_identifier
  if(any(grepl("/", indiv)==T)){indiv <- gsub("/","-",indiv)}
  reftb$individual_local_identifierNObsl <- indiv
  reftb$pthName <- paste0(studyId,"__",reftb$individual_local_identifierNObsl,"__",reftb$deployment_id,".rds")
  # allindv <- unique(reftb$individual_id) ## using indv_id as sometimes indv_loc_idenitf gives error or does not exist
  doneIndv <- list.files(pthDownld)[grep(studyId,list.files(pthDownld))] ## indiv downloaded
  # allStInd <- reftb$pthName ## individuals in study
  # missInd <- allStInd[!allStInd%in%doneIndv] ## missing indiv
  ## identify indv that are live 
  reftb$status <- NA
  reftb$status[reftb$pthName%in%doneIndv] <- "done"
  reftb$status[reftb$status=="done" & reftb$timestamp_end_individual>as.POSIXct(lastDwld, tz="UTC")] <- "live"
  liveAndmissingIndv <- reftb$pthName[!reftb$status%in%c("done")]
  liveAndmissingIndv <- liveAndmissingIndv[!liveAndmissingIndv%in%newerToday] # to exclude those downloaded today already
  # liveAndmissingIndv <- liveAndmissingIndv[!liveAndmissingIndv%in%lastDownload] # to download missed last download
  
  
  print(paste0("done:",length(doneIndv),"-todo:",length(liveAndmissingIndv)))
  
  # missIndstrp <- sub("^\\d+_(.+)\\.rds$", "\\1", liveAndmissingIndv)
  # ^\\d+_ matches the initial number and underscore
  # (.+) captures everything after that until the file extension
  # \\.rds$ matches the ".rds" at the end of the string
  # The \\1 in the replacement refers to the captured group, effectively keeping only the middle part of the string.
  
  todoIndv <-  reftb$deployment_id[reftb$pthName%in%liveAndmissingIndv] ## using deployment id to have one file per deployment and reduce mistakes downstream. Also single objects might get smaller and better to handle
  # first round
  # todoIndv <- allindv
  
  ## download each individual separatly
  # dpy <- todoIndv[2]
  results2 <- lapply(todoIndv, function(dpy)try({
    class(dpy) <- "integer64" ## lapply changes the class when looping though it
    print(paste0(studyId,"_",dpy))
    mv2 <- movebank_download_study(studyId,
                                   sensor_type_id=c("gps","argos-doppler-shift","radio-transmitter"),
                                   # individual_local_identifier= ind,
                                   deployment_id=dpy,
                                   timestamp_end=as.POSIXct(Sys.time(), tz="UTC")) # to avoid locations in the future
    mt_track_id(mv2) <- "deployment_id"
    
    if("individual_local_identifier" %in% names(mt_track_data(mv2))){
      indiv <- as.character(unique(mt_track_data(mv2)$individual_local_identifier))
      if(any(grepl("/", indiv)==T)){indiv <- gsub("/","-",indiv)}
      print(indiv)
    }else if("individual_local_identifier" %in% names(mv2)){
      indiv <- as.character(unique(mv2$individual_local_identifier))
      if(any(grepl("/", indiv)==T)){indiv <- gsub("/","-",indiv)}
      print(indiv)
    }else{
      indiv <- mt_track_data(mv2)$individual_id
      print(indiv)
    }
    saveRDS(mv2, file=paste0(pthDownld,studyId,"__",indiv,"__",dpy,".rds"))
  }))
}))
end_time <- Sys.time()
end_time-start_time # ~ 12h + 21h +1h

# saveRDS(results,paste0(saveErrorPath,"all",".rds"))

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
# Check studies that returned errors:
giveError <- Ids_toDo[vapply(results, is.error, logical(1))]

tberr <- all[as.numeric(all$id)%in%giveError,]

# ###### for those studies that need accepting license terms ## need to change it for non accepted license terms
# 
# resultsEr <- lapply(giveError, function(studyId)try({
#   ## create table with individuals per study, to be able to download per indivdual
#   class(studyId) <- "integer64" ## lapply changes the class when looping though it
#   print(studyId)
#   reftb <- movebank_download_deployment(study_id=studyId, omit_derived_data=F,
#                                         'license-md5'= sub('...Alternat.*','',sub('.*se-md5.=.','',as.character(rlang::catch_cnd(movebank_download_study(studyId))))))
#   # vroom::problems(reftb)
#   reftb <- reftb[reftb$number_of_events > units::set_units(0,"count"),]
#   indiv <- reftb$individual_local_identifier
#   if(any(grepl("/", indiv)==T)){indiv <- gsub("/","-",indiv)}
#   reftb$individual_local_identifierNObsl <- indiv
#   allindv <- unique(reftb$individual_id) ## using indv_id as sometimes indv_loc_idenitf gives error or does not exist
#   doneIndv <- list.files(pthDownld)[grep(studyId,list.files(pthDownld))]
#   allStInd <- paste0(studyId,"_",reftb$individual_local_identifierNObsl,".rds")
#   missInd <- allStInd[!allStInd%in%doneIndv]
#   
#   missIndstrp <- sub("^\\d+_(.+)\\.rds$", "\\1", missInd)
#   # ^\\d+_ matches the initial number and underscore
#   # (.+) captures everything after that until the file extension
#   # \\.rds$ matches the ".rds" at the end of the string
#   # The \\1 in the replacement refers to the captured group, effectively keeping only the middle part of the string.
#   
#   todoIndv <-  reftb$individual_id[reftb$individual_local_identifierNObsl%in%missIndstrp]
#   # first round
#   # todoIndv <- allindv
#   
#   ## download each individual separatly
#   # ind <- allindv[2]
#   results2 <- lapply(todoIndv, function(ind)try({
#     class(ind) <- "integer64" ## lapply changes the class when looping though it
#     print(ind)
#     mv2 <- movebank_download_study(studyId,
#                                    sensor_type_id=c("gps","argos-doppler-shift","radio-transmitter"),
#                                    # individual_local_identifier= ind,
#                                    individual_id=ind,
#                                    timestamp_end=as.POSIXct(Sys.time(), tz="UTC")) # to avoid locations in the future
#                                    
#     if("individual_local_identifier" %in% names(mt_track_data(mv2))){
#       indiv <- unique(mt_track_data(mv2)$individual_local_identifier)
#       if(any(grepl("/", indiv)==T)){indiv <- gsub("/","-",indiv)}
#       print(indiv)
#     }else{
#       indiv <- mt_track_data(mv2)$individual_id
#       print(indiv)
#     }
#     saveRDS(mv2, file=paste0(pthDownld,studyId,"_",indiv,".rds"))
#   }))
# }))
# 


#######################
## make table with available studyID and indiv names, to find out which are missing:

dfIndiv_L <- lapply(Ids_toDo, function(studyId){
  ## create table with individuals per study, to be able to download per indivdual
  class(studyId) <- "integer64" ## lapply changes the class when looping though it
  print(studyId)
  reftb <- movebank_download_deployment(study_id=studyId, omit_derived_data=F )
  # vroom::problems(reftb)
  reftb <- reftb[reftb$number_of_events > units::set_units(0,"count"),]
  
  
  if("individual_local_identifier" %in% names(reftb)){
    allindv <- as.character(unique(reftb$individual_local_identifier)) 
    indivds <- unlist(lapply(allindv, function(x){if(grepl("/", x)==T){gsub("/","-",x)}else{x}}))
    allindvPth <- paste0(studyId, "_",indivds,".rds")
  }else{
    allindv <- as.character(unique(reftb$individual_id)) 
    indivds <- allindv #unlist(lapply(allindv, function(x){if(grepl("/", x)==T){gsub("/","-",x)}else{x}}))
    allindvPth <- paste0(studyId, "_",indivds,".rds")
  }
  
  df <- data.frame(studyID=rep(studyId, length(indivds)), indiv=indivds, fileName=allindvPth)
  return(df)
})

dfIndiv <- do.call("rbind",dfIndiv_L)

dwnIndv <- data.frame(fileName=list.files(pthDownld), downl="y")

df <- merge(dfIndiv,dwnIndv,all=T)

unique(df$studyID[is.na(df$downl)])

#_____________________________
## Study summary table: ####
# Create a summary table per study where we write which studies were downloaded, and how many of the available individuals were actually downloaded? 

# studsT <- read.csv("MovementData/availableWikelskiStudies_accessed29July2022.csv", as.is=T)
# 
# didNotDownload <- read.csv("MovementData/studiesThatDidNotDownaloadViaAPI.csv", as.is=T)
# didNotDownload$studyDownloaded <- "missing individual id - re-assigned after manual download"
# didNotDownload$studyDownloaded[c(4,7,9,10,11,13)] <- "yes manually"
# 
# fls <- list.files("MovementData/RawData", pattern="onlyGps.rds", full.names = T)
# studies_downloadTable <- as.data.frame(rbindlist(lapply(fls, function(f){
#   gps <- readRDS(f)
#   print(gps$study.id[1])
#   df <- data.frame(id=gps$study.id[1], 
#                    studyDownloaded = "yes from API",
#                    downloadedIndividuals=length(unique(gps$individual.local.identifier)), stringsAsFactors = F)
#   return(df)
# })))
# 
# downloadedStudies <- as.data.frame(rbindlist(list(didNotDownload[,c("id","studyDownloaded")], studies_downloadTable), fill=T))
# 
# studies_summaryTable <- merge(studsT[,c("id","name","contact_person_name","number_of_individuals")], downloadedStudies, by="id", all.x=T)
# studies_summaryTable$studyDownloaded[is.na(studies_summaryTable$studyDownloaded)] <- "no - no individual with GPS data available"
# 
# studies_summaryTable <- studies_summaryTable[order(studies_summaryTable$id),c("id","name","number_of_individuals","downloadedIndividuals","studyDownloaded","contact_person_name")]
# 
# write.csv(studies_summaryTable, file="MovementData/studies_summaryTable_downloadOrNot.csv", row.names = F)

