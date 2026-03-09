### in this script data is downloaded from movebank, each individual is downloaded separately ###
## when downloading data again it only will download data of animals that have more data than in the last download ##
## if download is interrupted, than the code can check which data have been downloaded today and ignore those ###

library(move2)
library(bit64)
library(units)
genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
dir.create(paste0(genPath,"1.vultureIndv_mv2"))
pthDownld <- paste0(genPath,"1.vultureIndv_mv2/")


## list accounts
# keyring::key_list()

## specify account to use in the R session
options("move2_movebank_key_name" = "Drylands")

#### downloading studies to which the user "Drylands MPIAB" has been added as collaborator or manager
## download list of studies available through this account
all <- movebank_download_study_info(study_permission=c("data_manager","collaborator"))

###############################
### save a table with license type and license terms to have them all in one place ## only relevant when using shared studies from external partners
##################################
license_table <- all[,c("id","name","taxon_ids","principal_investigator_name","contact_person_name","license_type","license_terms")]
write.csv(license_table, paste0(genPath,"license_terms_table.csv"))

## some summary info on the available data 
library(lubridate)
range(c(year(all$timestamp_first_deployed_location), 
        year(all$timestamp_last_deployed_location)))
sum(all$number_of_individuals) 
length(unique(all$id))
length(unique(all$contact_person_name))
sort(unique(unlist(strsplit(all$taxon_ids, ","))))
sum(all$number_of_deployed_locations) 

## check which studies are already downloaded. If last timestamp after last download date, download again, if not, jump. For 1st round directly run ~L28
lastDwld <- "2024-11-25" # date of last download
thisDwnl <- "2024-12-13" ## date of this download (if interrupted)

##################
## when download is interrupted in the middle (internet connection in Bückle)
library(R.utils)
tb_pth <- data.frame(pth=list.files(pthDownld, full.names=T),filenm=list.files(pthDownld, full.names=F), mbid=sub("_.*", "", list.files(pthDownld)))
tb_pth$lastSaved <- do.call(c,(lapply(tb_pth$pth, lastModified)))
IDs_doneMdl <- unique(tb_pth$mbid[tb_pth$lastSaved >= as.POSIXct(thisDwnl)])
###################
### assigning "done" to studies that have finished collecting data, and "live" for those still collecting data, i.e. they have more data since last download
IDs_done <- unique(sub("_.*", "", list.files(pthDownld)))
compareDF <- all
compareDF$status <- NA
compareDF$status[compareDF$id%in%IDs_done] <- "done"
compareDF$status[compareDF$status=="done" & compareDF$timestamp_last_deployed_location>as.POSIXct(lastDwld, tz="UTC")] <- "live"
table(compareDF$status)
table(is.na(compareDF$status))

######

Ids_toDo <- all$id ## first round
## or # subsequent rounds
Ids_toDo <- compareDF$id[!compareDF$status%in%c("done")]
## and ## interrupted in the middle
Ids_toDo <- Ids_toDo[!Ids_toDo%in%IDs_doneMdl]

start_time <- Sys.time()

# studyId <- Ids_toDo[1]
# studyId <-560810760

# newerToday <- tb_pth$filenm[tb_pth$lastSaved >= as.POSIXct(thisDwnl)] ## to exclude those downloaded today already
lastDownload <- tb_pth$filenm[tb_pth$lastSaved >= as.POSIXct(lastDwld)] ## to download missed last time

results <- lapply(Ids_toDo, function(studyId)try({
  ## create table with individuals per study, to be able to download per indivdual
  class(studyId) <- "integer64" ## lapply changes the class when looping though it
  print(studyId)
  reftb <- movebank_download_deployment(study_id=studyId, omit_derived_data=F )
  # vroom::problems(reftb)
  reftb <- reftb[reftb$number_of_events > units::set_units(0,"count"),]
  indiv <- reftb$individual_local_identifier
  if(any(grepl("/", indiv)==T)){indiv <- gsub("/","-",indiv)} ## some indiv names have "/" included which messes with R
  reftb$individual_local_identifierNObsl <- indiv
  reftb$pthName <- paste0(studyId,"__",reftb$individual_local_identifierNObsl,"__",reftb$deployment_id,".rds")
  doneIndv <- list.files(pthDownld)[grep(studyId,list.files(pthDownld))] ## indiv downloaded
  ## identify indv that are live 
  reftb$status <- NA
  reftb$status[reftb$pthName%in%doneIndv] <- "done"
  reftb$status[reftb$status=="done" & reftb$timestamp_end_individual>as.POSIXct(lastDwld, tz="UTC")] <- "live"
  liveAndmissingIndv <- reftb$pthName[!reftb$status%in%c("done")]
  # liveAndmissingIndv <- liveAndmissingIndv[!liveAndmissingIndv%in%newerToday] # to exclude those downloaded today already
  liveAndmissingIndv <- liveAndmissingIndv[!liveAndmissingIndv%in%lastDownload] # to download missed last download
  
  print(paste0("done:",length(doneIndv),"-todo:",length(liveAndmissingIndv)))
  
  missIndstrp <- sub("^\\d+_(.+)\\.rds$", "\\1", liveAndmissingIndv)
  ## ^\\d+_ matches the initial number and underscore
  ## (.+) captures everything after that until the file extension
  ## \\.rds$ matches the ".rds" at the end of the string
  ## The \\1 in the replacement refers to the captured group, effectively keeping only the middle part of the string.
  
  
  todoIndv <-  reftb$individual_id[reftb$individual_local_identifierNObsl%in%missIndstrp] ## using individual_id as sometimes individual_local_identifier gives error or does not exist

  ## download each individual separatly
  # ind <- todoIndv[2]
  results2 <- lapply(todoIndv, function(ind)try({
    class(ind) <- "integer64" ## lapply changes the class when looping though it
    print(paste0(studyId,"_",ind))
    mv2 <- movebank_download_study(studyId,
                                   sensor_type_id=c("gps","argos-doppler-shift","radio-transmitter"),
                                   individual_id=ind,
                                   timestamp_end=as.POSIXct(Sys.time(), tz="UTC")) # to avoid locations in the future

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
end_time-start_time 

# saveRDS(results,paste0(saveErrorPath,"all",".rds"))

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
## Check studies that returned errors:
giveError <- Ids_toDo[vapply(results, is.error, logical(1))]
tberr <- all[as.numeric(all$id)%in%giveError,]

