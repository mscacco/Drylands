library(move2)
library(units)
library(bit64)
library(lubridate)
library(tools)
library(sf)
library(terra)
library(plyr)
library(dplyr)


library(doParallel)
# library(plyr)
mycores <- detectCores()-1 
registerDoParallel(mycores) 

genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
dir.create(paste0(genPath,"WEB_vultureIndv_mv2"))
pthDownldIndv <- paste0(genPath,"WEB_vultureIndv_mv2/")
dir.create(paste0(genPath,"WEB_vultureIndv_raster_01_moll"))
pthraster01 <- paste0(genPath,"WEB_vultureIndv_raster_01_moll/")
dir.create(paste0(genPath,"WEB_vultureIndv_raster_nbLocs_moll"))
pthrasterNbLocs <- paste0(genPath,"WEB_vultureIndv_raster_nbLocs_moll/")
dir.create(paste0(genPath,"WEB_vultureIndv_final_rasters_moll"))
pthraster4plots <- paste0(genPath,"WEB_vultureIndv_final_rasters_moll/")

# dir.create("licenseTerms")
# dir.create("rastersForMap")





# # list accounts
# keyring::key_list()
# specify account to use in the R session
options("move2_movebank_key_name" = "Drylands")
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
all$timestamp_last_deployed_location
sort(all$number_of_deployed_locations)

# some summary info on the available data (updated Nov 8th)

range(c(year(all$timestamp_first_deployed_location), #from 2006
        year(all$timestamp_last_deployed_location)))
sum(all$number_of_individuals) #1892 individuals
length(unique(all$contact_person_name)) #33 data owners
length(unique(unlist(strsplit(all$taxon_ids, ",")))) #16 species
sum(all$number_of_deployed_locations) #269668097 locations

#________________
# Download data, by individual.
# check which data has been downloaded and changed in size since last download

# # Extract licences for data download (only needed once)
# licencesErrors <- lapply(1:length(all$id), function(i)try({
#   movebank_download_study(all$id[i],
#                                 sensor_type_id="gps",
#                                 attributes = c("study_id", "individual_id",
#                                                "individual_local_identifier",
#                                                "individual_taxon_canonical_name"))
# }))
# licences <- sapply(lapply(licencesErrors, "[", 1), function(err){
#   lic <- gsub("'. Alternatively.*","",gsub(".*'license-md5'='", "", err))
#   #lic <- gsub(". Alternatively.*","",gsub(".*download request: ", "", err))
#   return(lic)
# })


## check which studies I already have downloaded and if number of indiv. are the same as the available on MB, only applys after 1st round. For 1st round directly run ~L28
# IDs_done <- sub("_[^_]+$", "", list.files(pthDownldIndv))
# indvPerStud <- data.frame(table(IDs_done))
# colnames(indvPerStud)[1] <- "id"
# indvPerStud$id <- bit64::as.integer64(as.character(indvPerStud$id))
# allIndvStudy <- all[,c("id", "number_of_individuals")]
# compareDF <- merge(allIndvStudy,indvPerStud, by= "id", all=T)
# compareDF$Freq <- units::set_units(compareDF$Freq, "count")
# compareDF <- compareDF[!compareDF$number_of_individuals-compareDF$Freq==units::set_units(0,"count") |is.na(compareDF$Freq),]
# compareDF <- compareDF[!is.na(compareDF$id),]

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
  reftb <- reftb[reftb$number_of_events > units::set_units(0,"count"),]
  allindv <- unique(reftb$individual_id) ## using indv_id as sometimes indv_loc_idenitf gives error or does not exist
  # doneIndv <- list.files(pthDownldIndv)[grep(studyId,list.files(pthDownldIndv))]
  # allI <- paste0(studyId,"_",allindv,".rds")
  # todoIndv <-  
  
  ## download each individual separatly
  # ind <- allindv[2]
  results2 <- lapply(allindv, function(ind)try({
    class(ind) <- "integer64" ## lapply changes the class when looping though it
    print(ind)
    #system(paste0('curl -v -u ', paste(usr, psw, sep=":"), ' -c ./cookies.txt -o ./licenseTerms/license_terms.txt "https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=', studyId, '"'))
    mv2 <- movebank_download_study(studyId,
                                   sensor_type_id=c("gps","argos-doppler-shift"),
                                   # individual_local_identifier= ind,
                                   individual_id=ind,
                                   timestamp_end=as.POSIXct(Sys.time(), tz="UTC"), # to avoid locations in the future
                                   #'license-md5'=md5sum("./licenseTerms/license_terms.txt"), #(preferred) to use together with the system call at line 71
                                   #'license-md5'=licences[3], #to use together with licence list downloaded as "errors" (see above, line 47)
                                   #'license-md5'='85ba9b4a5037a598f66e911053375585', #to use alone for one single study/licence
                                   attributes = c("study_id","individual_id",
                                                  "individual_taxon_canonical_name")
    )
    
    if("individual_local_identifier" %in% names(mt_track_data(mv2))){
      indiv <- mt_track_data(mv2)$individual_local_identifier
      if(grepl("/", indiv)==T){indiv <- gsub("/","-",indiv)}
      print(indiv)
    }else{
      indiv <- mt_track_data(mv2)$individual_id
      print(indiv)
    }
    saveRDS(mv2, file=paste0(pthDownldIndv,studyId,"_",indiv,".rds"))
  }))
}))
end_time <- Sys.time()
end_time-start_time # ~ 3.5h

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
# Check studies that returned errors:
giveError <- Ids_toDo[vapply(results, is.error, logical(1))]

tberr <- all[as.numeric(all$id)%in%giveError,]

#################################
##### rasterize data ###########
## nb sps, nb indiv, nb locs, nb studies

# rr <- rast(xmin=-180, xmax=180, ymin=-90, ymax=90, crs="EPSG:4326", resolution=1, vals=NA)
rr <- rast(paste0(genPath,"A1.rastersOfAllSpeciesRange/","Gyps_africanus.tiff"))
values(rr) <- NA
names(rr) <- "template"
terra::varnames(rr) <-  "template"

IDs_done <- gsub("__(.*?).tif","",list.files(pthraster01)) 
Ids_toDo <- gsub(".rds","",list.files(pthDownldIndv))
Ids_toDo <- Ids_toDo[!Ids_toDo%in%IDs_done]
# Indv <- Ids_toDo[1]
start_time <- Sys.time()
# results <- llply(Ids_toDo, function(Indv) try({
results <- lapply(Ids_toDo, function(Indv) try({
  terraMv <- readRDS(paste0(pthDownldIndv,Indv,".rds"))
  terraMv <- dplyr::filter(terraMv, !sf::st_is_empty(terraMv))
  # copy individual attribute to location attribute
  if("taxon_canonical_name"%in%names(mt_track_data(terraMv))){
  terraMv <- mt_as_event_attribute(terraMv, c("study_id","individual_id","taxon_canonical_name"))
  }else{
    terraMv <- mt_as_event_attribute(terraMv, c("study_id","individual_id","taxon_ids"))
    terraMv <- rename(terraMv, taxon_canonical_name = taxon_ids )
  }
  
  # Check completeness of species name, for some species only genus is preset. When this happens we complete it with the name of the species with matching genus
  taxons <- as.character(unique(terraMv$taxon_canonical_name))
  if(any(sapply(strsplit(taxons, " "), length)==1)){
    specComplete <- taxons[sapply(strsplit(taxons, " "), length)==2]
    specIncomplete <- taxons[sapply(strsplit(taxons, " "), length)==1]
    if(!is.na(specIncomplete)){
      newSpec <- grep(specIncomplete, specComplete, value=T)
      terraMv$taxon_canonical_name[terraMv$taxon_canonical_name==specIncomplete] <- newSpec
      terraMv$taxon_canonical_name <- as.factor(as.character(terraMv$taxon_canonical_name))
    }}
  
  terraMv_t <- st_transform(terraMv,crs(rr))
  terraVect_sp <- terraMv_t
  mt_track_id(terraVect_sp) <- NULL
  terraVect_sp <- vect(terraVect_sp)
  ## this raster per individual can be used to calculate nb of indiv, nb of studies and nb of taxons
  rast01 <- rasterize(terraVect_sp, rr, field="taxon_canonical_name", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
  
  names(rast01) <- gsub(" ","_",taxons)
  # save species raster with sps name separated by double underscore to make it easier to find with gsub/grep, etc
  writeRaster(rast01, paste0(pthraster01,Indv,"__",gsub(" ","_",taxons),".tif"), overwrite=T)
  
  
  rastNb <- rasterize(terraVect_sp, rr, fun="length")
  writeRaster(rastNb, paste0(pthrasterNbLocs,Indv,".tif"), overwrite=T)
})
)
# ,.parallel = T)
end_time <- Sys.time()
end_time-start_time # ~25min+20min

##################### 
# Import, stack and sum raster layers of number of locations, individuals and studies
######################
nLocs_sum <- sum(rast(lapply(list.files(pthrasterNbLocs, full.names = T), rast)), na.rm=T)
writeRaster(nLocs_sum, paste0(pthraster4plots,"nLocs_sum.tif"), overwrite=T)
plot(nLocs_sum, col=rainbow(100, start=0.1))
nInds_sum <- sum(rast(lapply(list.files(pthraster01, full.names = T), rast)), na.rm=T)
writeRaster(nInds_sum, paste0(pthraster4plots,"nInds_sum.tif"), overwrite=T)
plot(nInds_sum, col=rainbow(100, start=0.1))

IDs_study <- unique(gsub("_(.*?).tif","",list.files(pthraster01)))
sID <- IDs_study[1]
nMBstud_sum <- sum(rast(
  lapply(IDs_study, function(sID){
  studyIDr <- sum(rast(lapply(list.files(pthraster01, pattern=sID, full.names = T), rast)), na.rm=T)
  studyIDr[studyIDr>1] <- 1
  return(studyIDr)
})
), na.rm=T)
plot(nMBstud_sum, col=rainbow(100, start=0.1))
writeRaster(nMBstud_sum, paste0(pthraster4plots,"nMBstud_sum.tif"), overwrite=T)

IDs_taxa <- unique(gsub(".tif","",gsub(".*__","",list.files(pthraster01))))
IDs_taxa <- IDs_taxa[!IDs_taxa%in% "Trigonoceps_occipitalis,Gyps_africanus,Necrosyrtes_monachus,Torgos_tracheliotus"]
# tID <- IDs_taxa[16]
nTaxa_sum <- sum(rast(
  lapply(IDs_taxa, function(tID){
    taxar <- sum(rast(lapply(list.files(pthraster01, pattern=tID, full.names = T), rast)), na.rm=T)
    taxar[taxar>1] <- 1
    return(taxar)
  })
), na.rm=T)
plot(nTaxa_sum, col=rainbow(100, start=0.1))
nTaxa_sum[nTaxa_sum==10] <- NA ## 10 is at the lat/long 0,0 => we should really clean the data a bit...
writeRaster(nTaxa_sum, paste0(pthraster4plots,"nTaxa_sum.tif"), overwrite=T)

# # Plot and save the maps
dir.create(paste0(genPath,"WEB_vultureIndv_land_ocean"))
pthlandocean <- paste0(genPath,"WEB_vultureIndv_land_ocean/")
dir.create(paste0(genPath,"WEB_vultureIndv_final_plots"))
pth4plots <- paste0(genPath,"WEB_vultureIndv_final_plots/")

library(rnaturalearth)
library(terra)
library(ggplot2)
library(tidyterra)
land <- ne_download(scale=110, type="land", category="physical", destdir=pthlandocean,returnclass="sf")
ocean <- ne_download(scale=110, type="ocean", category="physical", destdir=pthlandocean,returnclass="sf")

# ocean <- ne_load(scale=110, type="ocean", category="physical", destdir=pthlandocean,returnclass="sf")
# land <- ne_load(scale=110, type="land", category="physical", destdir=pthlandocean,returnclass="sf")
ggplot()+geom_sf(data=ocean, fill="paleturquoise1")+geom_sf(data=land, fill="bisque")+coord_sf(crs = crs(spsRast)) 

nLocs_sum <- rast(paste0(pthraster4plots,"nLocs_sum.tif"))
nInds_sum <- rast(paste0(pthraster4plots,"nInds_sum.tif"))
nMBstud_sum <- rast(paste0(pthraster4plots,"nMBstud_sum.tif"))
nTaxa_sum <- rast(paste0(pthraster4plots,"nTaxa_sum.tif"))

#N. of species, N. of GPS locations, N. of individuals, N. of Movebank studies
ggplot() +
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan")+ #paleturquoise1
  geom_spatraster(data = nLocs_sum, aes(fill = sum), show.legend = F) +
  coord_sf(crs = crs(nLocs_sum)) +
  scale_fill_whitebox_c("N. of GPS locations",palette="viridi", direction=1)+
  theme_bw()
# ggsave(paste0(pth4plots,"nLocs_sum",".png"),width = 2800,height = 2100, units="px")
ggsave(paste0(pth4plots,"nLocs_sum_noledgend",".png"),width = 2800,height = 2100, units="px")

ggplot() +
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan")+ #paleturquoise1
  geom_spatraster(data = nInds_sum, aes(fill = sum), show.legend = F) +
  coord_sf(crs = crs(nInds_sum)) +
  scale_fill_whitebox_c("N. of individuals",palette="viridi", direction=1)+
  theme_bw()
# ggsave(paste0(pth4plots,"nInds_sum",".png"),width = 2800,height = 2100, units="px")
ggsave(paste0(pth4plots,"nInds_sum_noledgend",".png"),width = 2800,height = 2100, units="px")

ggplot() +
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan")+ #paleturquoise1
  geom_spatraster(data = nMBstud_sum, aes(fill = sum), show.legend = F) +
  coord_sf(crs = crs(nMBstud_sum)) +
  scale_fill_whitebox_c("N. of Movebank studies",palette="viridi", direction=1)+
  theme_bw()
# ggsave(paste0(pth4plots,"nMBstud_sum",".png"),width = 2800,height = 2100, units="px")
ggsave(paste0(pth4plots,"nMBstud_sum_noledgend",".png"),width = 2800,height = 2100, units="px")

ggplot() +
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan")+ #paleturquoise1
  geom_spatraster(data = nTaxa_sum, aes(fill = sum), show.legend = F) +
  coord_sf(crs = crs(nTaxa_sum)) +
  scale_fill_whitebox_c("N. of species",palette="viridi", direction=1)+
  theme_bw()
# ggsave(paste0(pth4plots,"nTaxa_sum",".png"),width = 2800,height = 2100, units="px")
ggsave(paste0(pth4plots,"nTaxa_sum_noledgend",".png"),width = 2800,height = 2100, units="px")



# 
# library(viridis)
# library(leaflegend)
# #remotes::install_github("rstudio/leaflet")
# library(leaflet)
# 
# # Choose one at a time the raster that we want to display and the corresponding legend
# # (choose among: nTaxa_sum nLocs_sum nInds_sum nMBstud_sum)
# SPr <- nLocs_sum
# # (choose among: N. of species, N. of GPS locations, N. of individuals, N. of Movebank studies)
# legendTitle <- "N. of species"
# 
# ## THIS REPROJECTION DOES NOT SEEM TO WORK PROPERLY!!!
# #bounds <- as.numeric(as.vector(ext(SPr)))
# SPr_l <- projectRasterForLeaflet(SPr, method = "ngb") #EPSG:3857
# SPr_l <- terra::project(SPr,y="EPSG:3857")
# 
# maxBin <- 10
# if(max(values(SPr_l), na.rm=T) <= maxBin){
#   myBins <- length(1:max(values(SPr_l), na.rm=T))
# }else{myBins <- maxBin}
# 
# if(myBins == maxBin){
#   # rPal <- colorBin(brewCol, c(1, max(values(SPr_l), na.rm=T)), 
#   #                  reverse = F,
#   #                  na.color = "transparent", bins=myBins)
#   rPal <- colorNumeric("viridis", domain=c(1,max(values(SPr_l), na.rm=T)), 
#                        reverse = F,
#                        na.color = "transparent")
# }else{
#   rPal <- colorFactor("viridis", domain=as.factor(1:max(values(SPr_l), na.rm=T)), 
#                       reverse = F, 
#                       na.color = "transparent")
# }
# 
# outl <- leaflet() %>% 
#   #setMaxBounds(lng1 = bounds[1], lng2 = bounds[2], lat1 =  bounds[3], lat2 = bounds[4]) %>% 
#   setView(lng = 0, lat = 0, zoom = 3) %>% 
#   addTiles() %>%
#   addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
#   addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
#   addRasterImage(SPr_l, colors = rPal, opacity = 0.7, project = FALSE, group = "raster") %>%
#   addScaleBar(position="bottomright",
#               options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
#   addLayersControl(
#     baseGroups = c("TopoMap","Aerial"),
#     overlayGroups = "raster",
#     options = layersControlOptions(collapsed = FALSE))
# 
# #max(values(SPr), na.rm=T)>10000 == SPr@ptr$range_max
# if(SPr@cpp$range_max > maxBin){ #a way to say if the raster we are looking at measures number of locations (rather than n. indiv, n. species, n. studies)
#   outl <- outl %>%
#     addLegend(position="bottomleft", opacity = 0.6, bins = ifelse(SPr@cpp$range_max > 100, 10, 5), 
#               pal = rPal, values = 1:max(values(SPr_l), na.rm=T), title = legendTitle)
# }else{
#   outl <- outl %>%
#     addLegend(position="bottomleft", opacity = 0.6, 
#               pal = rPal, values = as.factor(1:max(values(SPr_l), na.rm=T)), title = legendTitle)
# }
# 
# outl   
# 
# # save output as html or image 
# library(mapview)
# 
# dir.create("MapResults")
# 
# # mapshot(outl, url = "MapResults/Map_nSpecies_newLeg.html") # Map_nSpecies, Map_nLocations, Map_nIndividuals, Map_nMBstudies
# # mapshot(outl, file = "MapResults/Map_nSpecies_newLeg.jpeg") #zoom has to be 2
# # Adjust size and resolution for the website default 992*744, minimum for website 1440*1080, recommended 2800*2100
# mapshot(outl, file = "MapResults/Map_nSpecies_newLeg_sizeWebsite_11July.jpeg", vwidth = 1440, vheight = 1080,
#         delay=0.5) # zoom has to be 3
# 
# # library(webshot)
# # webshot(url, file = "MapResults/Map_nSpecies_newLeg_sizeWebsite.jpeg", vwidth = 2800, vheight = 2100) 
# 
