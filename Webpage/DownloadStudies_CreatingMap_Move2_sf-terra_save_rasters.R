# install from unzipped folder downloaded from gitlab on 2.12.2022
#devtools::install("/home/mscacco/ownCloud/Martina/ProgettiVari/Move2/move2-main") 
# or install from gitlab
# require('devtools')
# devtools::install_git('https://gitlab.com/bartk/move2.git')

#setwd("/home/mscacco/ownCloud/DryLands/scripts")
dir.create("licenseTerms")

library(move2)
library(units)

# # remove old keyring accounts:
# movebank_remove_credentials() #or:
# keyring::default_backend()$keyring_delete()
# # store credentials for new movebank account
# movebank_store_credentials("Drylands", "*****", key_name = "Drylands")
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

# some summary info on the available data (updated July 13th)
library(lubridate)
range(c(year(all$timestamp_first_deployed_location), #from 2006
        year(all$timestamp_last_deployed_location)))
sum(all$number_of_individuals) #1768 individuals
length(unique(all$contact_person_name)) #32 data owners
unique(unlist(strsplit(all$taxon_ids, ","))) #16 species
sum(all$number_of_deployed_locations) #236'510'519 locations

#________________
# Download and rasterize data per study

library(tools)
library(sf)
library(terra)

#studyArea <- st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs=st_crs("EPSG:4326"))
#grd <- st_as_stars(studyArea, dx = 0.1, dy = 0.1, values = 0)
rr <- rast(xmin=-180, xmax=180, ymin=-90, ymax=90, crs="EPSG:4326",
           resolution=1, vals=NA)

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

#setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/Drylands/DryLands/scripts")
dir.create("rastersForMap")

IDs_done <- gsub("[A-z.]", "", list.files("rastersForMap", pattern="nLocs"))
Ids_toDo <- as.numeric(all$id[! as.character(all$id) %in% IDs_done])
# studies_newSpec <- c(54115008) #studies for which the species name were updated/completed
# Ids_toDo <- c(Ids_toDo, studies_newSpec)

usr <- "Drylands"
psw <- "******"

results <- lapply(Ids_toDo, function(studyId) try({ 
    #system(paste0('curl -v -u ', paste(usr, psw, sep=":"), ' -c ./cookies.txt -o ./licenseTerms/license_terms.txt "https://www.movebank.org/movebank/service/direct-read?entity_type=event&study_id=', studyId, '"'))
    terraMv <- movebank_download_study(studyId,
                                  sensor_type_id=c("gps","argos-doppler-shift"),
                                  #'license-md5'=md5sum("./licenseTerms/license_terms.txt"), #(preferred) to use together with the system call at line 71
                                  #'license-md5'=licences[3], #to use together with licence list downloaded as "errors" (see above, line 47)
                                  'license-md5'='85ba9b4a5037a598f66e911053375585', #to use alone for one single study/licence
                                  attributes = c("study_id","individual_id",
                                                 "individual_taxon_canonical_name"))
    # copy individual attribute to location attribute
    terraMv <- mt_as_event_attribute(terraMv, c("study_id","individual_id","taxon_canonical_name"))
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
    # split move object per species
    mv_ls <- split(terraMv, as.character(terraMv$taxon_canonical_name))
    # rasterize n.species (calculate one raster per species)
    tmp1_st <- lapply(mv_ls, function(sp){
      terraVect_sp <- sp
      mt_track_id(terraVect_sp) <- NULL
      terraVect_sp <- vect(terraVect_sp[!st_is_empty(terraVect_sp),])
      tmp1 <- rasterize(terraVect_sp, rr, field="taxon_canonical_name", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
      return(tmp1)
    })
    spNames <- names(tmp1_st)
    tmp1_st <- rast(tmp1_st) #create a raster stack
    names(tmp1_st) <- spNames # names can be extracted from tmp1_st@ptr$names
    # save species raster and remove unnecessary objects
    writeRaster(tmp1_st, paste0("rastersForMap/nSpec_",studyId,".tif"), overwrite=T)
    rm(mv_ls, tmp1_st)
  
    #remove NAs in geometry (coords) and create a SpatVect object from the move object
    mt_track_id(terraMv) <- NULL
    terraMv <- vect(terraMv[!st_is_empty(terraMv),])
    
    # rasterize n.locs using terra package
    tmp2 <- terra::rasterize(terraMv, rr, field=1:nrow(terraMv), fun="length")
    # rasterize n.individuals
    tmp3 <- rasterize(terraMv, rr, field="individual_id", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
    # rasterize n. MB studies
    tmp4 <- rasterize(terraMv, rr, field="study_id", fun=function(x, ...){length(unique(na.omit(x)))}, update=TRUE)
    # remove unnecessary object
    rm(terraMv)
    
    # Save raster results locally
    writeRaster(tmp2, paste0("rastersForMap/nLocs_",studyId,".tif"), overwrite=T)
    writeRaster(tmp3, paste0("rastersForMap/nInds_",studyId,".tif"), overwrite=T)
    writeRaster(tmp4, paste0("rastersForMap/nMBstud_",studyId,".tif"), overwrite=T)
  })
)

results
is.error <- function(x) inherits(x, "try-error")
vapply(results, is.error, logical(1))


#________________
# Import, stack and sum raster layers of number of locations, individuals and studies

library(terra)

#setwd("/home/mscacco/ownCloud/Martina/ProgettiVari/Drylands/DryLands/scripts")
nLocs_sum <- sum(rast(lapply(list.files("rastersForMap", pattern = "nLocs", full.names = T), rast)), na.rm=T)
nInds_sum <- sum(rast(lapply(list.files("rastersForMap", pattern = "nInds", full.names = T), rast)), na.rm=T)
nMBstud_sum <- sum(rast(lapply(list.files("rastersForMap", pattern = "nMBstud", full.names = T), rast)), na.rm=T)

# Import species raster layers and stack all in one big raster stack
nSpec_stack <- rast(lapply(list.files("rastersForMap", pattern = "nSpec", full.names = T), rast))
# then split it by species, to sum the number of species per pixel
names(nSpec_stack)[names(nSpec_stack)=="Neophron"] <- "Neophron percnopterus"
allSpec_ls <- split(nSpec_stack, nSpec_stack@ptr$names)
# average within each species to merge all studies belonging to the same species
nSpec_stack_all <- rast(lapply(allSpec_ls, function(st){
  stAvg <- mean(st, na.rm=T) # all rasters contain either 1 or NA, so we can use mean to merge them
  names(stAvg) <- unique(st@ptr$names)
  return(stAvg)
}))
# list available species
names(nSpec_stack_all)
# Finally sum across species to count how many DIFFERENT species we have per pixel
nSpec_sum <- sum(nSpec_stack_all, na.rm=T)

#________________
# Plot and save the maps

library(viridis)
library(leaflegend)
#remotes::install_github("rstudio/leaflet")
library(leaflet)

# Choose one at a time the raster that we want to display and the corresponding legend
# (choose among: nSpec_sum nLocs_sum nInds_sum nMBstud_sum)
SPr <- nSpec_sum
# (choose among: N. of species, N. of GPS locations, N. of individuals, N. of Movebank studies)
legendTitle <- "N. of species"

#bounds <- as.numeric(as.vector(ext(SPr)))
SPr_l <- projectRasterForLeaflet(SPr, method = "ngb")

maxBin <- 10
if(max(values(SPr_l), na.rm=T) <= maxBin){
  myBins <- length(1:max(values(SPr_l), na.rm=T))
}else{myBins <- maxBin}

if(myBins == maxBin){
  # rPal <- colorBin(brewCol, c(1, max(values(SPr_l), na.rm=T)), 
  #                  reverse = F,
  #                  na.color = "transparent", bins=myBins)
  rPal <- colorNumeric("viridis", domain=c(1,max(values(SPr_l), na.rm=T)), 
                       reverse = F,
                       na.color = "transparent")
}else{
  rPal <- colorFactor("viridis", domain=as.factor(1:max(values(SPr_l), na.rm=T)), 
                      reverse = F, 
                      na.color = "transparent")
}

outl <- leaflet() %>% 
  #setMaxBounds(lng1 = bounds[1], lng2 = bounds[2], lat1 =  bounds[3], lat2 = bounds[4]) %>% 
  setView(lng = 0, lat = 0, zoom = 3) %>% 
  addTiles() %>%
  addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
  addRasterImage(SPr_l, colors = rPal, opacity = 0.7, project = FALSE, group = "raster") %>%
  addScaleBar(position="bottomright",
              options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
  addLayersControl(
    baseGroups = c("TopoMap","Aerial"),
    overlayGroups = "raster",
    options = layersControlOptions(collapsed = FALSE))

#max(values(SPr), na.rm=T)>10000 == SPr@ptr$range_max
if(SPr@ptr$range_max > maxBin){ #a way to say if the raster we are looking at measures number of locations (rather than n. indiv, n. species, n. studies)
  outl <- outl %>%
    addLegend(position="bottomleft", opacity = 0.6, bins = ifelse(SPr@ptr$range_max > 100, 10, 5), 
              pal = rPal, values = 1:max(values(SPr_l), na.rm=T), title = legendTitle)
}else{
  outl <- outl %>%
    addLegend(position="bottomleft", opacity = 0.6, 
              pal = rPal, values = as.factor(1:max(values(SPr_l), na.rm=T)), title = legendTitle)
}

outl   

# save output as html or image 
library(mapview)

dir.create("MapResults")

# mapshot(outl, url = "MapResults/Map_nSpecies_newLeg.html") # Map_nSpecies, Map_nLocations, Map_nIndividuals, Map_nMBstudies
# mapshot(outl, file = "MapResults/Map_nSpecies_newLeg.jpeg") #zoom has to be 2
# Adjust size and resolution for the website default 992*744, minimum for website 1440*1080, recommended 2800*2100
mapshot(outl, file = "MapResults/Map_nSpecies_newLeg_sizeWebsite_11July.jpeg", vwidth = 1440, vheight = 1080,
        delay=0.5) # zoom has to be 3

# library(webshot)
# webshot(url, file = "MapResults/Map_nSpecies_newLeg_sizeWebsite.jpeg", vwidth = 2800, vheight = 2100) 

