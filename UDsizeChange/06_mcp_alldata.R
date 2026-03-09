library(move2)
library(adehabitatHR)
library(sf)
library(raster)


genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pathFolder <- paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/")
flsMV <- list.files(pathFolder, full.names = F)
dir.create(paste0(genPath,"7.mcp95_alldata"))
pathmcp95 <- paste0(genPath,"7.mcp95_alldata/")
# dir.create(paste0(genPath,"7.mcp50"))
# pathmcp50 <- paste0(genPath,"7.mcp50/")

done <- list.files(pathmcp95, full.names = F)
flsMV <- flsMV[!flsMV%in%done]
# ind <- flsMV[1]

is.error <- function(x) inherits(x, "try-error")
start_time<- Sys.time()
results <- lapply(flsMV, function(ind)try({
  print(ind)
  vultr <- readRDS(paste0(pathFolder,ind))
  vultr_p <- sf::st_transform(vultr, "ESRI:54009") # using mollweide so all have same projection
  
  vultr_p$id <- mt_track_id(vultr_p)
  vultr_p_sp <- as_Spatial(vultr_p[,'id'])
 
  ## function mcp is very particular about the input object, it must only contain 1 column, and names of the individuals names have to follow the validNames() rules
  vultr_p_sp <- vultr_p_sp[,(names(vultr_p_sp) %in% "id")] 
  levels(vultr_p_sp$id) <- validNames(levels(vultr_p_sp$id))
  
  mcp95 <- mcp(vultr_p_sp, percent=95,unin = "m",unout="km2")
  # mcp50 <- mcp(vultr_p_sp, percent=50)
  
  mcp95sf <- st_as_sf(mcp95,crs="ESRI:54009")
  # mcp50sf <- st_as_sf(mcp50,crs="ESRI:54009")
  
  saveRDS(mcp95sf, file=paste0(pathmcp95,ind))
  # saveRDS(mcp50sf, file=paste0(pathmcp50,ind))
  
}))
end_time <- Sys.time() 
end_time-start_time # 20min

table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]


####################################
###### maps per species ##########
####################################
library(move2)
library(adehabitatHR)
library(mapdata)
library(scales)
library(sf)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggplot2)

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
referenceTableStudies <- readRDS(paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
pathmcp95 <- paste0(genPath,"7.mcp95_alldata/")
# pathmcp50 <- paste0(genPath,"7.mcp50/")
dir.create(paste0(genPath,"8.mcp95plots_alldata"))
pathmcp95plot <- paste0(genPath,"8.mcp95plots_alldata/")

flsMV <- list.files(pathmcp95, full.names = F)

sps <- as.character(unique(referenceTableStudies$species))

x <- sps[2]
lapply(sps, function(x){
  print(x)
  indl <- referenceTableStudies$fileName[referenceTableStudies$species==x]
  indl_p <- indl[indl%in%flsMV]
  
  imcpL <- lapply(indl_p,function(i){
  readRDS(paste0(pathmcp95,i))
})
  
  mcpSps <- dplyr::bind_rows(imcpL)
  
  worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
  
  ggplot() + theme_void() +
    geom_sf(data = worldMap) +
    geom_sf(data = mcpSps, fill=alpha("red",0.5), color="firebrick")+
    ggtitle(paste0(x," (",length(imcpL), " individuals)"))
  
  ggsave(paste0(pathmcp95plot, x,".jpg"))
})


## zoomed in
x <- sps[1]
lapply(sps, function(x){
  print(x)
  indl <- referenceTableStudies$fileName[referenceTableStudies$species==x]
  indl_p <- indl[indl%in%flsMV]
  
  imcpL <- lapply(indl_p,function(i){
    readRDS(paste0(pathmcp95,i))
  })
  
  mcpSps <- dplyr::bind_rows(imcpL)
  bbml <- st_bbox(mcpSps)
  bb <- st_transform(bbml,4326)
  exp <- 2
  
  worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
  
  ggplot() + theme_void() +
    geom_sf(data = worldMap) +
    geom_sf(data = mcpSps, fill=alpha("red",0.5), color="firebrick")+
    ggtitle(paste0(x," (",length(imcpL), " individuals)"))+
    coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)
  
  ggsave(paste0(pathmcp95plot, x,"_zoomIn.jpg"))
})
