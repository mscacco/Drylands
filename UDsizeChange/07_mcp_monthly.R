library(move2)
library(adehabitatHR)
library(sf)
library(raster)
library('lubridate')
library("units")

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pathFolder <- paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/")
flsMV <- list.files(pathFolder, full.names = F)
dir.create(paste0(genPath,"9.mcp95_monthly"))
pathmcp95 <- paste0(genPath,"9.mcp95_monthly/")

done <- list.files(pathmcp95, full.names = F)
flsMV <- flsMV[!flsMV%in%done]
# ind <- flsMV[1]
# referenceTableStudies <- readRDS(paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
# as.character(unique(referenceTableStudies$species))
# x <- "Gyps fulvus"
# indl <- referenceTableStudies$fileName[referenceTableStudies$species==x]
# flsMVsel <- indl[indl%in%flsMV]


flsMVsel <- flsMV
is.error <- function(x) inherits(x, "try-error")
start_time<- Sys.time()
results <- lapply(flsMVsel, function(ind)try({
  print(ind)
  vultr <- readRDS(paste0(pathFolder,ind))
  
  vultr$monthYear <- paste0(year(mt_time(vultr)),"-",sprintf("%02d",month(mt_time(vultr))))
  vultrL <- split(vultr, vultr$monthYear) 
  mnthmcpL <-  lapply(1:length(vultrL), function(y)tryCatch({
    indM <- vultrL[[y]]
    yrm <- names(vultrL[y])
    
    vultr_p <- sf::st_transform(indM, "ESRI:54009") # using mollweide so all have same projection
    
    vultr_p$id <- paste0(mt_track_id(vultr_p),"__",yrm)
    vultr_p_sp <- as_Spatial(vultr_p[,'id'])
    ## function mcp is very particular about the input object, it must only contain 1 column, and names of the individuals names have to follow the validNames() rules
    vultr_p_sp <- vultr_p_sp[,(names(vultr_p_sp) %in% "id")] 
    levels(vultr_p_sp$id) <- validNames(levels(vultr_p_sp$id))
    mcp95 <- mcp(vultr_p_sp, percent=95,unin = "m",unout="km2")
    mcp95$yrm <- yrm
    mcp95sf <- st_as_sf(mcp95,crs="ESRI:54009")
  }, error = function(e) {
    return(NULL)
  }))
  mcpInd <- dplyr::bind_rows(mnthmcpL)
  
  saveRDS(mcpInd, file=paste0(pathmcp95,ind))
  # saveRDS(mcp50sf, file=paste0(pathmcp50,ind))
}))
end_time <- Sys.time() 
end_time-start_time # 1.2h

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
pathmcp95 <- paste0(genPath,"9.mcp95_monthly/")
dir.create(paste0(genPath,"10.mcp95plots_monthly"))
pathmcp95plot <- paste0(genPath,"10.mcp95plots_monthly/")
flsMV <- list.files(pathmcp95, full.names = F)

migratory_month <- readRDS(paste0(genPath,"migratory_month.rds"))
sps <- as.character(unique(migratory_month$species))

# x <- sps[2]
# # lapply(sps, function(x){
#   print(x)
#   indl <- referenceTableStudies$fileName[referenceTableStudies$species==x]
#   indl_p <- indl[indl%in%flsMV]
# 
#   # indl_p <- flsMV
#   imcpL <- lapply(indl_p,function(i){
#   readRDS(paste0(pathmcp95,i))
# })
#   
#   mcpSps <- dplyr::bind_rows(imcpL)
#   bbml <- st_bbox(mcpSps)
#   bb <- st_transform(bbml,4326)
#   exp <- 2
#   worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
#   
#   ggplot() + theme_void() +
#     geom_sf(data = worldMap) +
#     geom_sf(data = mcpSps, fill=alpha("red",0.5), color="firebrick")+
#     ggtitle(paste0(x," (",length(imcpL), " individuals)"))+
#   coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)
#  
#   
#   
#   i <- indl_p[550]
#   mcpSpsIn <- readRDS(paste0(pathmcp95,i))
#   bbml <- st_bbox(mcpSpsIn)
#   bb <- st_transform(bbml,4326)
#   exp <- 2
#   worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
#   
#   ggplot() + theme_void() +
#     geom_sf(data = worldMap) +
#     geom_sf(data = mcpSpsIn, fill=alpha("red",0.5), color="firebrick")+
#     # facet_wrap(~id)+
#     coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)
#   
#   ggplot() + theme_void() +
#     # geom_sf(data = worldMap) +
#     geom_sf(data = mcpSpsIn, fill=alpha("red",0.5), color="firebrick")+
#     facet_wrap(~id)
#    
# 
# #   ggsave(paste0(pathmcp95plot, x,".jpg"))
# # })


## zoomed in
# x <- sps[3]
# sps <- c("Aegypius monachus","Cathartes aura","Gyps fulvus","Neophron percnopterus")
lapply(sps, function(x){
  print(x)
  indl <- referenceTableStudies$fileName[referenceTableStudies$species==x]
  indl_p <- indl[indl%in%flsMV]
  
  # i <- indl_p[1]
  imcpL <- lapply(indl_p,function(i){
   mcp95 <-  readRDS(paste0(pathmcp95,i))
   if(nrow(mcp95)==0){return(NULL)}else{
    sub <- migratory_month[migratory_month$fileName==i,]
    # table(sub$migratoryMonth)
    # mcp95_sub <- mcp95[mcp95$yrm %in% sub$monthYear[sub$migratoryMonth==T], ] ## keep migratory month
    mcp95_sub <- mcp95[!mcp95$yrm %in% sub$monthYear[sub$migratoryMonth==T], ] ## remove migratory month
    if(nrow(mcp95_sub)==0){return(NULL)} else{
      return(mcp95_sub)
    }}
  })
  mcpSps <- dplyr::bind_rows(imcpL)
  # unlist(lapply(imcpL,nrow))
  
  bbml <- st_bbox(mcpSps)
  bb <- st_transform(bbml,4326)
  exp <- 2
  
  worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
  
  ggplot() + theme_void() +
    geom_sf(data = worldMap, fill="grey90", color="grey85") +
    geom_sf(data = mcpSps, fill=alpha("red",0.5), color="firebrick")+
    ggtitle(paste0(x," (",length(imcpL), " individuals)"))+
    coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)
  
  # ggsave(paste0(pathmcp95plot, x,"_monthly_Migration_zoomIn.jpg"))
  ggsave(paste0(pathmcp95plot, x,"_monthly_noMigration_zoomIn.jpg"))
})
