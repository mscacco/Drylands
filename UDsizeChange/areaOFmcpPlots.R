library(move2)
library(adehabitatHR)
library(mapdata)
library(scales)
library(sf)
library(rnaturalearth) #needs rnaturalearthhires also installed
library(ggplot2)
library(dplyr)

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
referenceTableStudies <- readRDS(paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
# pathmcp95 <- paste0(genPath,"6.mcp95/")
pathmcp95 <- paste0(genPath,"6.mcp95_monthly/")

flsMV <- list.files(pathmcp95, full.names = F)
sps <- as.character(unique(referenceTableStudies$species))
# sps
# sps
# [1] "Gypaetus barbatus"       "Torgos tracheliotus"     "Neophron percnopterus"   "Gyps himalayensis"       "Gyps africanus"    
# [6] "Trigonoceps occipitalis" "Necrosyrtes monachus"    "Gyps rueppellii"         "Aegypius monachus"       "Gyps fulvus"         
# [11] "Vultur gryphus"          "Cathartes aura"          "Coragyps atratus"        "Cathartes burrovianus"   "Sarcoramphus papa"  
# [16] "Gyps coprotheres"        "Gyps bengalensis" 
# x <- sps[17] # 3,9,11,12
# indl <- referenceTableStudies$fileName[referenceTableStudies$species==x]
# indl_p <- indl[indl%in%flsMV]
indl_p <- flsMV
i <- "327161259__Deborah-0HA__1283254784.rds"
mcpMeasuresL <- lapply(indl_p,function(i){
  print(i)
  mcp95 <- readRDS(paste0(pathmcp95,i))
  mcp95L <- split(mcp95, mcp95$id)
  df_l <- lapply(mcp95L, function(x){
    mcparea <- round(x$area)
    coords <- st_coordinates(x)
    width <- min(dist(coords))
    length <- max(dist(coords))
    max_width <- round(max(width, length))
    data.frame(fileName=i,id=x$id,mcpareakm2=mcparea, mcpmaxWidthKM=max_width/1000, sps=referenceTableStudies$species[referenceTableStudies$fileName==i])
  })
  do.call("rbind",df_l)
})

mcpMeasuresAll <- do.call("rbind",mcpMeasuresL)

per <- aggregate(mcpMeasuresAll$mcpmaxWidthKM, by=list(mcpMeasuresAll$sps), FUN=function(x) quantile(x, probs = 0.95))
mx <- aggregate(mcpMeasuresAll$mcpmaxWidthKM, by=list(mcpMeasuresAll$sps), FUN="max")

x <- sps[1]
mcpMeasures <- mcpMeasuresAll[mcpMeasuresAll$sps==x]
summary(mcpMeasures$mcpareakm2)
round(quantile(mcpMeasures$mcpareakm2, seq(0.8,1,0.001)))
round(quantile(mcpMeasures$mcpareakm2, seq(0.7,1,0.01)))
hist(mcpMeasures$mcpareakm2, breaks="FD")
hist(mcpMeasures$mcpareakm2[mcpMeasures$mcpareakm2<10], breaks="FD")  

plot(quantile(mcpMeasures$mcpareakm2, seq(0,1,0.001)))
plot(quantile(mcpMeasures$mcpareakm2, seq(0.9,1,0.001)))

hist(mcpMeasures$mcpmaxWidthKM, breaks="FD")
plot(quantile(mcpMeasures$mcpmaxWidthKM, seq(0,1,0.001)))
plot(mcpMeasures$mcpmaxWidthKM,mcpMeasures$mcpareakm2)

plot(quantile(mcpMeasures$mcpareakm2, seq(0,1,0.001)))
points(quantile(mcpMeasures$mcpmaxWidthKM, seq(0,1,0.001)), col="red")

##
selind <- mcpMeasures$fileName[mcpMeasures$mcpareakm2<10]
selind <- mcpMeasures$fileName[mcpMeasures$mcpareakm2>10 & mcpMeasures$mcpareakm2<30]
selind <- mcpMeasures$fileName[mcpMeasures$mcpareakm2>30]
imcpL <- lapply(selind,function(i){
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
#

bigDFsb <- bigDF[bigDF$fileName%in%selind,]
tbAge <- group_by(bigDFsb, fileName, species) %>% reframe(age=unique(animal_life_stage))
table(tbAge$age)

group_by(tbAge, species) %>%
  reframe(na_count = sum(is.na(age)),
          non_na_count = sum(!is.na(age)))


table(bigDF$animal_life_stage[bigDF$species=="Gyps fulvus"])
## check divide less/more than 3yrs - adult/juvenile

moveObj <- readRDS(paste0( "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/5.vultureIndv_mv2_1h_outlspeed_dist/", "560810760_GF_CAU_2010_ADU_W_HNF_Rodin.rds"))

moveObj <- readRDS(paste0( "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/5.vultureIndv_mv2_1h_outlspeed_dist/", "560810760_GF_CAU_2010_ADU_W_HNF_Rodin.rds"))

