# s1 <- unzip("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season1/100km2.zip")
# s2 <- unzip("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season2/100km2.zip")
# s3 <- unzip("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season3/100km2.zip")
# 
# gen <- c("Gyps", "Necrosyrtes", "Trigonoceps", "Torgos", "Vultur", "Sarcogyps", "Neophron", "Gypaetus", "Aegypius", "Gymnogyps", "Coragyps", "Cathartes","Sarcoramphus","Gypohierax")
# gen_ <- paste0(gen,"_")
# 
# vulS1L <- lapply(seq(gen_), function(i){
#   s1[grepl(gen_[i], s1, perl = TRUE)]
# })
# vulS1 <- unlist(vulS1L)
# gsub("./100km2/","",vulS1)
# vulS2L <- lapply(seq(gen_), function(i){
#   s2[grepl(gen_[i], s2, perl = TRUE)]
# })
# vulS2 <- unlist(vulS2L)
# gsub("./100km2/","",vulS2)
# vulS3L <- lapply(seq(gen_), function(i){
#   s3[grepl(gen_[i], s3, perl = TRUE)]
# })
# vulS3 <- unlist(vulS3L)
# gsub("./100km2/","",vulS3)
# 
# allNames <- c(vulS1,vulS2,vulS3)
# table(allNames)
# ################
# library(raster)
# am_s1 <- raster("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season1/100km2/Aegypius_monachus")
# am_s2 <- raster("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season2/100km2/Aegypius_monachus")
# am_s3 <- raster("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season3/100km2/Aegypius_monachus")
# plot(am_s1)
# plot(am_s2)
# plot(am_s3)
# 
# all <- sum(am_s1,am_s2,am_s3)
# plot(all, col=rev(rainbow(4)))
# 
# #######
# 
# 
# Fs1 <- list.files("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season1/100km2/", full.names=T)
# Fs2 <- list.files("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season2/100km2/", full.names=T)
# Fs3 <- list.files("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season3/100km2/", full.names=T)
# 
# gen <- c("Gyps", "Necrosyrtes", "Trigonoceps", "Torgos", "Vultur", "Sarcogyps", "Neophron", "Gypaetus", "Aegypius", "Gymnogyps", "Coragyps", "Cathartes","Sarcoramphus","Gypohierax")
# gen_ <- paste0(gen,"_")
# 
# vulS1L <- lapply(seq(gen_), function(i){
#   Fs1[grepl(gen_[i], Fs1, perl = TRUE)]
# })
# vulS1 <- unlist(vulS1L)
# 
# vulS2L <- lapply(seq(gen_), function(i){
#   Fs2[grepl(gen_[i], Fs2, perl = TRUE)]
# })
# vulS2 <- unlist(vulS2L)
# 
# vulS3L <- lapply(seq(gen_), function(i){
#   Fs3[grepl(gen_[i], Fs3, perl = TRUE)]
# })
# vulS3 <- unlist(vulS3L)
# 
# 
# all <- c(vulS1,vulS2,vulS3)

####################################
### make one raster per species ###
####################################
library(terra)
spsNamesL <- list.files("/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season1/100km2/", full.names=F)

gen <- c("Gyps", "Necrosyrtes", "Trigonoceps", "Torgos", "Vultur", "Sarcogyps", "Neophron", "Gypaetus", "Aegypius", "Gymnogyps", "Coragyps", "Cathartes","Sarcoramphus","Gypohierax")
gen_ <- paste0(gen,"_")

vulspsNamesL <- lapply(seq(gen_), function(i){
  spsNamesL[grepl(gen_[i], spsNamesL, perl = TRUE)]
})
vulspsNames <- unlist(vulspsNamesL)

pths1 <- "/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season1/100km2/"
pths2 <- "/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season2/100km2/"
pths3 <- "/home/ascharf/GIT_SYNC/AnEnvIPaper/RasteRanges/Season3/100km2/"
outpth <- "/home/ascharf/Documents/Projects/Drylands/EVC23/A1.rastersOfAllSpeciesRange/"
sps <- vulspsNames[1]
lapply(vulspsNames, function(sps){
  try(s1 <- rast(paste0(pths1,sps)))
  try(s2 <- rast(paste0(pths2,sps)))
  try(s3 <- rast(paste0(pths3,sps)))
  all <- c(s1,s2,s3)
  all_r <- sum(all)
  names(all_r) <- sps
  all_r[all_r > 1] <- 1
  writeRaster(all_r,paste0(outpth,sps,".tiff"), overwrite=TRUE)
})

################################################
####### rasterize 1h data to range raster ######
################################################
library(move2)
library(sf)
library(terra)

genPath <- "/home/ascharf/Documents/Projects/Drylands/EVC23/"
data1hPth <- "4.vultureIndv_mv2_1h_outlspeed/"
rastPth <- "A1.rastersOfAllSpeciesRange/"
datrngPth <- "A2.RasterDataOnRange/"

referenceTableStudies_ALL <- readRDS(file=paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
indvUsed <- referenceTableStudies_ALL[referenceTableStudies_ALL$excluded=="no",]

indvSpsL <- split(indvUsed, indvUsed$species)
lapply(indvSpsL,nrow)

 spsNm <- "Gyps rueppellii"

 lapply(unique(indvUsed$species), function(spsNm)try({
  spsNm_ <- gsub(" ","_",spsNm)
  rngrast <- rast(paste0(genPath,rastPth,spsNm_,".tiff"))
  
  indvNms <- indvSpsL[[spsNm]]$fileName
  indvCoordsL <- lapply(indvNms, function(ind){
    vul <- readRDS(paste0(genPath,data1hPth,ind))
    vul_t <- st_transform(vul,crs(rngrast))
    st_coordinates(vul_t)
  })
  indvCoords <- do.call("rbind",indvCoordsL)
  
  
  spsrast <- rasterize(indvCoords,rngrast)#, fun="length")
  spsrast[spsrast > 0] <- 10
  # plot(spsrast)
  
  spsrast_range <- sum(spsrast,rngrast,na.rm=T)
  plot(spsrast_range)
  spsrast_range[spsrast_range > 1] <- 10
  names(spsrast_range) <- spsNm_
  
  writeRaster(spsrast_range,paste0(genPath,datrngPth,spsNm_,".tiff"), overwrite=TRUE)
}))
