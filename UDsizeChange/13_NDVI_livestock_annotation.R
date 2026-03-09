
## calculated on cluster - see script o13_NDVI....

library(sf)
library(terra)


genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pathCoord <- paste0(genPath,"15.vultureIndv_monthlyDBBcoordinatesSF/")
ndvipth <- "/home/ascharf/Documents/Projects/NDVI_monthly/global_MOD13A3.061__1_km_monthly_NDVI/"
lstk <- rast("/home/ascharf/Documents/Projects/Drylands/UDsizeChange/lifestock/GLW4-2020.D-DA.GLEAM3-ALL-LU.tif")
flsMV <- list.files(pathCoord, full.names = T)

dd <- readRDS(flsMV[10])


ddL <- split(dd,f=dd$monthYear)

# mUD <- ddL[[7]]
mUDL <- lapply(ddL, function(mUD){
  ymth <- paste0(gsub("-","",unique(mUD$monthYear)),"01")
  rndvi <- rast(paste0(ndvipth,ymth,".tif"))
  mUDvec <- vect(mUD)
  mUDvec$ndvi <- extract(rndvi,mUDvec,ID=F)
  mUDvec$lifestock <- extract(lstk,mUDvec,ID=F)
  df <- data.frame(fileName=unique(mUD$fileName), 
                   yearMonth=unique(mUD$monthYear),
                   wmNDVI=weighted.mean(mUDvec$ndvi,mUDvec$dBBvalue,na.rm=T), 
                   wmLifestock=weighted.mean(mUDvec$lifestock,mUDvec$dBBvalue,na.rm=T))
  rm(rndvi)
  gc()
  return(df)
})

mUD_df <- do.call("rbind",mUDL)
