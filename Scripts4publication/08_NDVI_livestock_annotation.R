# ---
# Title: Annotation of UDs with corresponding NDVI and livestock values
# Author: Anne K Scharf, MPIAB
# Date: July 2025
# Description: coordinates for UDs are annotated with correponding monthly 
#             NDVI raster, and weighted mean (weight: dBBMM value) is calculated. 
#             Weighted mean of livestock is also calculated
# ---

library("sf")
library("terra")
library("doParallel")
library("plyr")
registerDoParallel(4) ## chose number of cores (maximum available-1)

genPath <- "/path_to_folder/"


pathCoord <- paste0(genPath, "10.vultureIndv_monthlyDBBcoordinatesSF/")
ndvipth <- paste0(genPath, "/NDVI/global_MOD13A3.061__1_km_monthly_NDVI/")
lstk <- rast(paste0(genPath, "/GLW4-2020.D-DA.GLEAM3-ALL-LU.tif")) # https://data.apps.fao.org/catalog//iso/9d1e149b-d63f-4213-978b-317a8eb42d02
dir.create(paste0(genPath, "11.vultureIndv_UDannotation"))
annotcorrds <- paste0(genPath, "11.vultureIndv_UDannotation/")


month_UD_ann <- function(indvFileRDS) {
  ind <- readRDS(paste0(pathCoord, indvFileRDS))
  indL <- split(ind, f = ind$yearMonth)

  mUDL <- lapply(indL, function(mUD) {
    ymth <- paste0(gsub("-", "", unique(mUD$yearMonth)), "01")
    rndvi <- rast(paste0(ndvipth, ymth, ".tif"))
    mUDvec <- vect(mUD)
    mUDvec$ndvi <- extract(rndvi, mUDvec, ID = F)
    mUDvec$livestock <- extract(lstk, mUDvec, ID = F)
    df <- data.frame(
      fileName = unique(mUD$fileName),
      yearMonth = unique(mUD$yearMonth),
      wmNDVI = weighted.mean(mUDvec$ndvi, mUDvec$dBBvalue, na.rm = T),
      wmLifestock = weighted.mean(mUDvec$livestock, mUDvec$dBBvalue, na.rm = T)
    )
    rm(rndvi)
    gc()
    return(df)
  })

  mUD_df <- do.call("rbind", mUDL)
  saveRDS(mUD_df, file = paste0(annotcorrds, indvFileRDS))
}


flscords <- list.files(pathCoord, full.names = F)
llply(flscords, function(x) {
  try(month_UD_ann(indvFileRDS = x))
}, .parallel = T)
