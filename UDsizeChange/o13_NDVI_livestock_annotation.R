args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

library(sf)
library(terra)
library(doParallel)
library(plyr)
registerDoParallel(4)

pathCoord <- "./drylands/15.vultureIndv_monthlyDBBcoordinatesSF/"
ndvipth <- "/MVPR-anenvi/NDVI/global_MOD13A3.061__1_km_monthly_NDVI_filtered/"
lstk <- rast("./drylands/GLW4-2020.D-DA.GLEAM3-ALL-LU.tif")
annotcorrds <- "./drylands/16.vultureIndv_UDannotation/" ## manually create on cluster

flsMV <- list.files(pathCoord, full.names = F)
flsMV_seq_split<- round(seq(1, length(flsMV), length.out=301))
seqInd <- flsMV_seq_split[i]:flsMV_seq_split[i+1]
indvFileRDS_L <- flsMV[seqInd]
# indvFileRDS_L <- flsMV[1:4]

month_UD_ann <- function(indvFileRDS){
  
  ind <- readRDS(paste0(pathCoord,indvFileRDS))
  indL <- split(ind,f=ind$monthYear)
  
  # mUD <- indL[[7]]
  mUDL <- lapply(indL, function(mUD){
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
  saveRDS(mUD_df, file=paste0(annotcorrds,indvFileRDS))
  
}

llply(indvFileRDS_L, function(x){try(month_UD_ann(indvFileRDS=x))},.parallel =T)
