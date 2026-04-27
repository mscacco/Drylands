
library(sf)
library(terra)

library(doParallel)
library(plyr)
registerDoParallel(7)


genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pathCoord <- paste0(genPath,"15.vultureIndv_monthlyDBBcoordinatesSF/")
dryl <- st_read("/home/ascharf/Documents/Projects/Drylands/UDsizeChange/Drylands_dataset_2007/Drylands_latest_July2014/drylands_UNCCD_CBD_july2014.shp")

dir.create(paste0(genPath, "18.vultureIndv_UDannotation_drylands"))
annotcorrdsdry <- paste0(genPath, "18.vultureIndv_UDannotation_drylands/")



flsMV <- list.files(pathCoord, full.names = F)
indvFileRDS_L <- flsMV#[1:2]

# indvFileRDS <- flsMV[2000]
month_UD_ann <- function(indvFileRDS){
  
  ind <- readRDS(paste0(pathCoord,indvFileRDS))
  indL <- split(ind,f=ind$monthYear)
  
  # mUD <- indL[[1]]
  mUDL <- lapply(indL, function(mUD){
    
    
    mUD_pj <- st_transform(mUD, st_crs(dryl))
    
    sf::sf_use_s2(FALSE)
    mUD_pj_dry <- st_join(mUD_pj, dryl)
    
    # label non-dryland group
    mUD_pj_dry$HIX_DESC[!mUD_pj_dry$HIX_DESC %in% c("Arid", "Dry subhumid", "Hyperarid", "Semiarid")] <- "Non-dryland"
    
    dry_dbbsum <- aggregate(mUD_pj_dry$dBBvalue, by=list(mUD_pj_dry$HIX_DESC), FUN=sum, na.rm=T)
    dry_wmode <- dry_dbbsum$Group.1[dry_dbbsum$x==max(dry_dbbsum$x)]
    
    
    df <- data.frame(fileName=unique(mUD$fileName), 
                     yearMonth=unique(mUD$monthYear),
                     # wmNDVI=weighted.mean(mUDvec$ndvi,mUDvec$dBBvalue,na.rm=T), 
                     # wmLifestock=weighted.mean(mUDvec$lifestock,mUDvec$dBBvalue,na.rm=T),
                     wmodeDrylands=dry_wmode)
    # rm(rndvi)
    gc()
    return(df)
  })
  
  mUD_df <- do.call("rbind",mUDL)
  saveRDS(mUD_df, file=paste0(annotcorrdsdry,indvFileRDS))
  
}

llply(indvFileRDS_L, function(x){try(month_UD_ann(indvFileRDS=x))},.parallel =T)

# lapply(indvFileRDS_L, function(x){try(month_UD_ann(indvFileRDS=x))},.parallel =T)



annot_l <- lapply(list.files(annotcorrdsdry, full.names = T), function(x) {
  readRDS(x)
})



annot_df <- do.call("rbind", annot_l)
head(annot_df)


tb_mdls <- readRDS(paste0(genPath, "/table_for_models_july2025.rds"))
head(tb_mdls)

tbudann <- merge(tb_mdls, annot_df, by = c("fileName", "yearMonth"))
head(tbudann)

table(tbudann$wmodeDrylands)

saveRDS(tbudann, paste0(genPath, "/table_for_models_april2026.rds"))

