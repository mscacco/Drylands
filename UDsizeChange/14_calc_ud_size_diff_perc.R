library(move)

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"

pthmonthlyUDLpth <- paste0(genPath,"/13.vultureIndv_monthlyUDL/")

dir.create(paste0(genPath,"17.multiple_ud_prec_area"))
udspths <- paste0(genPath,"17.multiple_ud_prec_area/")

flsMV_all <- list.files(pthmonthlyUDLpth, full.names = F)
flsMV_done <- list.files(udspths, full.names = F)
todo <- flsMV_all[!flsMV_all%in%flsMV_done]

# UDpercentage <- 0.99
rasterLayer <- 1000 ## resolution in mts of raster

#indfl <- "352166254__GA_5864__1080727534.rds"  
todo <- todo[!todo%in%c("352166254__GA_5864__1080727534.rds")]

# indfl <- todo[2]
lapply(todo, function(indfl){
  monthlyUDL <- readRDS(paste0(pthmonthlyUDLpth,indfl))
  print(indfl)
  if(length(monthlyUDL)==0){print("no UD")}else{
  UDsizeL <- lapply(1:length(monthlyUDL), function(x){
    ud <- monthlyUDL[[x]]
    dateschr <- names(monthlyUDL[x])
    dateschr <- gsub("X","",dateschr)
    dateschr <- gsub("\\.","-",dateschr)
    
    
    UDsel99 <- ud<=.99
    UDsizem299 <- cellStats(UDsel99, 'sum')*rasterLayer*rasterLayer
    UDsizeKm299 <- UDsizem299/1000000
    
    UDsel95 <- ud<=.95
    UDsizem295 <- cellStats(UDsel95, 'sum')*rasterLayer*rasterLayer
    UDsizeKm295 <- UDsizem295/1000000
    
    UDsel9 <- ud<=.9
    UDsizem29 <- cellStats(UDsel9, 'sum')*rasterLayer*rasterLayer
    UDsizeKm29 <- UDsizem29/1000000
    
    UDsel5 <- ud<=.5
    UDsizem25 <- cellStats(UDsel5, 'sum')*rasterLayer*rasterLayer
    UDsizeKm25 <- UDsizem25/1000000
    
    
    df <- data.frame(fileName=indfl,
                     yearMonth=dateschr,
                     UDsizeKm2_99=UDsizeKm299,
                     UDsizeKm2_95=UDsizeKm295,
                     UDsizeKm2_90=UDsizeKm29,
                     UDsizeKm2_50=UDsizeKm25
    )
    
    return(df)
  })
  dfid <- do.call("rbind",UDsizeL)
  saveRDS(dfid, file=paste0(udspths,indfl))
  rm(monthlyUDL)
  gc()
  }
})


flsMV2 <- list.files(udspths, full.names = T)
UDsize_L <- lapply(flsMV2, readRDS)
UDsize_df <- do.call("rbind",UDsize_L)
saveRDS(UDsize_df,paste0(genPath,"/table_UDsize_multiple_perc.rds"))
