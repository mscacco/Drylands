args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[1])

library(move)
library(doParallel)
library(plyr)
registerDoParallel(4)

pthmonthlyUDLpth <- "./drylands/13.vultureIndv_monthlyUDL/"
udspths <- "./drylands/17.multiple_ud_prec_area/" ## needs to be created manually on cluster

flsMV <- list.files(pthmonthlyUDLpth, full.names = F)
flsMV_seq_split<- round(seq(1, length(flsMV), length.out=301))
seqInd <- flsMV_seq_split[i]:flsMV_seq_split[i+1]
indvFileRDS_L <- flsMV[seqInd]

rasterLayer <- 1000 ## resolution in mts of raster

monthSize <-  function(indfl){
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
}

llply(indvFileRDS_L, function(x){try(monthSize(indfl=x))},.parallel =T)


######################
### on pc #########

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
udspths <- paste0(genPath,"17.multiple_ud_prec_area")
flsMV2 <- list.files(udspths, full.names = T)
UDsize_L <- lapply(flsMV2, readRDS)
UDsize_df <- do.call("rbind",UDsize_L)
saveRDS(UDsize_df,paste0(genPath,"/table_UDsize_multiple_perc.rds"))
