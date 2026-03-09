genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
dat1h <- paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/")

referenceTableStudies <- readRDS(paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
referenceTableStudiesUsed <- referenceTableStudies[referenceTableStudies$excluded=="no",]
min(referenceTableStudiesUsed$tracking_start_date)
max(referenceTableStudiesUsed$tracking_end_date)

dir.create(paste0(genPath,"data_availability_africa"))
savePathOutlDist <- paste0(genPath,"data_availability_africa/")


library(move2)
library(units)
library(dplyr)

flsMV2 <- as.character(referenceTableStudiesUsed$fileName)

library(sf)
library(rnaturalearth)
library(dplyr)

# 1. World polygons with continent info
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, continent)

start_time<- Sys.time()
vultr1d <- lapply(flsMV2, function(ind){
  vultr <- readRDS(paste0(dat1h,ind))
  vultr_thinned <- mt_filter_per_interval(vultr,criterion = "first",unit="day")
  vultr_thinned <- st_join(vultr_thinned, world["continent"])
  vultr_thinned
})
end_time <- Sys.time() 
end_time-start_time


start_time<- Sys.time()
tb_l <- lapply(vultr1d, function(indmv){
  indmv <- filter(indmv, !sf::st_is_empty(indmv))
  indmv <-  mt_as_event_attribute(indmv, taxon_canonical_name)
  mt_track_id(indmv) <- "individual_id"
  indmv <- indmv |> select(timestamp,individual_id,taxon_canonical_name,continent)
  tibble::as_tibble(indmv)
})
end_time <- Sys.time() 
end_time-start_time


start_time<- Sys.time()
tb_sub_l <- lapply(1:length(tb_l), function(x){
  print(x)
  indmv <- tb_l[[x]]
  if(nrow(indmv)>1){
    indmv <- filter(indmv,continent=="Africa" )
    if(nrow(indmv)>1){
      indmv <- indmv |> select(-5)
      indmv$timestamp <- as.Date(indmv$timestamp)
      indmv
    }
  }
})
end_time <- Sys.time() 
end_time-start_time

big_tbl <- bind_rows(tb_sub_l)

big_tbl_L <- split(big_tbl, big_tbl$taxon_canonical_name)

lapply(big_tbl_L, nrow)

library(ggplot2)
ggplot(big_tbl_L[[1]])+ geom_point(aes(timestamp,individual_id))

lapply(big_tbl_L, function(spstb){
  result <- spstb %>%  group_by(individual_id) %>%summarise(min_timestamp = min(timestamp, na.rm = TRUE), .groups = "drop")
  result <- result %>% arrange(min_timestamp)
  result$ind <- as.integer(1:nrow(result))
  spstb <- spstb %>% left_join(result %>% select(individual_id, ind), by = "individual_id")
  sps <- unique(spstb$taxon_canonical_name)
  ggplot(spstb)+ geom_point(aes(timestamp,ind), size=0.5, shape=15, color="forestgreen")+ggtitle(sps)+theme_bw()
  ggsave(filename = paste0(savePathOutlDist,sps,".jpeg"))
})
