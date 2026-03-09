
library(sf)

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
annotcorrdspth <- paste0(genPath,"/16.vultureIndv_UDannotation/")
monthlyUDtablept <- paste0(genPath,"/14.vultureIndv_monthlyUDtable/")
UDsize_df <- readRDS(paste0(genPath,"/table_UDsize_multiple_perc.rds"))
refTb <- readRDS(paste0(genPath,"/referenceTableStudies_ALL_excludedColumn.rds"))
refTb$animal_id <- paste0(refTb$MBid,"_",refTb$individual_local_identifier)

ecoregions <- st_read(paste0(genPath,"Ecoregions2017/Ecoregions2017.shp")) # BIOME_NAME and REALM - "A biogeographic realm is the broadest biogeographic division of Earth's land surface, based on distributional patterns of terrestrial organisms. They are subdivided into bioregions, which are further subdivided into ecoregions. " (Wikipedia)
# SOURCE: Olson et al. Terrestrial Ecoregions of the World: A New Map of Life on Earth: A new global map of terrestrial ecoregions provides an innovative tool for conserving biodiversity, BioScience, Volume 51, Issue 11, November 2001, Pages 933–938, https://doi.org/10.1641/0006-3568(2001)051[0933:TEOTWA]2.0.CO;2
# MAP: https://ecoregions.appspot.com/ 

# fileName, species, animal_id

udtb_l <- lapply(list.files(monthlyUDtablept, full.names = T), function(x){
  readRDS(x)
})
udtb_df <- do.call("rbind",udtb_l)
head(udtb_df)
colnames(udtb_df)[colnames(udtb_df)=="monthYear"] <- "yearMonth"

udtb_df_sf <- st_as_sf(udtb_df[,c("fileName", "yearMonth")], coords=c("UDwMeanLongitude", "UDwMeanLatitude"), crs="EPSG:4326")
udtb_df_sf_pj <- st_transform(udtb_df_sf,st_crs(ecoregions))

sf::sf_use_s2(FALSE)
udtb_df_sf_pj_eco <- st_join(udtb_df_sf_pj, ecoregions)

udtb_df_eco <- merge(udtb_df,udtb_df_sf_pj_eco[,c("fileName", "yearMonth","BIOME_NAME", "REALM")])
head(udtb_df_eco)
udtb_df_eco$geometry <- NULL

annot_l <- lapply(list.files(annotcorrdspth, full.names = T), function(x){
  readRDS(x)
})
annot_df <- do.call("rbind",annot_l)
head(annot_df)
tbudann <- merge(udtb_df_eco,annot_df, by=c("fileName", "yearMonth"))

summary(tbudann)

tb_mrg <- merge(tbudann,refTb[,c("fileName", "species", "animal_id")], by="fileName", all.x=T, all.y=F)
head(tb_mrg)

tb_tot <- merge(tb_mrg,UDsize_df, by=c("fileName", "yearMonth"),all.x=T, all.y=F)
head(tb_tot)

tb_tot$UDsizeKm2 <- NULL
tb_tot$locsPerMonth <- NULL
tb_tot$UDcentroidsLongitude <- NULL
tb_tot$UDcentroidsLatitude <- NULL
tb_mdls <- tb_tot
tb_mdls$UDsizeKm2_95 <- NULL
tb_mdls$UDsizeKm2_90 <- NULL
tb_mdls$UDsizeKm2_50 <- NULL
head(tb_mdls)
saveRDS(tb_mdls,paste0(genPath,"/table_for_models_july2025.rds"))

# names(tb_tot)
# [1] "fileName"         "yearMonth"        "UDsizeKm2"        "UDwMeanLongitude" "UDwMeanLatitude"  "wmNDVI"          
# [7] "wmLifestock"      "species"          "animal_id"    

dd <- data.frame(sort(table(as.character(tb_tot$species))))

# Var1  Freq
# 1    Cathartes burrovianus     1
# 2         Gyps bengalensis     4
# 3          Gyps rueppellii     8
# 4        Sarcoramphus papa    31
# 5           Vultur gryphus   346
# 6  Trigonoceps occipitalis   384
# 7        Gyps himalayensis   404
# 8      Torgos tracheliotus   437
# 9     Necrosyrtes monachus   758
# 10        Coragyps atratus   950
# 11        Gyps coprotheres  1721
# 12       Gypaetus barbatus  2073
# 13          Cathartes aura  2944
# 14          Gyps africanus  5445
# 15       Aegypius monachus  7257
# 16   Neophron percnopterus  9728
# 17             Gyps fulvus 15135

result <- aggregate(animal_id ~ species, data = tb_tot, FUN = function(x) length(unique(x)))
 ddr <- merge(dd,result, by.x="Var1", by.y="species")


tb_plot <- tb_tot
tb_plot <- tb_plot[!tb_plot$species%in%c( "Cathartes burrovianus", "Gyps bengalensis", "Gyps rueppellii","Sarcoramphus papa" ),]

tb_plot$year <- as.integer(substr(tb_plot$yearMonth, 1, 4))
tb_plot$month <- as.integer(substr(tb_plot$yearMonth, 6, 7))
summary(tb_plot)
# [1] "fileName"         "yearMonth"        "UDwMeanLongitude" "UDwMeanLatitude"  "wmNDVI"           "wmLifestock"     
# [7] "species"          "animal_id"        "UDsizeKm2_99"     "UDsizeKm2_95"     "UDsizeKm2_90"     "UDsizeKm2_50"    
# [13] "year"             "month"  "BIOME_NAME"           "REALM" 

tb_plot$migratory <- "no"
tb_plot$migratory[tb_plot$species%in%c("Gyps fulvus","Neophron percnopterus","Cathartes aura")] <- "yes"


library(ggplot2)
ggplot(tb_plot)+ geom_boxplot(aes(UDsizeKm2_99, species,color=migratory))+theme_bw()+ggtitle("size of monthly 99%UD (Km2)")
ggplot(tb_plot)+ geom_boxplot(aes(UDsizeKm2_99, species,color=migratory))+theme_bw()+ggtitle("size of monthly 99%UD (Km2) - log10 transfomed")+scale_x_continuous(trans="log10")
ggplot(tb_plot)+ geom_boxplot(aes(UDsizeKm2_95, species,color=migratory))+theme_bw()+ggtitle("size of monthly 95%UD (Km2)")
ggplot(tb_plot)+ geom_boxplot(aes(UDsizeKm2_90, species,color=migratory))+theme_bw()+ggtitle("size of monthly 90%UD (Km2)")
ggplot(tb_plot)+ geom_boxplot(aes(UDsizeKm2_50, species,color=migratory))+theme_bw()+ggtitle("size of monthly 50%UD (Km2)")

ggplot(tb_plot)+ geom_boxplot(aes(wmNDVI, species,color=migratory))+theme_bw()+ggtitle("weighted mean NDVI in monthly UDs")
ggplot(tb_plot)+ geom_boxplot(aes(wmLifestock,species ,color=migratory))+theme_bw()+ggtitle("weighted mean lifestock density in monthly UDs")#+scale_x_continuous(trans="log10")

ggplot(tb_plot)+ geom_histogram(aes(year),bins=22)+theme_bw()+facet_wrap(~species,ncol=5)+ggtitle("number of monthly UDs per year")
ggplot(tb_plot)+ geom_histogram(aes(month), bins=12)+scale_x_continuous(breaks=seq(0,12,2))+theme_bw()+facet_wrap(~species,ncol=5)+ggtitle("number of monthly UDs per month (for all years)")

tb_plot_na <- tb_plot[!tb_plot$REALM%in%c(NA,"N/A"),]
ggplot(tb_plot_na)+geom_bar(aes(REALM))+scale_x_discrete(guide = guide_axis(angle = 90))
ggplot(tb_plot_na)+geom_bar(aes(BIOME_NAME))+scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(tb_plot_na)+geom_bar(aes(REALM))+facet_wrap(~species,ncol=5)+scale_x_discrete(guide = guide_axis(angle = 90))+theme_bw()#+ggtitle("weighted mean lifestock density in monthly UDs")
ggplot(tb_plot_na)+geom_bar(aes(BIOME_NAME))+facet_wrap(~species,ncol=5)+scale_x_discrete(guide = guide_axis(angle = 90))+theme_bw()#+ggtitle("weighted mean lifestock density in monthly UDs")
ggplot(tb_plot_na)+geom_bar(aes(species))+facet_wrap(~REALM,ncol=5)+scale_x_discrete(guide = guide_axis(angle = 90))+theme_bw()#+ggtitle("weighted mean lifestock density in monthly UDs")
ggplot(tb_plot_na)+geom_bar(aes(species))+facet_wrap(~BIOME_NAME,ncol=5)+scale_x_discrete(guide = guide_axis(angle = 90))+theme_bw()#+ggtitle("weighted mean lifestock density in monthly UDs")


library(ggplot2)
library(sf)
library(patchwork)

species_list <- unique(tb_plot_na$species)

# Create a plot for each species, individually zoomed and without a legend
plots <- lapply(species_list, function(sp) {
  df_sub <- tb_plot_na[tb_plot_na$species == sp, ]
  x_limits <- range(df_sub$UDwMeanLongitude, na.rm = TRUE) + c(-1, 1)
  y_limits <- range(df_sub$UDwMeanLatitude, na.rm = TRUE) + c(-1, 1)
  
  ggplot(df_sub) +
    theme_void() +
    geom_sf(data = worldMap, color = "grey85", fill = "grey90") +
    geom_point(aes(UDwMeanLongitude, UDwMeanLatitude, color = BIOME_NAME)) +
    coord_sf(xlim = x_limits, ylim = y_limits) +
    ggtitle(sp) +
    theme(legend.position = "none",
          panel.border = element_rect(color = "black", fill = NA))
})

# Combine plots and place a single, shared legend on the right
wrap_plots(plots, ncol = 4) + plot_layout(widths = rep(1, 4))

plots2 <- lapply(species_list, function(sp) {
  df_sub <- tb_plot_na[tb_plot_na$species == sp, ]
  x_limits <- range(df_sub$UDwMeanLongitude, na.rm = TRUE) + c(-1, 1)
  y_limits <- range(df_sub$UDwMeanLatitude, na.rm = TRUE) + c(-1, 1)
  
  ggplot(df_sub) +
    theme_void() +
    geom_sf(data = worldMap, color = "grey85", fill = "grey90") +
    geom_point(aes(UDwMeanLongitude, UDwMeanLatitude, color = REALM)) +
    coord_sf(xlim = x_limits, ylim = y_limits) +
    ggtitle(sp) +
    theme(legend.position = "none",
          panel.border = element_rect(color = "black", fill = NA))
})

# Combine plots and place a single, shared legend on the right
wrap_plots(plots2, ncol = 4) + plot_layout(widths = rep(1, 4))



lapply(split(tb_plot, as.character(tb_plot$species)), function(x){
  print(unique(x$species))
  quantile(x$UDsizeKm2_99, probs=seq(0.98,1,0.005), rm.na=T) ## cutoff 99%
  
})
quantile(tb_plot$UDsizeKm2_99, probs=seq(0.98,1,0.005)) ## cutoff 99%

tb_plot$remQ <- F
for(i in unique(as.character(tb_plot$species))){
  tbsp <- tb_plot[tb_plot$species==i,]
  q99 <- quantile(tbsp$UDsizeKm2_95, probs=0.995)
  tb_plot$remQ[tb_plot$species==i & tb_plot$UDsizeKm2_95>q99] <- T
}

tb_plot$remQ <- F
q99 <- quantile(tb_plot$UDsizeKm2_99, probs=0.995)
tb_plot$remQ[tb_plot$UDsizeKm2_99>q99] <- T

ggplot(tb_plot)+ 
 # geom_boxplot(aes(UDsizeKm2_99, species,color=remQ))+
  theme_bw()+ggtitle("size of monthly 99%UD (Km2)")+
 geom_point(aes(UDsizeKm2_95, species,color=remQ))



ggplot(tb_plot[tb_plot$remQ==F,])+ geom_boxplot(aes(UDsizeKm2_99, species,color=migratory))+theme_bw()+ggtitle("size of monthly 99%UD (Km2)")



