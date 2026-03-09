
library(ggplot2)
library(viridis)

dryPath <- "/home/mscacco/ownCloud/Martina/ProgettiVari/Drylands/DryLands/"
#dryPath <- "/home/mscacco/ownCloud - mscacco@ab.mpg.de@owncloud.gwdg.de/Martina/ProgettiVari/Drylands/DryLands/"
#udPath <- "/media/mscacco/6EB4C1E7B4C1B23F/vulturesUDs/9.vultureIndv_monthlyUDL"
udPath <- "/home/mscacco/ownCloud - mscacco@ab.mpg.de@owncloud.gwdg.de/Martina/ProgettiVari/Drylands/DryLands/VutureConference_Nov23/UDrasters/"

UD_ndvi_allYears <- readRDS(paste0(dryPath,"UDs_keeper/UDcentroidsWeight-monthlyNDVI_allIndividuals.rds"))

plot(monthMax_avgNdvi~UDsizeKm2, data=UD_ndvi_allYears[UD_ndvi_allYears$UDsizeKm2<=20000,])
unique(UD_ndvi_allYears$species[UD_ndvi_allYears$UDsizeKm2 > 80000])

#_________________________________
### BOXPLOT of UD size per species
ggplot(UD_ndvi_allYears, aes(x = species, y = UDsizeKm2, fill = species)) +
  geom_boxplot() + xlab("UD size (km2)") + ylab("Species") +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)
ggsave(paste0(dryPath,"Results&Figures/species_boxplot.pdf"))

#___________________
### Plot UD rasters

library(ggmap)
library(ggplot2)
library(move)

# Find an individual with many months
# Himalayan vultures gave nice results in the models
hym <- UD_ndvi_allYears[UD_ndvi_allYears$species=="Gyps himalayensis",]
table(as.character(hym$individual_local_identifier), hym$monthYear)
flN <- unique(hym$fileName[hym$individual_local_identifier=="Thang Kaar Soed Zang (3964)"]) 

indFile <- grep(flN, list.files(udPath, full.names = T), fixed=T, value=T)
ud <- readRDS(indFile)
contour(ud[[1]],levels=.95)
contour(ud[[2]],levels=.95, add=T)

## plotting the UD95 on google map
allUDcontours_ls <- lapply(1:length(ud), function(i){
  spL <- rasterToContour(ud[[i]],levels=.95)
  spL$monthYear <- sub("X","",names(ud)[i])
  return(spL)
})
allUDcontours <- do.call(rbind, allUDcontours_ls)

# with graphics
cols <- rainbow(length(allUDcontours_ls))
plot(extent(bbox(allUDcontours)*2))
lapply(1:length(allUDcontours_ls), function(i){
  plot(allUDcontours_ls[[i]], col=cols[i], add=T)
})
legend(locator(1), sub("X","",names(ud)), col=cols, 
       lwd=2, cex=0.4, bty="n")

# with ggplot2
ggplot(fortify(allUDcontours), aes(x=long, y=lat, group=interaction(id,group), color=id)) + 
  geom_path() +
  theme_minimal()


# cl95 <- rasterToContour(ud[[1]],levels=.95)
# cl95LL <- spTransform(cl95, CRS("+proj=longlat"))
# cl95df <- data.frame(do.call("rbind", coordinates(cl95LL)[[1]]))
# cl95df$polyN <- rep(1:length(coordinates(cl95LL)[[1]]), lapply(coordinates(cl95LL)[[1]], nrow))
# 
# m <- get_map(sp::bbox(extent(cl95LL)*4), zoom=13, source="stamen", maptype="watercolor")
#   ggmap(m)+
#   geom_path(data=cl95df, aes(x=X1,y=X2,group=polyN),color="red")+
#   geom_path(data=leroyDF, aes(x=coords.x1, y=coords.x2),alpha=0.2)+
#   geom_point(data=leroyDF, aes(x=coords.x1, y=coords.x2),alpha=0.3, shape=20)+
#   labs(x="",y="")+
#   theme(axis.text=element_blank(),axis.ticks=element_blank())+
#   theme(legend.position="none")

