library(terra)
library(ggplot2)
library(tidyterra)
library(rnaturalearth)

allrange <- rast(list.files("/home/ascharf/ownCloud/_quick_between/A1.rastersOfAllSpeciesRange/", full.names = T))
plot(allrange[[1]])
sumallrange <- sum(allrange)
plot(sumallrange)

allpresrange <- rast(list.files("/home/ascharf/ownCloud/_quick_between/A2.RasterDataOnRange/", full.names = T))
plot(allpresrange[[1]])
sumallpresrange <- sum(allpresrange)
plot(sumallpresrange)

onlypres <- allpresrange
onlypres[onlypres == 1] <- 0
sumonlypres <- sum(onlypres)
plot(sumonlypres)

range1 <- sumallrange
range1[range1 > 1] <- 1
range1[range1 ==0] <- NA
range1[range1 ==1] <- 0
plot(range1)

plot(range1, col=c("white","red"))
sumonlypres[sumonlypres == 0] <- NA
plot(sumonlypres, add=T)

names(sumonlypres)
sumonlypres <- sumonlypres/10
ggplot() +
  geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_spatraster(data = sumonlypres, aes(fill = sum)) +
  coord_sf(crs = crs(sumonlypres)) +
  scale_fill_whitebox_c(palette="viridi", direction=-1)+ 
  theme_bw()

sumallrange[sumallrange == 0] <- NA
ggplot() +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan")+
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_spatraster(data = sumallrange, aes(fill = sum)) +
  coord_sf(crs = crs(sumallrange)) +
  scale_fill_whitebox_c("Nb species (range)", palette="viridi", direction=1,limits=c(1,17))+ 
  theme_bw()
ggsave("/home/ascharf/ownCloud/DryLands/VutureConference_Nov23/plotsVRC/range_overlap.png",width = 30,height = 15, units="cm")  

ggplot() +
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan")+ #paleturquoise1
  # geom_spatraster(data = range1) +
  geom_spatraster(data = sumonlypres, aes(fill = sum)) +
  coord_sf(crs = crs(sumonlypres)) +
  scale_fill_whitebox_c("Nb species (data)",palette="viridi", direction=1)+
  # scale_fill_whitebox_c("Nb species (data)",palette="viridi", direction=-1,limits=c(0,17))+
  theme_bw()
ggsave("/home/ascharf/ownCloud/DryLands/VutureConference_Nov23/plotsVRC/data.png",width = 30,height = 15, units="cm") 

ggplot() +
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan")+ #paleturquoise1
  geom_spatraster(data = range1) +
  geom_spatraster(data = sumonlypres, aes(fill = sum)) +
  coord_sf(crs = crs(sumonlypres)) +
  scale_fill_whitebox_c("Nb species (data)",palette="viridi", direction=1)+
  # scale_fill_whitebox_c("Nb species (data)",palette="viridi", direction=-1,limits=c(0,17))+
  theme_bw()
ggsave("/home/ascharf/ownCloud/DryLands/VutureConference_Nov23/plotsVRC/data_overlap_scalerange.png",width = 30,height = 15, units="cm") 

stk <- rast(list(range1,sumonlypres))
names(stk) <- c("range","data")

ggplot() +
  geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_spatraster(data=stk,aes(fill=range)) +
  geom_spatraster(data = stk, aes(fill = data)) +
  coord_sf(crs = crs(sumonlypres)) +
  # scale_fill_whitebox_c("Nb species (data)",palette="viridi", direction=-1)+
  scale_fill_whitebox_c("Nb species (data)",palette="viridi")+#, direction=-1,limits=c(1,17))+
  # scale_color_continuous(na.value = NA)+
  theme_bw()


sumallrange0 <- sumallrange
sumallrange0[is.na(sumallrange0)] <- 0
sumonlypres0 <- sumonlypres
sumonlypres0[is.na(sumonlypres0)] <- 0
diffR <- sumallrange0-sumonlypres0
diffR[diffR<=0] <- NA

ggplot() +
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan")+ #paleturquoise1
  geom_spatraster(data = diffR, aes(fill = sum)) +
  coord_sf(crs = crs(diffR)) +
  scale_fill_whitebox_c("Nb species with no data",palette="viridi", direction=1)+
  # scale_fill_whitebox_c("Nb species (data)",palette="viridi", direction=-1,limits=c(0,17))+
  theme_bw()
ggsave("/home/ascharf/ownCloud/DryLands/VutureConference_Nov23/plotsVRC/NBspeciesWithNoData.png",width = 30,height = 15, units="cm") 


allpresrangeNA <- allpresrange
allpresrangeNA[allpresrangeNA==0] <- NA

dt <- data.frame(id=c(1,10), type=c("range","data"))

names <- read.csv("/home/ascharf/ownCloud/DryLands/VutureConference_Nov23/plotsVRC/cientific_comon_name.csv") ## created below
spsL <- names(allpresrangeNA)
# i <- spsL[1]
lapply(spsL, function(i){
  sciname <- names$spsNamePretty[names$spsname==i]
  comonname <- names$Common.name[names$spsname==i]
  pltname <- paste0(sciname," (",comonname,")")
spsRast <- allpresrangeNA[[i]]
levels(spsRast) <- dt
ggplot() +
  geom_sf(data=ocean, fill="skyblue1", alpha=.4)+geom_sf(data=land, fill="tan", alpha=.4)+
  # geom_sf(data = ne_coastline(returnclass = "sf", 50), color="grey70") +
  # geom_spatraster(data = range1) +
  geom_spatraster(data = spsRast)+#, aes(fill = i)) +
  coord_sf(crs = crs(spsRast)) +
  scale_fill_manual("" ,values=c("green4","magenta3"),na.translate=F)+
  # scale_fill_whitebox_d("",palette="purple",na.translate=F,direction=-1)+
  theme_bw()+
  ggtitle(pltname)+
  theme(
    legend.position = c(.98, .995),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
    )
  
ggsave(paste0("/home/ascharf/ownCloud/DryLands/VutureConference_Nov23/plotsVRC/data_overlap_scalerange_",i,".png"),width = 30,height = 15, units="cm")
})


misssps <- names(allrange)[!names(allrange)%in%names(allpresrange)]

mistb <- names[names$spsname%in%misssps,]


### table with equivalence sps name- comon name
allspsN <- list.files("/home/ascharf/ownCloud/_quick_between/A1.rastersOfAllSpeciesRange/", full.names = F)
allspsN <- gsub(".tiff","",allspsN)
allspsN2 <- gsub("_"," ",allspsN)

alldf <- data.frame(spsname=allspsN)
alldf$spsNamePretty <- gsub("_"," ",alldf$spsname)
dfnms <- read.csv("/home/ascharf/Downloads/sps_comon_names.csv") # from Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_7.xlsx downloaded from birdlife

df2 <- merge(alldf, dfnms[,c("Common.name","Scientific.name")], by.x="spsNamePretty", by.y="Scientific.name", all.x=T)
write.csv(df2,file="/home/ascharf/ownCloud/DryLands/VutureConference_Nov23/plotsVRC/cientific_comon_name.csv")


#########

library(rnaturalearth)
# land <- ne_download(scale=110, type="land", category="physical", destdir="/home/ascharf/ownCloud/_quick_between/naturalearth/",returnclass="sf")
# 
# ocean <- ne_download(scale=110, type="ocean", category="physical", destdir="/home/ascharf/ownCloud/_quick_between/naturalearth/",returnclass="sf")
# # ne_load()
ocean <- ne_load(scale=110, type="ocean", category="physical", destdir="/home/ascharf/ownCloud/_quick_between/naturalearth/",returnclass="sf")
land <- ne_load(scale=110, type="land", category="physical", destdir="/home/ascharf/ownCloud/_quick_between/naturalearth/",returnclass="sf")


ggplot()+geom_sf(data=ocean, fill="paleturquoise1")+geom_sf(data=land, fill="bisque")+coord_sf(crs = crs(spsRast)) 
