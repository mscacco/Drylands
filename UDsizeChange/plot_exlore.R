library(lubridate)
library(move2)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(units)


indp <-  "327161259_Deborah-0HA.rds"
# "327161259_Deborah-0HA.rds" # es/fr
# "327161259_Bubu.rds" # es
# "481458_Andres.rds" #usa
# "327161259_Cid.rds" #eu/afri
genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
ind <- readRDS(paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/",indp))
weekdispl <- readRDS(paste0(genPath,"6.weeklyDisplacementsTables/", indp))
ind$weekYear <- paste0(week(mt_time(ind)),"-",year(mt_time(ind)))
ind <- left_join(ind, weekdispl, by = "weekYear")
indMth <- ind
mt_track_id(indMth) <- "weekYear"
indMth <- mt_as_track_attribute(indMth,locsPerWeek,.keep=T)
# "327161259_Deborah-0HA.rds" # es/fr
# "327161259_Bubu.rds" # es
# "481458_Andres.rds" #usa
# "327161259_Cid.rds" #eu/afri

indMth360 <- filter_track_data(indMth,locsPerWeek>=84)
indMth360 <- filter(indMth360,maxNetDispl>=units::set_units(50,"km"))
indMth360 <- filter(indMth360,straightnessIndex<=0.5)

unique(mt_track_id(indMth360))
indMth360 <- filter_track_data(indMth360,.track_id=paste0(1:52, "-","2015" ))
length(unique(mt_track_id(indMth360)))
indMth360 <- indMth360[order(mt_track_id(indMth360)),]
worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
bb <- st_bbox(indMth360)
exp <- 2
ggplot() + theme_void() +
  geom_sf(data = worldMap) +
  geom_sf(data = indMth360,aes(color = weekYear), show.legend = F) +
  # geom_sf(data = mt_track_lines(indMth360), aes(color = weekYear), show.legend = F) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)#+
  # facet_wrap(~weekYear)


hist(indMth360$maxNetDispl)
hist(indMth360$distFirstLast)
hist(indMth360$straightnessIndex)
plot(indMth360$maxNetDispl~indMth360$distFirstLast)
plot(indMth360$maxNetDispl^2~indMth360$straightnessIndex)

hist(indMth$straightnessIndex)
plot(maxNetDispl^2~distFirstLast, data=indMth360[indMth360$maxNetDispl< units::set_units(200,"km"),])



library(EMbC)
indWeek84 <- filter_track_data(indMth,locsPerWeek>=84)
mydat <- data.frame(maxd=indWeek84$maxNetDispl, srghtI=indWeek84$straightnessIndex) 
mymat <- as.matrix(mydat)
mymat <- mymat[complete.cases(mymat),]
bc <- embc(mymat)
# lkhp(bc)
# lkhp(bc,15)
lblp(bc)

# obtain 4 categories, resulting from the combinations of high/low 
# par(mfrow=c(1,1))
sctr(bc)
stts(bc)
# sctr(bc, showVars=c(1, 2, 3))

Cat <- bc@A
# Cat[Cat %in% c(1,2,3)] <- "stationary"
# Cat[Cat %in% c(4)] <- "migrating"
# Cat[Cat %in% c(1,4)] <- "stationary"
# Cat[Cat %in% c(3,5:8)] <- "migrating"

indWeek84$embcCat <- Cat

ggplot() + theme_void() +
  # geom_sf(data = worldMap) +
  geom_sf(data = indWeek84,aes(color = as.factor(embcCat)), show.legend = F) +
  # geom_sf(data = mt_track_lines(indWeek84), aes(color = embcCat), show.legend = F)
facet_wrap(~embcCat)
