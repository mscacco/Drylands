library(lubridate)
library(move2)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(units)

length(unique(migratory_month$fileName))
# [1] 3061
length(unique(migratory_month$fileName[migratory_month$locsPerMonth>=360]))
# [1] 2398


# "327161259_Deborah-0HA.rds" # es/fr
# "327161259_Bubu.rds" # es

# "481458_Andres.rds" #usa
# "327161259_Cid.rds" #eu/afri

# 327161259 - Aegypius monachus - Cinereous vulture
# 481458 - Cathartes aura - Turkey vulture

indv <- c("327161259__Deborah-0HA__1283254784.rds","327161259__Bubu__588370927.rds")
# indv <- c("481458__Andres__20769287.rds","327161259__Cid__976053874.rds")



caur <- c("481458__Andres__20769287.rds","481458__Steamhouse 1__2076923.rds","481458__Tesla__14011527.rds","481458__Leo__37900997.rds","481458__Captain Haddock__233577534.rds")
amon <- c("327161259__Deborah-0HA__1283254784.rds","327161259__Bubu__588370927.rds","327161259__Cid__976053874.rds")
gful <- c("1575276297__6RV__2150849613.rds", "1575276297__0XV__1600748808.rds", "1502676405__7W2__2800792332.rds", "1502676405__6R9__2136860943.rds","533218859__Calcón__2119710070.rds", "533218859__SAIA__894556446.rds","533218859__Matal__2119711382.rds.", "1591294976__AZ (6UU)__2125993736.rds")
ghim <- ("20202974__Thang Kaar Tooh (4188)__54325576.rds")
nperc <- c("316961802__Morgaez-Alius21__1672264258.rds", "316961802__Huebra__316964725.rds", "20106351__Azahar 75657__20107144.rds", "572056515__Gamla Quarry H59 Red__2886423492.rds","572056515__HBC IB5 Red__1248681303.rds")

indv <- caur

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pthlocs <- paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/")
pthmcp <- paste0(genPath,"9.mcp95_monthly/")
pthdbbvar <- paste0(genPath,"11.vultureIndv_monthlydBBvar/")
pthud <- paste0(genPath,"13.vultureIndv_monthlyUDL/")
migratory_month <- readRDS("/home/ascharf/Documents/Projects/Drylands/UDsizeChange/migratory_month.rds")

x <- 1

pts <- readRDS(paste0(pthlocs,indv[x]))
mcps <- readRDS(paste0(pthmcp,indv[x]))
dvar <- readRDS(paste0(pthdbbvar,indv[x]))
ud <- readRDS(paste0(pthud,indv[x]))

pts$monthYear <- paste0(year(mt_time(pts)),"-",sprintf("%02d",month(mt_time(pts))))
migrMnth <- as.character(migratory_month$monthYear[migratory_month$fileName==indv[x] & migratory_month$migratoryMonth==T])
pts$migrMnth <- F
pts$migrMnth[pts$monthYear%in%migrMnth] <- T
pts$month <- month(mt_time(pts))
pts$year <- year(mt_time(pts))
# pts$bvar <- dvar@means
# plot(mcps)

mcps$migrMnth <- F
mcps$migrMnth[mcps$yrm%in%migrMnth] <- T


worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
bb <- st_bbox(pts)
exp <- 2


ggplot() + theme_void() +
  geom_sf(data = worldMap) +
  geom_sf(data = pts, aes(color = migrMnth), show.legend = T) +
  # geom_sf(data = pts, aes(color = monthYear), show.legend = T) +
  # geom_sf(data = mt_track_lines(pts), aes(color = migrMnth), show.legend = F) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)#+


ggplot() + theme_void() +
  geom_sf(data = worldMap) +
  geom_sf(data = pts, aes(color = migrMnth), show.legend = T) +
  # geom_sf(data = pts, aes(color = monthYear), show.legend = T) +
  # geom_sf(data = mt_track_lines(pts), aes(color = migrMnth), show.legend = F) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)+
facet_wrap(~monthYear)

ggplot() + theme_void() +
  geom_sf(data = worldMap) +
  geom_sf(data = pts, aes(color = migrMnth), show.legend = T) +
  # geom_sf(data = pts, aes(color = monthYear), show.legend = T) +
  # geom_sf(data = mt_track_lines(pts), aes(color = migrMnth), show.legend = F) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)+
  facet_grid(year~month)+
  ggtitle("Aegypius monachus")
ggsave("Aegypius_monachus_explr_classification.jpeg",width = 40, height = 20, units = "cm")

ggplot() + theme_void() +
  geom_sf(data = worldMap) +
  geom_sf(data = pts, aes(color = bvar), show.legend = T) +
  # geom_sf(data = dvar, aes(color = monthYear), show.legend = T) +
  # geom_sf(data = mt_track_lines(pts), aes(color = migrMnth), show.legend = F) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)+
  facet_grid(year~month)

ggplot() + theme_void() +
  # geom_sf(data = worldMap) +
  geom_sf(data = mcps, aes(color = migrMnth), show.legend = T) +
  facet_wrap(~migrMnth)


jpeg("Aegypius_monachus_explr_dbbvar.jpeg",width = 40, height = 20, units = "cm", res=150)
plot(mt_time(pts), pts$bvar, pch=20, col=c("orangered3","cyan3")[as.numeric(pts$migrMnth)+1], main="Aegypius monachus explr")
dev.off()


###############################

library(lubridate)
library(move2)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(units)

caur <- c("481458__Andres__20769287.rds","481458__Steamhouse 1__2076923.rds","481458__Tesla__14011527.rds","481458__Leo__37900997.rds","481458__Captain Haddock__233577534.rds")
amon <- c("327161259__Deborah-0HA__1283254784.rds","327161259__Bubu__588370927.rds","327161259__Cid__976053874.rds")
gful <- c("1575276297__6RV__2150849613.rds", "1575276297__0XV__1600748808.rds", "1502676405__7W2__2800792332.rds", "1502676405__6R9__2136860943.rds","533218859__Calcón__2119710070.rds", "533218859__SAIA__894556446.rds","533218859__Matal__2119711382.rds", "1591294976__AZ (6UU)__2125993736.rds")
ghim <- ("20202974__Thang Kaar Tooh (4188)__54325576.rds")
nperc <- c("316961802__Morgaez-Alius21__1672264258.rds", "316961802__Huebra__316964725.rds", "20106351__Azahar 75657__20107144.rds", "572056515__Gamla Quarry H59 Red__2886423492.rds","572056515__HBC IB5 Red__1248681303.rds")

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pthlocs <- paste0(genPath,"5.vultureIndv_mv2_1h_outlspeed_dist/")
# migratory_month <- readRDS("/home/ascharf/Documents/Projects/Drylands/UDsizeChange/migratory_month.rds")
migratory_month <- readRDS("/home/ascharf/Documents/Projects/Drylands/UDsizeChange/migratory_month_9area.rds")

indvL <- list(caur,amon,gful,ghim,nperc)
spsn <- c("Cathartes_aura","Aegypius_monachus", "Gyps_fulvus", "Gyps_himalayensis","Neophron_percnopterus")

# x <- 3
lapply(1:length(indvL), function(x){
  
  indv <- indvL[[x]]
  sn <- spsn[x]
  
  lapply(indv, function (y){
    
    pts <- readRDS(paste0(pthlocs,y))
    
    pts$monthYear <- paste0(year(mt_time(pts)),"-",sprintf("%02d",month(mt_time(pts))))
    migrMnth <- as.character(migratory_month$monthYear[migratory_month$fileName==y & migratory_month$migratoryMonth==T])
    pts$migrMnth <- F
    pts$migrMnth[pts$monthYear%in%migrMnth] <- T
    pts$month <- month(mt_time(pts))
    pts$year <- year(mt_time(pts))
    
    pts_d <- mt_filter_per_interval(pts,criterion="first",unit="day") # making plots lighter
    
    worldMap <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
    bb <- st_bbox(pts_d)
    exp <- 2
    
    ggplot() + theme_void() +
      geom_sf(data = worldMap) +
      geom_sf(data = pts_d, aes(color = migrMnth), show.legend = T) +
      coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)+
      facet_grid(year~month)+
      ggtitle(y)
    # ggsave(paste0("explr_plots/",sn,"_",y,".jpeg"),width = 40, height = 20, units = "cm")
    ggsave(paste0("explr_plots_90area/",sn,"_",y,".jpeg"),width = 40, height = 20, units = "cm")
    
  })
})