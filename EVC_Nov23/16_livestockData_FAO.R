
library(terra)
library(raster)

setwd("/home/mscacco/ownCloud - mscacco@ab.mpg.de@owncloud.gwdg.de/Martina/ProgettiVari/Drylands/DryLands/sharedWithDennis/LivestockRasters_FAO")

DA_fls <- list.files("GLW4_2020_DA", full.names = T)
AW_fls <- list.files("GLW4_2020_AW", full.names = T)

buff_da <- rast(DA_fls[1])
buff_aw <- rast(AW_fls[1])

plot(buff_da)
range(values(buff_da), na.rm=T)
plot(buff_aw)
range(values(buff_aw), na.rm=T)
