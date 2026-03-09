library(move2)
library(terra)
library(sf)

# "327161259_Deborah-0HA.rds" # es/fr
# "327161259_Bubu.rds" # es
# "327161259_Cid.rds" #eu/afri
# "327161259_Fuelle.rds" #eu/afri

moveObj <- readRDS(paste0( "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/5.vultureIndv_mv2_1h_outlspeed_dist/", "327161259_Fuelle.rds"))
plot(moveObj,max.plot=1)

crds <- st_coordinates(moveObj)

r <- rast(ext(crds), nrows=6, ncols=6)
count_raster <- rasterize(crds, r, fun="count")
plot(count_raster)
hist(values(count_raster), breaks="FD")
vls <- values(count_raster)
hist(vls[vls>10])
