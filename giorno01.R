library(sp)
data(meuse)
VarZ <- var(meuse$zinc)
CovZ<-cov(meuse$zinc, meuse$zinc)
load("oggetti.rda")

cor(meuse$zinc, meuse$elev)
acf(meuse$zinc, main = "Zinco")




library(sf)
lat<-45.398557
long<-11.876460
# punti in classe SF
pt <- st_sfc(st_point(c(long, lat)), crs = 4326)
# UTM fuso 32
pt_utm <- st_transform(pt, 32632)
#
pt_ecef <- st_transform(pt, 4978)
pt_igm <- st_transform(pt, 6875)

# le coordinate nei vari sistemi
geographic = sf::st_coordinates(pt)
planimetric = st_coordinates(pt_utm)
geocentric = st_coordinates(pt_ecef)






library(gstat)
library(terra)
data(meuse.grid)
data(meuse)
## molto importanti i passaggi sotto
coordinates(meuse) = ~x+y
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE
###
zinc.idw = idw(zinc~1, meuse, meuse.grid)
plot(zinc.idw)

dev.off()

zinc.idw.terra <- terra::rast(zinc.idw)
plot(zinc.idw.terra, main="Mappa Concentrazione Zinco")
terra::crs(zinc.idw.terra) <- "EPSG:28992"
terra::writeRaster(zinc.idw.terra, "zinco.tif", overwrite=TRUE)
