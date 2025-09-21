library(sp)
data(meuse)
VarZ <- var(meuse$zinc)
CovZ<-cov(meuse$zinc, meuse$zinc)
load("oggetti.rda")

cor(meuse$zinc, meuse$elev)
acf(meuse$zinc, main = "Zinco")



## carichiamo la libreria sf che serve per trasformare le coordinate tra
## sistemi di riferimento (CRS) differenti
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



library(sp)
library(gstat)
library(terra)
### carichiamo i dati dell'area meuse - fanno parte del pacchetto sp
data(meuse.grid)
data(meuse)
## molto importanti i passaggi sotto - impostano le variabili meuse e meuse grid,
## associate a tabelle, come dati di tipo spatial - meuse.grid come dato spatial
## a passo regolare
coordinates(meuse) = ~x+y
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE
summary(meuse.grid)
meuse.grid.terra <- terra::rast(meuse.grid)

### IDW - dal manuale:### Irast()DW - dal manuale:
### Function idw performs just as krige without a model being passed,
### but allows direct specification of the inverse distance weighting power.
### Don't use with predictors in the formula
zinc.idw = idw(zinc~1, meuse, meuse.grid)
plot(zinc.idw)



zinc.idw.terra <- terra::rast(zinc.idw)
plot(zinc.idw.terra, main="Mappa Concentrazione Zinco")
terra::crs(zinc.idw.terra) <- "EPSG:28992"
terra::writeRaster(zinc.idw.terra, "zinco.tif", overwrite=TRUE)
