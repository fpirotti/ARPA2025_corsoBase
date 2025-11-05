library(sp)
library(tidyverse)

data(meuse)
class(meuse)

##carico dati Veneto del meteo
veneto <- read.csv("dati/venetoDatiMeteo.csv", sep = ";",dec = "," )
## elimino stazioni che non hanno registrato pioggia nell'anno (impossibile)
veneto <- veneto |> filter(tot.precipitation.mm != 0) |> dplyr::filter(mean.dailyMeanTemp != 0)
## coordinate in Roma40 (Gauss Boaga) - codice EPSG 3003
coordinates(veneto) <- c("GBEST", "GBNORD")

coordinates(meuse) <- c("x", "y")

# sp::spplot(veneto, "tot.precipitation.mm" )
# bubble(veneto, "tot.precipitation.mm", do.log = T, key.space = "bottom")

library(lattice)
xyplot(log(zinc) ~ sqrt(dist), as.data.frame(meuse))


## meuse
zn.lm <- lm(log(zinc) ~ sqrt(dist), meuse)
meuse$fitted.s <- predict(zn.lm, meuse) - mean(predict(zn.lm, meuse))
meuse$residuals <- residuals(zn.lm)
spplot(meuse, c("fitted.s", "residuals"))

## veneto
veneto.lm <- lm(tot.precipitation.mm ~  mean.dailyMeanTemp, veneto)
veneto$fitted.s <- predict(veneto.lm, veneto) # - mean(predict(veneto.lm, veneto), na.rm=T)
veneto$residuals <- residuals(veneto.lm)
spplot(veneto, c( "residuals"))


######### idw

library(gstat)
data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")

idw.out <- idw(zinc ~ 1, meuse, meuse.grid, idp = 2.5)

plot(meuse.grid)
plot(idw.out)

library(sf)
library(stars)
library(terra)

## leggo il dato vettoriale con il poligono
confini <- terra::vect("dati/veneto.gpkg")

## utilizzo il poligono come "forma" per creare una griglia a passo
## definito dall'utente (500 m)
### raster NA (vuoto) con esteensioni e risoluzione definiti dall'utente
veneto.grid <- terra::rast(confini, resolution=500)
### converto i valori delle celle da NA a 1 solo all'interno del confine
veneto.grid <- terra::rasterize(confini, veneto.grid)


veneto.grid.st <- stars::st_as_stars(veneto.grid)

sp::proj4string(veneto) <- CRS("epsg:3003")

idw.out.veneto <- idw(tot.precipitation.mm ~ 1,
                      veneto,
                      newdata=veneto.grid.st,
                      idp = 2.5)


plot(idw.out.veneto)




zn.lm <- lm(log(zinc) ~ sqrt(dist), meuse)


meuse.grid$pred <-   predict(zn.lm, meuse.grid)
meuse.grid$se.fit <- predict(zn.lm, meuse.grid, se.fit = TRUE)$se.fit

spplot(meuse.grid)

### oppure
meuse.tr2 <- krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 2)
plot(meuse.tr2)

## oppure
lm(log(zinc) ~ I(x^2) + I(y^2) + I(x * y) + x + y, meuse)


hscat(log(zinc) ~ 1, meuse, (0:9) * 100)

