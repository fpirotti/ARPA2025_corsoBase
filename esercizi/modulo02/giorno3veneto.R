library(sp)
library(gstat)
library(tidyverse)
library(terra)
library(sf)

# RIPASSO ---
## CARICO DATI VENETO -----
veneto <- read.csv("dati/venetoDatiMeteo.csv", sep = ";",dec = "," )
## elimino stazioni che non hanno registrato pioggia nell'anno (impossibile)
veneto <- veneto |> filter(tot.precipitation.mm != 0) |> dplyr::filter(mean.dailyMeanTemp != 0)
## coordinate in Roma40 (Gauss Boaga) - codice EPSG 3003
coordinates(veneto) <- c("GBEST", "GBNORD")
## non obbligatorio ma consigliato: inserisco il Sist. di Rif.
sp::proj4string(veneto) <- CRS("epsg:3003")
## leggo il dato vettoriale confini veneto
confini <- terra::vect("dati/veneto.gpkg")
## vediamo una possibile co-variata: la quota
## la prendiamo dal DEM del veneto
## uso le coordinate "casted" da oggetto di classe sp::spatialPoints DataFrame  a classe terra::vect
pt<-terra::vect(veneto)
quote <- terra::rast("dati/dtm_veneto3003.tif") |> terra::extract(pt)
veneto@data$quota <- quote$dtm_veneto3003
plot(veneto@data$quota, veneto@data$tot.precipitation.mm)
cor(veneto@data[,4:10])
## chiaro outlier ... rimuovo - nota: prova a vedere risultati kriging con e senza questo outlier

soglia<-median(veneto@data$tot.precipitation.mm)-3*sd(veneto@data$tot.precipitation.mm)
veneto <- veneto[veneto@data$tot.precipitation.mm > soglia, ]

#############
## contenitore vuoto con i modelli da confrontare
models <- list()

# kriging ordinario, usa solo la variabile dipendente
t.vgm <- variogram(tot.precipitation.mm~1, data=veneto)
plot(t.vgm)
t.fit<-fit.variogram(t.vgm, vgm("Sph"));
plot(t.vgm, t.fit)
models[["OK"]] <- t.fit

# kriging universale, usa solo la variabile dipendente
v <- variogram(tot.precipitation.mm ~ GBEST+GBNORD, veneto)
plot(v)
v.fit <- fit.variogram(v, vgm(80000, "Sph", 40000, 1))
plot(v, v.fit)
models[["UK"]] <- v.fit
#####

v.dir <- variogram(tot.precipitation.mm ~ GBEST+GBNORD, veneto, alpha = (0:3) * 45)
plot(v.dir)

## variogramma dove indichiamo che in direzione 45°
v.anis <- vgm(80000, "Sph", 40000, 1, anis = c(80, 0.9))
plot(v.dir, v.anis)
models[["UKanis"]] <- v.anis

## NB possiamo variare il passo nel variogramma e la distanza max, ma dipende da quanta densità abbiamo di campionamento
plot(variogram(tot.precipitation.mm ~ GBEST+GBNORD,
               veneto, map = TRUE,
               cutoff = 120000, width = 10000))




venetovect<- terra::vect("dati/veneto.gpkg")
# plot(venetovect)
rveneto<- terra::rast(venetovect, resolution=500)
rveneto <- terra::rasterize(  venetovect, rveneto )

plot(rveneto)

library(stars)

# terra::crs(rveneto)<-NA

rveneto.stars <- stars::st_as_stars(rveneto)

proj4string(veneto)<-CRS("EPSG:3003")

t.krige <- krige(tot.precipitation.mm~1, locations=veneto,
                 newdata=rveneto.stars, model=t.fit)

class(t.krige)
plot(t.krige)



t.vgm <- variogram(tot.precipitation.mm~ GBEST+GBNORD, data=veneto)

t.fit<-fit.variogram(t.vgm, vgm("Sph"));

t.krige <- krige(tot.precipitation.mm~GBEST+GBNORD, locations=veneto,
                 newdata=rveneto.stars, model=t.fit)

t.krige.terra <- terra::rast(t.krige)
plot(t.krige.terra)

