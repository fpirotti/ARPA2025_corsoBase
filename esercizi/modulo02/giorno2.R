library(sp)
library(tidyverse)

## richiede di aver eseguito i comandi del giorno 1
##carico dati Veneto del meteo
veneto <- read.csv("dati/venetoDatiMeteo.csv", sep = ";",dec = "," )
## elimino stazioni che non hanno registrato pioggia nell'anno (impossibile)
veneto <- veneto |> filter(tot.precipitation.mm != 0) |> dplyr::filter(mean.dailyMeanTemp != 0)
## coordinate in Roma40 (Gauss Boaga) - codice EPSG 3003
coordinates(veneto) <- c("GBEST", "GBNORD")
# hscat(log(zinc) ~ 1, meuse, (0:9) * 100)

hscat(tot.precipitation.mm ~ 1, veneto, (1:20) * 1000)
## ma come quantificare la correlazione spaziale? Indice di Moran -
library(spdep)
nb <- dnearneigh(coordinates(veneto), 0, 100000)
lw <- nb2listw(nb, style = "W")

# Moran I
moran.test(veneto$tot.precipitation.mm , lw)
## attenzione che Moran è molto sensibile ai trend spaziali
## (dunque non-stazionarietà di 1 ordine)
## vediamo se ci sono e cerchiamo di toglierli

library(scatterplot3d)
scatplot <- scatterplot3d(
  x = meuse@coords[,1],
  y = meuse@coords[,2],
  z = meuse@data$zinc,
  main = "3D Trend Surface",
  pch = 16,
  color = "steelblue"
)

# Add regression plane
scatplot$plane3d(fit, lty = "solid", lwd = 1.5, col = "darkred")


veneto.lm <- lm(tot.precipitation.mm ~  GBEST+GBNORD  , veneto)
veneto@data$residuals <- veneto.lm$residuals
moran.test(veneto.lm$residuals , lw)
v <- variogram(residuals ~ 1, veneto)
plot(v)

# variogramma empirico dei dati senza considerare il trend?
vgm<-variogram(tot.precipitation.mm~1,data=veneto)
plot(vgm)

## (già testato in Meuse)
x <- variogram(tot.precipitation.mm~1, data=veneto, cloud=TRUE)
plot(plot(x, identify = TRUE), veneto)
plot(plot(x, digitize = TRUE), veneto)

#### VERIFICA ANISOTROPIA
#### ## nb differenza
plot(variogram(tot.precipitation.mm ~  GBEST +GBNORD, veneto, map = TRUE, cutoff = 100000, width = 10000))
