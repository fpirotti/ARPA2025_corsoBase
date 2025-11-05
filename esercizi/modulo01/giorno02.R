library(stars)
library(terra)
library(sf)
library(sp)
library(gstat)

### carichiamo i dati  dal pacchetto sp
data(meuse.grid)
data(meuse)
coordinates(meuse) = ~x+y
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE


### input IDW: meuse sono i dati (locations) e newdata sono le coordinate dove stimare il valore di concentrazione di zinco
zinc.idw = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid)
## trasformo in oggetto "terra" funziona?
meuse.grid.terra <- terra::rast(meuse.grid)
zinc.idw.terra = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid.terra)
## trasformo in oggetto "stars" funziona?
meuse.grid.stars <- stars::st_as_stars(meuse.grid)
zinc.idw.stars = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid.stars)

## trasformo in oggetto "vector" funziona?
meuse.grid.terra.vect <-  as.points(meuse.grid.terra)  # coordinate
zinc.idw.punto.vect = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid.terra.vect)
## trasformo in oggetto "sf" funziona?
meuse.grid.sf <-  sf::st_as_sf(meuse.grid.terra.vect)
zinc.idw.punto.vect = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid.sf)

## funziona solo con oggetti classe Spatial, sf o stars  (vedi  manuale)


## prendiamo una coordinata, la trasformiamo in sf e la usiamo per stimare conc. zinco
## per questo però dobbiamo usare la libreria terra!
## NB disegno covariata "dist" nel raster
plot(meuse.grid.terra$dist)
plot(terra::vect(meuse), "zinc")
p <- click(meuse.grid.terra, n=1, xy=TRUE)
print(p)
p_sf <- sf::st_as_sf(p, coords = c("x", "y"), crs = crs(meuse.grid.terra))
zinc.idw.punto = idw(formula=zinc~1, locations= meuse, newdata=p_sf)
print(zinc.idw.punto)

## tutto in uno (|> = operatore disponibile nel pacchetto R base dalla versione
## R 4.1.0  simile a %>% ma più semplice da scrivere)
## NB - la funzione idw definisce i primi due argomenti, dunque quello fornito
## da |> diventa quello successivo, ovvero "newdata"
##  modificate pure il vaore di "n" per raccogliere più punti!
##

## ============ EXTRA -
plot(zinc~1, data= meuse)
zinc.idw = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid)
spplot(zinc.idw["var1.pred"], main = "IDW Zinco")
## covariata distanza  - creo modello lineare
lm_fit <- lm(log(zinc) ~ sqrt(dist), data = meuse)
meuse$resid <- residuals(lm_fit)
## IDW dei residui
idw_res <- idw(resid ~ 1, meuse, meuse.grid)
## stimo dal modello lineare la concentrazione zinco
meuse.grid$trend <- predict(lm_fit, newdata = as.data.frame(meuse.grid))
## aggiungo i residui stimati
meuse.grid$logzinc_idwreg <- meuse.grid$trend + idw_res$var1.pred
meuse.grid$zinc_idwreg <- exp(meuse.grid$logzinc_idwreg)

spplot(meuse.grid["zinc_idwreg"], main = "IDW Zinco con covariata distanza")


