library(stars)
library(terra)
library(sf)
library(sp)
### input IDW: meuse sono i dati (locations) e newdata sono le coordinate dove stimare il valore di concentrazione di zinco
zinc.idw = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid)
## trasformo in oggetto "terra" funziona?
meuse.grid.terra <- terra::rast(meuse.grid)
zinc.idw.terra = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid.terra)
## trasformo in oggetto "stars" funziona?
meuse.grid.stars <- stars::st_as_stars(meuse.grid)
zinc.idw.stars = idw(formula=zinc~1, locations= meuse, newdata=meuse.grid.stars)
plot(zinc.idw.stars)
print(zinc.idw.stars)

## trasformo in oggetto "sf" funziona?
grid_df <- as.points(meuse.grid.terra)  # coordinate
## funziona solo con oggetti classe Spatial, sf o stars  (vedi  manuale)
zinc.idw.punto = idw(formula=zinc~1, locations= meuse, newdata=grid_df)
# plot(zinc.idw)

## prendiamo una coordinata, la trasformiamo in sf e la usiamo per stimare conc. zinco
## per questo però dobbiamo usare la libreria terra!
## NB
plot(meuse.grid.terra$dist)
p <- click(meuse.grid.terra, n=1, xy=TRUE, cell=TRUE)
print(p)
p_sf <- sf::st_as_sf(p, coords = c("x", "y"), crs = crs(meuse.grid.terra))
zinc.idw.punto = idw(formula=zinc~1, locations= meuse, newdata=p_sf)
print(zinc.idw.punto)

## tutto in uno (|> = operatore disponibile nel pacchetto R base dalla versione
## R 4.1.0  simile a %>% ma più semplice da scrivere)
## NB - la funzione idw definisce i primi due argomenti, dunque quello fornito
## da |> diventa quello successivo, ovvero "newdata"
##  modificate pure il vaore di "n" per raccogliere più punti!
click(meuse.grid.terra, n=1, xy=TRUE, cell=TRUE) |>
      sf::st_as_sf( coords = c("x", "y"), crs = crs(meuse.grid.terra)) |>
      idw(formula=zinc~1, locations= meuse)
