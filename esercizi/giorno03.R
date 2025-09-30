library(stars)
library(terra)
library(sf)
library(gstat)
library(sp)

data(meuse.grid)
data(meuse)
## molto importanti i passaggi sotto
coordinates(meuse) = ~x+y
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE

zinc.idw = idw(locations= meuse,  formula=zinc~1, newdata=meuse.grid) 
 

meuse.grid.terra <- terra::rast(meuse.grid  )
meuse.grid.stars <- stars::st_as_stars(meuse.grid)
grid.terra <- terra::rast(meuse.grid.terra, nlyr=1)
#non funziona in quanto fornisco + bande raster
# zinc.idw.terra = idw(locations= meuse,  formula=zinc~1, newdata=stars::st_as_stars(meuse.grid.terra)) 
zinc.idw.terra = idw(locations= meuse,  formula=zinc~1, newdata=stars::st_as_stars(meuse.grid.terra[[1]])) 
plot(zinc.idw.terra)

## come mai non ritagliato? - vedi warnings
zinc.idw.terra = idw(locations= meuse,  formula=zinc~1, newdata=stars::st_as_stars(grid.terra)) 
plot(zinc.idw.terra)


stars.dims <- stars::st_dimensions(meuse.grid.stars) 
## più comprensibile terra 
meuse.grid.terra.20m.vuoto  <- meuse.grid.terra$dist 
res(meuse.grid.terra.20m.vuoto)<-20
meuse.grid.terra.20m  <-  terra::resample(meuse.grid.terra$dist, meuse.grid.terra.20m.vuoto)
  
zinc.idw.20m = idw(formula=zinc~1, locations= meuse, newdata=stars::st_as_stars(meuse.grid.terra.20m) )
plot( zinc.idw.20m )


##########  variogramma semplice
lzn.vgm = variogram(log(zinc)~1, meuse )
plot(lzn.vgm)
lm(log(zinc)~1, data=meuse)

lzn.vgm = variogram(log(zinc)~sqrt(dist), meuse )
plot(lzn.vgm)
 
modello.variogramma <- vgm(1, "Sph", 900, 1)
lzn.fit = fit.variogram(lzn.vgm, model = modello.variogramma )
plot(lzn.vgm, lzn.fit)




uk_model <- gstat(
  formula = zinc ~ x + y + I(x^2) + I(y^2) + I(x*y),
  data = meuse,
  nmax = 30 
)


v <- variogram(uk_model)
plot(v)




g <- gstat(id = "ln.zinc", formula = log(zinc)~1, data = meuse)
g <- gstat(g, id = "ln.lead", formula = log(lead)~1, data = meuse)
# examine variograms and cross variogram:
plot(variogram(g))
# enter direct variograms:
g <- gstat(g, id = "ln.zinc", model = vgm(.55, "Sph", 900, .05))
g <- gstat(g, id = "ln.lead", model = vgm(.55, "Sph", 900, .05))
# enter cross variogram:
g <- gstat(g, id = c("ln.zinc", "ln.lead"), model = vgm(.47, "Sph", 900, .03))
# examine fit:
plot(variogram(g), model = g$model, main = "models fitted by eye")




data(meuse)
coordinates(meuse) <- ~x+y

data(meuse.grid)
coordinates(meuse.grid) <- ~x+y
gridded(meuse.grid) <- TRUE




# Creo oggetto gstat con due variabili
g <- gstat(NULL, "zinc", log(zinc)~1, meuse)
g <- gstat(g, "cadmium", log(cadmium)~1, meuse)

# Variogrammi e cross-variogramma
vg <- variogram(g)

# Fit LMC con un modello sferico (va adattato manualmente)
model <- vgm(psill=1, model="Sph", range=900, nugget=0.05)
fit <- fit.lmc(vg, g, model)