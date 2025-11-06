library(sp)
library(tidyverse)

data(meuse)
class(meuse)
coordinates(meuse) <- c("x", "y")

#############
## contenitore vuoto con i modelli da confrontare
models <- list()
## oppure classico variogramma
## library(geoR)
v <- variogram(tot.precipitation.mm ~ GBEST+GBNORD, veneto)
plot(v)
v.fit <- fit.variogram(v, vgm(80000, "Sph", 40000, 1))
plot(v, v.fit)


#####

v.dir <- variogram(tot.precipitation.mm ~ GBEST+GBNORD, veneto, alpha = (0:3) * 45)
plot(v.dir)
## variogramma dove indichiamo che in direzione 45°
v.anis <- vgm(80000, "Sph", 40000, 1, anis = c(45, 0.3))
plot(v.dir, v.anis)

## NB possiamo variare il passo nel variogramma e la distanza max, ma dipende da quanta densità abbiamo di campionamento
plot(variogram(tot.precipitation.mm ~ GBEST+GBNORD,
               veneto, map = TRUE,
               cutoff = 120000, width = 10000))


#####
#plot empirical variogram
t.vgm <- variogram(tot.precipitation.mm~1, data=veneto)
plot(t.vgm)
t.fit<-fit.variogram(t.vgm, vgm("Sph"));
plot(t.vgm, t.fit)  #got a nice fit

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


######### co-kriging

cor(as.data.frame(meuse)[c("cadmium", "copper", "lead", "zinc")])

g <- gstat(NULL, "logCd", log(cadmium) ~ 1, meuse)
g <- gstat(g, "logCu", log(copper) ~ 1, meuse)
g <- gstat(g, "logPb", log(lead) ~ 1, meuse)
g <- gstat(g, "logZn", log(zinc) ~ 1, meuse)
vm <- variogram(g)
vm.fit <- fit.lmc(vm, g, vgm(1, "Sph", 800, 1))
plot(vm, vm.fit)
plot(vm)

cok.maps <- predict(vm.fit, meuse.grid)
spplot.vcov(cok.maps)

################## Collocated Cokriging


#Crea un oggetto gstat per log(zinc) nel dataset meuse.
g.cc <- gstat(NULL, "log.zinc", log(zinc) ~ 1, meuse, model = v.fit)
# v.fit è il modello di variogramma per log(zinc) usato prima.
# ~1 indica kriging ordinario / media costante.
# A questo punto hai un modello geostatistico univariato
meuse.grid$distn <- meuse.grid$dist - mean(meuse.grid$dist) + mean(log(meuse$zinc))
# Crea una nuova variabile distn sulla griglia di predizione.
# È centrata e scalata per avere la stessa media di log(zinc).
# L’idea: usare questa variabile come secondaria nel cokriging

v <- variogram(log(zinc) ~ 1, meuse)
v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))
########  # Adeguamento del variogramma per distn
vd.fit <- v.fit
plot(v,v.fit)
plot(v.fit, 2000)
# Copia il variogramma originale v.fit.
# Modifica il sill (psill) proporzionalmente al rapporto tra varianze della nuova variabile e dello zinco.
# Questo genera un variogramma sintetico per distn coerente con log(zinc).
vov <- var(meuse.grid$distn)/var(log(meuse$zinc))

vd.fit$psill <- v.fit$psill * vov

g.cc <- gstat(g.cc, "distn", distn ~ 1,
              meuse.grid, nmax = 1,
              model = vd.fit, merge = c("log.zinc", "distn"))

vx.fit <- v.fit
vx.fit$psill <- sqrt(v.fit$psill * vd.fit$psill) * cor(meuse$dist, log(meuse$zinc))
g.cc <- gstat(g.cc, c("log.zinc", "distn"), model = vx.fit)
x <- predict(g.cc, meuse.grid)


plot(vd.fit, 2000, add=T)

#################



coordinates(meuse) <- c("x", "y")

library(scatterplot3d)
# Aggiusto un modello di un piano nello spazio 2d
fit <- lm(meuse@data$zinc ~ meuse@coords[,1]+ meuse@coords[,2])

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

