#______________________________________________________________________________________________
# Kriging and Co-Kriging Introduction   Professor Francesco Pirotti  UniPD   May 2019
# outline of coding provided by Professors G.Biging UC Berkeley & J.Hernandez Univ of Chile
#______________________________________________________________________________________________

#read shapefile

library(sp); library(gstat);
library(dplyr); library(terra);

##carico dati Veneto del meteo
veneto <- read.csv("dati/venetoDatiMeteo.csv", sep = ";",dec = "," )
## elimino stazioni che non hanno registrato pioggia nell'anno (impossibile)
veneto <- veneto |> filter(tot.precipitation.mm != 0) |> dplyr::filter(mean.dailyMeanTemp != 0)
## coordinate in Roma40 (Gauss Boaga) - codice EPSG 3003
coordinates(veneto) <- c("GBEST", "GBNORD")
# VARIOGRAM
library(gstat)
# train.n<-sample(1:nrow(stz.data@data), nrow(stz.data@data)*0.93)
# training<-stz.data[ train.n, ]
# training@data[ training@data==0]<-NA
# training.df<-na.omit(training@data)
# ## removing zeroes
# training<-training[ rownames(training.df), ]


library(scatterplot3d)
scatplot<-scatterplot3d( training@coords[,1], training@coords[,2],
                         training@data$tot.precipitation.mm, main="3D Scatterplot")
fit <- lm(training@data$tot.precipitation.mm ~ training@coords[,1]+ training@coords[,2]) # FIT A PLANE THROUGH THE DATA
scatplot$plane3d(fit)

#variogram cloud
x <- variogram(tot.precipitation.mm~1, data=training, cloud=TRUE)
plot(plot(x, identify = TRUE), training)
plot(plot(x, digitize = TRUE), training)

#plot empirical variogram
t.vgm<-variogram(tot.precipitation.mm~1,data=training)
#usingt t.vgm for training variogram
plot(t.vgm)
#fit variogram model to the data.
# you could use:  Spherical "Sph", exponential "Exp" or Gaussian "Gau"; try them to find best one
t.fit<-fit.variogram(t.vgm, vgm("Sph"));
t.fit        # use when don't have estimate of range, sill

#t.fit<-fit.variogram(t.vgm, vgm(1,"Sph",60000,3.5))  # use when you have estimate of range, sill
t.fit<-fit.variogram(t.vgm, vgm(100,"Sph",88850, 57251))  # use when you have estimate of range, sill
plot(t.vgm, t.fit)  #got a nice fit

# look at the some directional variograms
#specify the directions to look at, in this case 0, 45, 90 and 135 degrees
# width of bins,  try 5000 or 1000 as an example.  As bin gets wider the
# variograms should become more smooth, but if they get too large you
#  have no real variogram



t.dvg1<-variogram(tot.precipitation.mm~training$LON+training$LAT, training,
                  width=5000, alpha=c(0,45,90,135))
plot(t.dvg1)

t.dvg2<-variogram(tot.precipitation.mm~training$LON+training$LAT, training,
                  width=10000, alpha=c(0,45,90,135))
plot(t.dvg2)

t.vgm3<-variogram(tot.precipitation.mm~1,data=training,map=TRUE,cutoff=80000,width=8000)
#cutoff approx = range,  width = width of bins
plot(t.vgm3)

tt<-terra::rast(t.vgm3$map)
plot(tt)

plot(t.vgm3)
#In two dimensions, two parameters define an anisotropy ellipse, say anis = c(30, 0.5). The
#first parameter, 30, refers to the main axis direction: it is the angle for the principal direction of
#continuity (measured in degrees, clockwise from positive Y, i.e. North). The second parameter,
#0.5, is the anisotropy ratio, the ratio of the minor range to the major range (a value between 0 and 1)
# In our example the angle =0,  the range in the 0 direction ~ 60000 and in the 90 deg. direction it is ~ 40000
# so the ratio = 2/3, we do see a NE-SW directional effect (the major axis of an ellipse)

# look at directional variograms at 0,45,90 and 135 degrees (could use other degrees)
dir.vgm<-variogram(tot.precipitation.mm~veneto@coords, veneto,
                   width=10000, alpha=c(0,45,90,135))
plot(dir.vgm)

dir.vgm<-variogram(tot.precipitation.mm~veneto@coords, veneto,
                   width=10000, alpha=c(0,45,90,135))
plot(dir.vgm)


# Create a "gstat" object
train.gstat <- gstat(id="veneto", formula=tot.precipitation.mm~ 1, data=veneto)
# Create directional variograms at 0, 45, 90, 135 degrees from north (y-axis)
dir.vgm.train <- variogram(train.gstat, alpha=c(0,45,90,135),width=5000)
# Create a new model
dir.model=vgm(model='Gau', anis=c(45, 0.67))

# Fit Anisotropic model to variogram model to the variogram
dir.model.fit <- fit.variogram(dir.vgm.train, model=dir.model)
## plot results:
plot(dir.vgm.train, model=dir.model.fit, as.table=TRUE)

# If we had more time we would do a better job with fitting the anisotropic variogram
# we might need to detrend the data to get the variogram in 0 and 135 direction
# to reach an asymptote

# For this example we will continue with using the
# Isotropic model which assumes that the variogram is the same in all directions.
# so let's fit an Isotropic Spherical variogram model
t.fitIso<-fit.variogram(t.vgm,vgm("Sph"));
t.fitIso        # use when don't have estimate of range, sill
plot(t.vgm, t.fitIso)

# KRIGING
library(sp)
library(raster)
library(rgeos)
library(dismo)

# plot of data coordinates in 2D
coordinates(training)
plot(training$LON,training$LAT)

#_____________________________DO KRIGING__________________  # needs the gridded polygon
#vgm(psill = NA, model, range = NA, nugget, add.to, anis,
m <- vgm(3.57, "Sph", 66600, 0.281)

proj4string(training)<-CRS("EPSG:3003")

venetovect<- terra::vect("dati/veneto.gpkg")
rveneto<- terra::rast(venetovect, resolution=500)
rveneto <- terra::rasterize(  venetovect,rveneto )
plot(rveneto)

library(stars)

rveneto.stars <- stars::st_as_stars(rveneto)
t.krige<-krige(tot.precipitation.mm~1, locations=training, newdata=rveneto.stars, model=t.fitIso)

terra::plot(t.krige)
terra::contour(t.krige, add=TRUE, drawlabels=TRUE, col='brown')
## non funziona bene in quanto oggetto STARS disegnato con funzioni TERRA
t.krige.terra <- terra::rast(t.krige)
terra::plot(t.krige.terra[[1]], main="Kriging tot.precipitation.mm")
terra::contour(t.krige.terra, add=TRUE, drawlabels=TRUE, col='brown')



points(training, pch=4, cex=0.5, col="white")


#_____________________________DO COKRIGING_________________
# I don't know which variable you want to use
# for co-kriging
cor(as.data.frame(veneto))
t.cok<-gstat(NULL, "VALTMX2", tot.precipitation.mm~1, training)
# experimental variogram
t.var<-variogram(t.cok);
plot(t.var)
# at this point I don't know what the starting values for
# the paramters in t.fit below should be
t.fit<-fit.lmc(t.var,t.cok,vgm(1,"Sph",66000,1))

# COKRIGING
t.cokrige<-predict(t.fit,  rveneto.stars)
image(t.cokrige, col=terrain.colors(20))
contour(t.krige, add=TRUE, drawlabels=TRUE, col='brown')

title('Cokriging temperature in Italy')
terra::writeRaster(t.cokrige, "bcokrige.tif", overwrite=T)

#_____________________________DO kriging VALIDATION_______
t_krige.cv<-gstat.cv(t.fit, nfold=nrow(training))
#nfold=nrow(b) indicates one-at-the-time cross-validation.
mean(t_krige.cv$residual)  #bias
sqrt(mean(t_krige.cv$residual^2))  #RMSE
#_____________________________DO cokriging VALIDATION______
t_cokrige.cv<-gstat.cv(t.cok, nfold=nrow(training))
mean(t_cokrige.cv$residual)  #bias
sqrt(mean(t_cokrige.cv$residual^2))  #RMSE
#______________________________FIN__________________________

setwd(wd)
