library(sp)
library(gstat)

data("meuse")
coordinates(meuse) <- c("x", "y")

data(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")

kriging <- list()

##  OK  ------
f <- log(zinc) ~ 1
v <- variogram(f, meuse)
v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))
kriging[["OK"]] <- gstat(formula = f, data=meuse, model=v.fit)

## OK con anisotropia ------
v.dir <- variogram(f, meuse, alpha = (0:3) * 45)
v.anis <- vgm(0.6, "Sph", 1600, 0.05, anis = c(45, 0.4))
plot(v.dir, v.anis)
kriging[["OK.anis"]] <-  gstat(NULL, "logZn", formula = f, data=meuse, model=v.anis)

## UK con X e Y ------
f <- log(zinc) ~ poly(x,y, degree = 2)
vt <- variogram(f, meuse)
vt.fit <- fit.variogram(vt, vgm(1, "Exp", 300, 1))
plot(vt, vt.fit)
kriging[["UK.poly2degxy"]] <-  gstat(NULL, "logZn.polyXY", formula = f, data=meuse, model=vt.fit)

## UK con dist ------
f <- log(zinc) ~ sqrt(dist)
vt <- variogram(f, meuse)
vt.fit <- fit.variogram(vt, vgm(1, "Exp", 300, 1))
plot(vt, vt.fit)
kriging[["UK.dist"]]<-  gstat(NULL, "logZn.sqrtD", formula = f, data=meuse, model=vt.fit)

## CO-KRIGING ----
g <- gstat(NULL, "logCd", log(cadmium) ~ 1, meuse)
g <- gstat(g, "logCu", log(copper) ~ 1, meuse)
g <- gstat(g, "logPb", log(lead) ~ 1, meuse)
g <- gstat(g, "logZn", log(zinc) ~ 1, meuse)

vm <- variogram(g)
vm.fit <- fit.lmc(vm, g, vgm(1, "Sph", 800, 1))
plot(vm, vm.fit)
kriging[["COK"]] <- g


## Collocated COK -----
#Crea un oggetto gstat per log(zinc) nel dataset meuse.
g.cc <- gstat(NULL, "logZn.Colloc",  log(zinc) ~ 1, meuse, model = v.fit)
# v.fit è il modello di variogramma per log(zinc) usato prima.
# ~1 indica kriging ordinario / media costante.
# A questo punto hai un modello geostatistico univariato
meuse.grid$distn <- meuse.grid$dist - mean(meuse.grid$dist) + mean(log(meuse$zinc))
# Crea una nuova variabile distn sulla griglia di predizione.
# È centrata e scalata per avere la stessa media di log(zinc).
# L’idea: usare questa variabile come secondaria nel cokriging

########  # Adeguamento del variogramma per distn
vd.fit <- v.fit
# Copia il variogramma originale v.fit.
# Modifica il sill (psill) proporzionalmente al rapporto tra varianze della nuova variabile e dello zinco.
# Questo genera un variogramma sintetico per distn coerente con log(zinc).
vov <- var(meuse.grid$distn)/var(log(meuse$zinc))
vd.fit$psill <- v.fit$psill * vov

# Aggiunge distn come nuova variabile gstat
g.cc <- gstat(g.cc, "distn", distn ~ 1, meuse.grid, nmax = 1, model = vd.fit, merge = c("logZn.Colloc", "distn"))
# nmax = 1 significa considerare solo il vicino più prossimo per la variabile secondaria
# merge = c("logZn.Colloc", "distn") combina le strutture dati per il cokriging

####### Variogramma incrociato per il cokriging
vx.fit <- v.fit
vx.fit$psill <- sqrt(v.fit$psill * vd.fit$psill) * cor(meuse$dist, log(meuse$zinc))
g.cc <- gstat(g.cc, c("logZn.Colloc", "distn"), model = vx.fit)
kriging[["COK.coll"]] <- g.cc

## BLOCK KRIGING ----- viene applicato quando si va ad applicare il modello!
#  predict(kriging[["OK"]], meuse.grid, block = c(30,30))

## Kriging variabile discreta -----
v <- variogram(log(zinc) ~ soil, meuse)
v.fit <- fit.variogram(v, vgm(0.6, "Sph", 900, 0.1))
# plot(v, v.fit)
# Universal kriging con soil come covariata
kriging[["UK.cat"]] <- gstat(NULL, "logZn.soil", formula = log(zinc) ~ soil, data = meuse, model = v.fit)


gstat2predict <- function(ogg.gstat){

  # r <- predict(g, meuse.grid)
  # browser()
  ## attenzione in co-kriging prende solo la prima variabile! ----
  ## uso 5 folds ... i dati vengono divisi in 5 gruppi (“fold”).
  ## 1 viene usato per validazione e i 4 rimanenti per aggiustare
  cv <- tryCatch({
    gstat.cv(ogg.gstat, nfold = 5 )
    ## sostituite la riga sopra con quella sotto per fare "block kriging"
    # gstat.cv(ogg.gstat, nfold = 5, block=c(50,50) )
  }, error=function(e){
    browser()
    message(e)
    NULL
  })

  if(is.null(cv)){
    browser()
    message("errore")
    return(NULL)
  }
  ### qui
  # plot(cv@data$observed, cv@data$var1.pred,
  #      xlab="Osservati", ylab="Stimati")
  ME <- mean(cv$residual)
  RMSE <- sqrt(mean(cv$residual^2)) ### attenzione, NON sd(cv$residual)

  # Residui standardizzati
  MSDR <- mean(cv$zscore^2)

  r <- data.frame(ME, RMSE, MSDR)
  return(r)
}

## applico la funzione sopra ad ogni oggetto gstat
predizioni <- lapply(kriging,   gstat2predict)

validazione <- data.table::rbindlist(predizioni,idcol = "tipo")
View(validazione)




