library(sp)
library(tidyverse)

data(meuse)
class(meuse)
coordinates(meuse) <- c("x", "y")

#############
## contenitore vuoto con i modelli da confrontare
models <- list()
## oppure classico variogramma


models[["simple.kriging"]] <- krige(log(zinc) ~ 1, meuse, meuse.grid, v.fit,beta = 5.9)
models[["ordinary.kriging"]] <- krige(log(zinc) ~ 1, meuse, meuse.grid, v.fit)

## universal kriging
# La tendenza (drift) è stimata da variabili eserne, e.g. le coordinate x e y o altro.
#  “il fenomeno aumenta verso est / diminuisce verso nord”, oppure
#  "il fenomeno è funzione delle distanza di fiumi".
# Modello di trend polinomiale (secondo grado) in x e y
# zinc ~ x + y + x^2 + y^2 + x*y
m_uk <- gstat(id = "zn",
              formula = zinc ~ x + y + I(x^2) + I(y^2) + I(x*y),
              data = meuse)

# Variogramma sperimentale (su residui rispetto al trend stimato)
v_uk <- variogram(m_uk)
vm_uk <- fit.variogram(v_uk, vgm(psill=1, "Sph", range=500, nugget=0.1))

# Kriging universale (predizione sulla griglia)


## un tipo specifico è ", external drift kriging" - Kriging a Deriva Esterna:
# La tendenza è spiegata da un’altra variabile che tu fornisci,
# ad esempio quota, o "distanza di fiumi" ecc.
# Tipo: “so che il fenomeno cresce con la quota → usala come guida”.
f <- log(zinc) ~ sqrt(dist)
vt <- variogram(f, meuse)
vt.fit <- fit.variogram(vt, vgm(1, "Exp", 300, 1))
models[["universal.kriging"]] <- krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid, vt.fit)

#####


### eseguo un loop per applicare krige.cv a tutti e verificare quale funziona meglio
k_uk <- predict(m_uk, meuse.grid, model = vm_uk)

