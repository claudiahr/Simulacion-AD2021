library(writexl)
library(ggplot2)
datos = data.frame()
n <-  15
k <-20
#for (k in c(10, 20, 30)) {
zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
y <- rep(0, k) # igual como las coordenadas y de las semillas

for (semilla in 1:k) {
  while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
    fila <- sample(1:n, 1)
    columna <- sample(1:n, 1)
    if (zona[fila, columna] == 0) {
      zona[fila, columna] = semilla
      x[semilla] <- columna
      y[semilla] <- fila
      break
      }
    }
  }
#}
celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (zona[fila, columna] > 0) { # es una semilla
    return(zona[fila, columna])
  } else {
    cercano <- NULL # sin valor por el momento
    menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
    for (semilla in 1:k) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist < menor) {
        cercano <- semilla
        menor <- dist
      }
    }
    return(cercano)
  }
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 2))
celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
stopImplicitCluster()
voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
rotate <- function(x) t(apply(x, 2, rev))
png("p4s.png")
par(mar = c(0,0,0,0))
image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
graphics.off()
png("p4c.png")
par(mar = c(0,0,0,0))
image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
graphics.off()

limite <- n # grietas de que largo minimo queremos graficar

inicio <- function() {
  direccion <- sample(1:4, 1)
  xg <- NULL
  yg <- NULL
  if (direccion == 1) { # horiz izq -> der
    xg <- 1
    yg <- sample(1:n, 1)
  } else if (direccion == 2) { # vertical abajo -> arriba
    xg <- sample(1:n, 1)
    yg <- 1
  } else if (direccion == 3) { # horiz der -> izq
    xg <- n
    yg <- sample(1:n, 1)
  } else { # vertical arriba -> abajo
    xg <- sample(1:n, 1)
    yg <- n
  }
  return(c(xg, yg))
}

vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
for (dx in -1:1) {
  for (dy in -1:1) {
    if (dx != 0 | dy != 0) { # descartar la posicion misma
      vp <- rbind(vp, c(dx, dy))
    }
  }
}
names(vp) <- c("dx", "dy")
vc <- dim(vp)[1]

propaga <- function(replica) {
  prob <- 1 # interno a la celda (inicial)
  dificil <- 0.99 # bajada al interior
  grieta <- voronoi # marcamos la grieta en una copia
  i <- inicio() # posicion inicial al azar
  xg <- i[1]
  yg <- i[2]
  largo <- 0
  while (TRUE) { # hasta que la propagacion termine
    grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
    largo <-  largo + 1
    frontera <- numeric()
    interior <- numeric()
    for (v in 1:vc) {
      vecino <- vp[v,]
      xs <- xg + vecino$dx # columna del vecino potencial
      ys <- yg + vecino$dy # fila del vecino potencial
      if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
        if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
          if (voronoi[yg, xg] == voronoi[ys, xs]) {
            interior <- c(interior, v)
          } else { # frontera
            frontera <- c(frontera, v)
          }
        }
      }
    }
    elegido <- 0
    if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
      if (length(frontera) > 1) {
        elegido <- sample(frontera, 1)
      } else {
        elegido <- frontera # sample sirve con un solo elemento
      }
      prob <- 1 # estamos nuevamente en la frontera
    } else if (length(interior) > 0) { # no hubo frontera para propagar
      if (runif(1) < prob) { # intentamos en el interior
        if (length(interior) > 1) {
          elegido <- sample(interior, 1)
        } else {
          elegido <- interior
        }
        prob <- dificil * prob # mas dificil a la siguiente
      }
    }
    if (elegido > 0) { # si se va a propagar
      vecino <- vp[elegido,]
      xg <- xg + vecino$dx
      yg <- yg + vecino$dy
    } else {
      break # ya no se propaga
    }
  }
  if (largo >= limite) {
    png(paste("p4g_", replica, ".png", sep=""))
    par(mar = c(0,0,0,0))
    image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
    graphics.off()
  }
  return(grieta)
}
#for (r in 1:10) { # para pruebas sin paralelismo
#    propaga(r)
#}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 2))
manhattanmatriz <- foreach(r = 1:1, .combine=c) %dopar% propaga(r)
stopImplicitCluster()
manhattanmatriz
datos = rbind(datos, manhattanmatriz)
write_xlsx(datos, "midataframe3015.xlsx") #exporta a xlsx

#gr�fica
ggplot(data = stack(datositer), aes(x = ind, y = values)) +
  stat_boxplot(geom = "errorbar",
               width = 0.2) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552",
               alpha= 0.9, outlier.colour = "red")+ 
  #scale_y_continuous( name = "Distancia Manhattan mayor.")
  labs(x = "N�mero de semillas.", y = "Distancia Mahattan Mayor.")+ 
  theme(axis.line = element_line(colour = "black", size = 0.25))

#pruebas de normalidad Shapiro test
#https://stackoverflow.com/questions/21239826/using-shapiro-test-on-multiple-columns-in-a-data-frame
lshap = lapply(datositer, shapiro.test)
lshap[[1]]
lshap = lapply(datositer, shapiro.test)
lshap[[2]]
lshap = lapply(datositer, shapiro.test)
lshap[[3]]


#Prueba estad�stica Kruskal Wallis

diez = c(8, 5, 7, 4, 8, 7, 5, 3, 6, 4, 8, 8, 7, 7, 7)
veinte =c(8, 4, 7, 5, 7, 8, 7, 7, 4, 4, 7, 5, 8, 6, 8)
treinta = c(8, 5, 6, 7, 5, 7, 4, 7, 8, 8, 7, 7, 7, 5, 7)
kruskal.test(list(diez, veinte, treinta))



