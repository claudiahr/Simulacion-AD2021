l <- 1.5 #Ancho de mi cuadrito
n <- 20 #individuos de mi población
pi <- 0.05 #Probabilidad inicial de infección
pr <- 0.02 #Probabilidad de recuperación
v <- l / 30 #Velocidad con la que se mueven mis individuos
pv = seq(0, 1, 0.1) #probabilidad de vacunados
datos = data.frame()
rep = 1:10
agentes = data.frame(x = double(), y = double(),
                     dx = double(), dy = double(),
                     estado  = character())


for (apv in pv) { #incremento en la probabilidad de vacunados
  for (arep in rep) { # replicas
    
    for (i in 1:n) {
      e <- "S"    #Suceptible
      if (runif(1) < pi) {
        e <- "I" #Infectado
      } else {
        if (runif(1) < apv){
          e = "R" #Recuperado
        }
      }
      
      agentes = rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                          dx = runif(1, -v, v), dy = runif(1, -v, v),
                                          estado = e))
    }
    levels(agentes$estado) <- c("S", "I", "R")        
    epidemia <- integer()    
    r <- 0.1 #Susana distancia :P
    tmax <- 15 #iteraciones
    digitos <- floor(log(tmax, 10)) + 1    
    
    
    for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
        a$x <- a$x + a$dx
        a$y <- a$y + a$dy
        if (a$x > l) {
          a$x <- a$x - l
        }
        if (a$y > l) {
          a$y <- a$y - l
        }
        if (a$x < 0) {
          a$x <- a$x + l
        }
        if (a$y < 0) {
          a$y <- a$y + l
        }
        agentes[i, ] <- a
      }
      aS <- agentes[agentes$estado == "S",]
      aI <- agentes[agentes$estado == "I",]
      aR <- agentes[agentes$estado == "R",]
      tl <- paste(tiempo, "", sep="")
      while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
      }
      salida <- paste("p6_t", tl, ".png", sep="")
      tiempo <- paste("Paso", tiempo)
      png(salida)
      plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
      if (dim(aS)[1] > 0) {
        points(aS$x, aS$y, pch=15, col="chartreuse2", bg="chartreuse2")
      }
      if (dim(aI)[1] > 0) {
        points(aI$x, aI$y, pch=16, col="red", bg="red")
      }
      if (dim(aR)[1] > 0) {
        points(aR$x, aR$y, pch=17, col="mediumblue", bg="mediumblue")
      }
      graphics.off()
    }
    png("p6e.png", width=600, height=300)
    plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentaje de infectados")
    graphics.off()
  }
}
