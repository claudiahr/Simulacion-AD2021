#HW6 - Sistema multiagente
suppressMessages(library(car)) # paquete 
suppressMessages(library(ggpubr)) # paquete 
suppressMessages (library(tidyverse))

l <- 1.5 #Ancho de mi cuadrito
n <- 25 #individuos de mi población
pi <- 0.05 #Probabilidad inicial de infección
pr <- 0.02 #Probabilidad de recuperación
v <- l / 30 #Velocidad con la que se mueven mis individuos
pv = seq(0, 0.9, 0.1) #probabilidad de vacunados
rep = (1:15)
datos = data.frame()
xxx= data.frame()


for (vac in pv) { #incremento en la probabilidad de vacunados
  for (arep in rep) { # replicas
    
    agentes = data.frame(x = double(), y = double(),
                         dx = double(), dy = double(),
                         estado  = character())
    
    for (i in 1:n) {
      e <- "S"    #Suceptible
      if (runif(1) < pi){
        e = "I" #Infectado
      } else {
        if (runif(1) < vac) {
          e <- "R" #Recuperado
        }
      }
      
      agentes = rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                          dx = runif(1, -v, v), dy = runif(1, -v, v),
                                          estado = e))
      levels(agentes$estado) <- c("S", "I", "R") 
    }
    
    epidemia <- integer()    
    r <- 0.1 #Susana distancia :P
    tmax <- 50 #iteraciones cantidad de imagenes que obtengo
    digitos <- floor(log(tmax, 10)) + 1    
    maxinf = 0
    
    for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      
      if (infectados == 0) {
        iteracion = tiempo
        break
      }
      if(max(epidemia) > maxinf){
        maxinf = max(epidemia)
        iteracion = tiempo
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
                  p <- (r - d) / r #probabilidad de contagio
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
    }
    porcentaje<-(maxinf/n)*100
    datos <- rbind(datos,c(vac, maxinf, porcentaje, iteracion)) #data frame 
    names(datos)<-c("probabilidad", "maxinf", "porcinf","tiempo")
    xxx = rbind(xxx, c(vac, maxinf))
    names(xxx) = c("proba", "maxinfect")
  }
}

#Estadísitica prueba de normalidad - 
      #con p menor a 0.05 se rechaza hipotesis nula H0
      #H0: los datos proceden de una distribución normal
      #H1: los datos no proceden de una distribución normal
tapply(datos$maxinf, datos$probabilidad, shapiro.test)



#PRUEBA ESTADISTICA
datos%>%
  group_by(probabilidad) %>%
  summarise(
      cantidad_de_participantes = n(),
      promedio = mean(maxinf, na.rm = TRUE),
      desviacion_estandar = sd(maxinf, na.rm = TRUE),
      varianza = sd(maxinf, na.rm = TRUE)^2,
      mediana = median(maxinf, na.rm = TRUE),
      rango_intercuartil =  IQR(maxinf, na.rm = TRUE)
  )

kruskal.test(maxinf ~ probabilidad, data = datos)
pairwise.wilcox.test(datos$maxinf, datos$probabilidad)


#Grafica
datos$maxinf = as.factor(datos$maxinf)
ggplot(datos, aes(x = probabilidad, y = maxinf, fill = arep )) +
  geom_boxplot(fill = "#C3BBEC", colour = "#9D64E2")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  labs(x = "probabilidad inicial de vacunación", y = "Cantidad de personas infectadas")

boxplot(datos$maxinf ~ datos$probabilidad, col = "#C3BBEC", 
        xlab = "Probabilidad inicial de vacunación", 
        ylab = "Cantidad de personas infectadas", 
        border = "#9D64E2")

boxplot(datos$tiempo ~ datos$probabilidad, col = "#C3EFFC", 
        xlab = "Probabilidad inicial de vacunación", 
        ylab = "Momento en que se dió el mayor número de contagios", 
        border = "#3CBEFC")






