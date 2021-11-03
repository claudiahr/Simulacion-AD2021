#Regla 2: el valor de cada objeto se genera independientemente con una distribucion exponencial y su peso es inversamente correlacionado con el valor. 
suppressMessages(library(ggplot2))
suppressMessages(library(testit))
suppressMessages(library(tcltk))
suppressMessages(library(tidyverse)) #Para estadistica
suppressMessages(library(car)) #Para estadistica
suppressMessages(library(ggpubr)) #Para estadistica

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso)
  assert(n == length(valor))
  vt <- sum(valor)
  if (pt < cap) {
    return(vt)
  } else {
    filas <- cap + 1
    cols <- n + 1
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols)
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0
    }
    rownames(tabla) <- 0:cap
    colnames(tabla) <- c(0, valor)
    for (objeto in 1:n) {
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        tabla[acum, objeto + 1] <- tabla[acum, objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

#valores independientes con distribucion exponencial
generador.valores <- function(cuantos, min, max) {
  return(round(normalizar(rexp(cuantos)) * (max - min) + min))
}

#pesos inversamente correlacionados con el valor
generador.pesos <- function(valores, min, max) {
  n <- length(valores)
  pesos <- double()
  for (i in 1:n) {
    media <- valores[i]
    desv <- runif(1, max=0.1)
    pesos <- c(pesos, rnorm(1, (1/media), desv))
  }
  pesos <- normalizar(pesos) * (max - min) + min
  return(pesos)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(round(runif(tam * n)), nrow = tam, ncol = n)
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

datos = data.frame()
cruz = c(15, 25, 35) #cruzamientos
reply = 1:3
pm = 0.2 #probabilidad de mutacion

for (rep in cruz){
  for (replica in reply){
    n <- 50 #objetos
    valores <- generador.valores(n, 10, 500)
    pesos <- generador.pesos(valores, 15, 80)
    capacidad <- round(sum(pesos) * 0.65)
    optimo <- knapsack(capacidad, pesos, valores)
    init <- 30  #soluciones
    p <- poblacion.inicial(n, init)
    tam <- dim(p)[1]
    assert(tam == init)
    mejores <- double()
    
    tiempo = 8 #segundos
    start = Sys.time()
    
    while(TRUE) {
      elapsed = as.numeric(difftime(Sys.time(), start, units = 'secs'))
      remaining = tiempo - round(elapsed) 
      Sys.sleep(0.1)
      print(remaining)
      
      p$obj <- NULL
      p$fact <- NULL
      for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
        if (runif(1) < pm) {
          p <- rbind(p, mutacion(p[i,], n))
        }
      }
      for (i in 1:rep) { #cantidad de reproducciones
        padres <- sample(1:tam, 2, replace=FALSE)
        hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
        p <- rbind(p, hijos[1:n]) # primer hijo
        p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
      }
      tam <- dim(p)[1]
      obj <- double()
      fact <- integer()
      for (i in 1:tam) {
        obj <- c(obj, objetivo(p[i,], valores))
        fact <- c(fact, factible(p[i,], pesos, capacidad))
      }
      p <- cbind(p, obj)
      p <- cbind(p, fact)
      mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
      p <- p[mantener,]
      tam <- dim(p)[1]
      assert(tam == init)
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
      
      print(paste(mejor, (optimo - mejor) / optimo))
      opt <- ((optimo - mejor) / optimo)*100
      segundos <-round(elapsed)
      
      if (remaining <= 0) break
      
      resultado = c(rep, replica, segundos, mejor, opt, optimo)
      datos = rbind(datos, resultado)
      names(datos) = c("Cruzamiento", "Replica", "Segundo", "Mejor", "%Optimo", "Optimo")
    }
  }
}

#graficas
datos$Segundo = as.factor(datos$Segundo)
data = split.data.frame(datos, f = datos$Cruzamiento)

ggplot(data$`15`, aes(x= Segundo, y= Mejor)) + 
  geom_boxplot(fill = "#F7C2D6")+
  labs(x = "Tiempo (segs)", y = "Mayor valor", title = 'Cruzamientos = 15')+
  geom_hline(aes(yintercept=Optimo), colour="#F21693", size= 1)

ggplot(data$`25`, aes(x= Segundo, y= Mejor)) + 
  geom_boxplot(fill = "#F7C2D6")+
  labs(x = "Tiempo (segs)", y = "Mayor valor", title = 'Cruzamientos = 25')+
  geom_hline(aes(yintercept=Optimo), colour="#F21693", size= 1)

ggplot(data$`35`, aes(x= Segundo, y= Mejor)) + 
  geom_boxplot(fill = "#F7C2D6")+
  labs(x = "Tiempo (segs)", y = "Mayor valor", title = 'Cruzamientos = 35')+
  geom_hline(aes(yintercept=Optimo), colour="#F21693", size= 1)

#Estadistica
#PRUEBA DE NORMALIDAD
#con p menor a 0.05 se rechaza hipotesis nula H0
#H0: los datos proceden de una distribución normal
#H1: los datos no proceden de una distribución normal
tapply(datos$Mejor, datos$Cruzamiento, shapiro.test)

#PRUEBA ESTADISTICA
datos %>%
  group_by(Cruzamiento) %>%
  summarise(
    #cantidad_de_participantes = n(),
    promedio = mean(Mejor, na.rm = TRUE),
    desviacion_estandar = sd(Mejor, na.rm = TRUE),
    varianza = sd(Mejor, na.rm = TRUE)^2,
    mediana = median(Mejor, na.rm = TRUE),
    rango_intercuartil =  IQR(Mejor, na.rm = TRUE)
  )

kruskal.test(Mejor ~ Cruzamiento, data = datos)
pairwise.wilcox.test(datos$Mejor, datos$Cruzamiento)

