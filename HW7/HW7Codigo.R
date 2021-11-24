suppressMessages(library(lattice))
suppressMessages(library(sp))
suppressMessages(library(viridisLite))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyverse))
suppressMessages(library(ggpubr))
suppressMessages(library(car))
suppressMessages(library(rstatix))
suppressMessages(library(rapportools))
suppressMessages(library(readr))
suppressMessages(library(gridExtra))


datos<- data.frame()
g <- function(x,y) {
  return(((x^2) - 8 * x - 10 * (5 * sin(x)) - (y^2) * (5 * sin(y)) ))
}

x <- seq(-5, 5, 0.25) 
y <- x
z <- outer(x, y, g)

low <- -5
high <- 5
pasos <- seq(0.25, 4, 0.25) 
repe = 1:2
replicas <- 50 #Cuantos puntitos


for (step in pasos) {
  for (reply in repe){ #hacer repeticiones del experimento
    replica <- function(t) {
      curr <- c(runif(1, low, high), runif(1, low, high))
      best <- curr
      for (tiempo in 1:t) {
        delta <- runif(1, 0, step)
        izq <- curr +c(-delta,0)
        der <- curr + c(delta,0)
        arr <- curr + c(0,-delta)
        aba <- curr + c(0,delta)
        
        coord <- c(izq, der, arr, aba)
        for(p in 1:8){
          if(coord[p] < (-2)){
            coord[p] <- coord[p]+5
          }
          if(coord[p] > 5){
            coord[p] <- coord[p]-2
          }
        }
        
        vx<-c()
        vy<-c()
        
        for(q in 1:8){
          if(q %% 2 == 0){
            vy <- c(vy,coord[q])
          }else{
            vx <- c(vx,coord[q])
          }
        }
        
        vg<- c()
        for(k in 1:4){
          vg <- c(vg, g(vx[k], vy[k]) )
        }
        
        pmax <- which.max(vg)
        curr <- c(vx[pmax], vy[pmax])
        if(g(curr[1],curr[2]) > g(best[1],best[2])){
          best <- curr
        }
      }
      return(best)
    }
    
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores(logical = FALSE) - 2))
    
    for (pot in 1:40) { #iteraciones
      tmax <- pot
      resultados <- foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax)
      
      vx<- c()
      vy<- c()
      aux<-(2*replicas)
      for(q in 1:aux){
        if(q %% 2 == 0){
          vy <- c(vy,resultados[q])
        }else{
          vx <- c(vx,resultados[q])
        }
      }
      
      
      val <- c()
      for(k in 1:replicas){
        val <- c(val, g(vx[k], vy[k]))
      }
      
      maximo <- which.max(val)
      x <- seq(-5, 5, 0.25) 
      y <-  x
      z <- outer(x, y, g)
      dimnames(z) <- list(x, y)
      d <- melt(z)
      names(d) <- c("x", "y", "z")
      
      if (reply == 1 & step == 3.5){
        png(paste0("t7_", tmax, ".png", sep=""), width=500, height=500)
        plot(levelplot(z ~ x * y, data = d, col.regions = cm.colors(100)))
        trellis.focus("panel", 1, 1, highlight=FALSE)
        lpoints(vx, vy, pch=20, col="red", cex=2) #puntos rojos
        trellis.unfocus()
        trellis.focus("panel"[1], 1, 1, highlight=FALSE) 
        lpoints(vx[maximo], vy[maximo], pch=20, col="blue",cex=3) #raya azul
        trellis.unfocus()
        
        graphics.off()
      }
    }
    ultimo<-min(val)
    datos<- rbind(datos,c(step,reply,ultimo))
  }
}
stopImplicitCluster()
names(datos) <- c("Paso", "Repeticion", "Minimo")

#GRAFICA
datos$Paso = as.factor(datos$Paso)
ggplot(datos, aes(x= Paso, y= Minimo, fill= Paso, )) + 
  geom_boxplot(fill = cm.colors(16))+
  labs(x = "Paso", y = "Valor minimo")

#Estadisitica - 
#con p menor a 0.05 se rechaza hipotesis nula H0
#H0: los datos proceden de una distribucion normal
#H1: los datos no proceden de una distribucion normal
tapply(datos$Minimo, datos$Paso, shapiro.test) #Shapiro

datos%>% #Datos individuales
  group_by(Paso) %>%
  summarise(
    
    promedio = mean(Minimo, na.rm = TRUE),
    desviacion_std = sd(Minimo, na.rm = TRUE),
    varianza = sd(Minimo, na.rm = TRUE)^2,
    mediana = median(Minimo, na.rm = TRUE),
    rango_intercuartil = IQR(Minimo, na.rm = TRUE)
  )

kruskal.test(Minimo ~ Paso, data = datos) #Kruskal
pairwise.wilcox.test(datos$Minimo, datos$Paso) #Wilcox

