suppressMessages(library(ggplot2))
datos = data.frame()
masas = seq(1, 3, 0.5)
repeticiones = 1:5
n <- 25

for (masa in masas){
  for ( replica in repeticiones){
   
    p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n), vel = numeric(n))
    xmax <- max(p$x)
    xmin <- min(p$x)
    p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
    ymax <- max(p$y)
    ymin <- min(p$y)
    p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien son de 0 a 1
    cmax <- max(p$c)
    cmin <- min(p$c)
    p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
    p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
    mmax = max(p$m) 
    mmin = min(p$m)
    p$m=masa #masa fija
    paso <- floor(256 / 10)
    niveles <- seq(0, 255, paso)
    colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
    eps <- 0.001
    G <- 0.6674 #valor supuesto para la constante gravitacional(6.674*10^(-11))
    
    fuerza <- function(i) { 
      xi <- p[i,]$x
      yi <- p[i,]$y
      ci <- p[i,]$c
      mi = p [i,]$m
      fx1 = 0
      fy1 = 0
      fx2 = 0
      fy2 = 0
      for (j in 1:n) {
        cj <- p[j,]$c
        mj = p[j,]$m
        dir <- (-1)^(1 + 1 * (ci * cj < 0)) #es atraccion o repulsion
        dx <- xi - p[j,]$x
        dy <- yi - p[j,]$y
        factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps) 
        fg <- G * ((mi * mj) / ((sqrt(dx^2 + dy^2) + eps)^2)) #fuerza gravitatoria
        fx1 = fx1 - dx * factor
        fy1 = fy1 - dy * factor
        fx2 = fx2 - dx * fg
        fy2 = fy2 - dy * fg
        fx = fx1 + fx2
        fy = fy1 + fy2
      }
      return(c(fx, fy))
    }
    
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores() - 1))
    tmax <-35
    digitos <- floor(log(tmax, 10)) + 1
    tl <- "0"
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    #Estado inicial
    png(paste("p9M_t", 0, tl, ".png", sep=""),width = 800,height = 700)
    print(ggplot(data=p, aes(x=x ,y=y, size=m, col=colores[p$g+6]))
          +geom_point(show.legend =  TRUE)+xlim(c(0,1))+ylim(c(0,1))+  
            ggtitle(paste("Estado Inicial"))
          + scale_shape_discrete(name  ="Carga")+ 
            scale_colour_discrete(name  ="Carga", labels=seq(-5,5)))
    graphics.off()
    for (iter in 1:tmax) {
      f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
      delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
      p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
      p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
      v = foreach(i = 1:n, .combine=c)%dopar% sqrt((delta * f[c(TRUE, FALSE)][i])^2 + (delta * f[c(FALSE, TRUE)][i])^2)
      p$vel = p$vel+v
      tl <- paste(iter, "", sep="")
      
      while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
      }
      
      png(paste("p9M_t", 0, tl, ".png", sep=""),width = 800,height = 700)
      print( ggplot(data=p, aes(x=x ,y=y, size=m, col=colores[p$g+6]) )
             +geom_point(show.legend =  TRUE)+xlim(c(0,1))+ylim(c(0,1))+  
               ggtitle(paste("Paso ", tl, sep=""))
             + scale_shape_discrete(name  ="Carga")+ 
               scale_colour_discrete(name  ="Carga", labels=seq(-5,5)))
      graphics.off()
    }
    stopImplicitCluster()
    datos<-rbind(datos,p)
  }
}

#Grafica
names(datos) <- c("X", "Y", "Carga", "Masa", "Velocidad", "g")
datos$Masa = as.factor(datos$Masa)
ggplot(datos, aes(x = Masa , y = Velocidad , fill= Masa)) +
  geom_boxplot()+  
  labs(x="Masa", y= "Velocidad")


#PRUEBA DE NORMALIDAD
#con p menor a 0.05 se rechaza hipotesis nula H0
#H0: los datos proceden de una distribución normal
#H1: los datos no proceden de una distribución normal
tapply(datos$Velocidad, datos$Masa, shapiro.test)

#PRUEBA ESTADISTICA
kruskal.test(Velocidad ~ Masa, data = datos)
pairwise.wilcox.test(datos$Velocidad, datos$Masa)
datos%>%
  group_by(Masa) %>%
  summarise(
    cantidad_de_participantes = n(),
    promedio = mean(Velocidad, na.rm = TRUE),
    desviacion_estandar = sd(Velocidad, na.rm = TRUE),
    varianza = sd(Velocidad, na.rm = TRUE)^2,
    mediana = median(Velocidad, na.rm = TRUE),
    rango_intercuartil =  IQR(Velocidad, na.rm = TRUE)
  )
