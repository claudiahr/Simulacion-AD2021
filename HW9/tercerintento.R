suppressMessages(library(ggplot2))
suppressMessages(library(tidyverse))

n <- 25 #cantidad de particulas
datos = data.frame()
repeticiones = 1:10

for (reply in repeticiones) {
  p <- data.frame(reply, x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n))
  xmax <- max(p$x)
  xmin <- min(p$x)
  p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
  ymax <- max(p$y)
  ymin <- min(p$y)
  p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
  cmax <- max(p$c)
  cmin <- min(p$c)
  p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
  #se agrega masa
  mmax = max(p$m)
  mmin = min(p$m)
  p$m = (p$m - mmin) / (mmax - mmin) + 0.001 #la masa no sera cero nunca.
  p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
  
  paso <- floor(256 / 10)
  niveles <- seq(0, 255, paso)
  colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
  eps <- 0.001
  
  fuerza <- function(i) { #datos de la particula i
    xi <- p[i,]$x
    yi <- p[i,]$y
    ci <- p[i,]$c
    mi = p [i,]$m #masa
    G = 0.6674 #Valor ficticio de Constante de Gravitacion Universal
    
    fx1 = 0
    fy1 = 0
    fx2 = 0
    fy2 = 0
    for (j in 1:n) {
      cj <- p[j,]$c
      mj = p[j,]$m
      dir <- (-1)^(1 + 1 * (ci * cj < 0))
      dx <- xi - p[j,]$x
      dy <- yi - p[j,]$y
      factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps) #carga entre particulas
      factor1 <- G * ((mi * mj) / ((sqrt(dx^2 + dy^2) + eps)^2)) #masa entre particulas
      
      fx1 = fx1 - dx * factor
      fy1 = fy1 - dy * factor
      fx2 = fx2 - dx * factor1
      fy2 = fy2 - dy * factor1
      
      fx = fx1 + fx2
      fy = fy1 + fy2
      
    }
    return(c(fx, fy))
  }
  
  suppressMessages(library(doParallel))
  registerDoParallel(makeCluster(detectCores() - 1))
  
  tmax <- 55 #iteraciones
  digitos <- floor(log(tmax, 10)) + 1
  tl <- "0"
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  
  #Estado inicial
  png(paste("p9_t", 0, tl, ".png", sep=""),width = 800,height = 700)
  print(ggplot(data=p, aes(x=x ,y=y, size=m, col=colores[p$g+6]))
         +geom_point(show.legend =  TRUE)+xlim(c(0,1))+ylim(c(0,1))+  
           ggtitle(paste("Estado Inicial"))
         + scale_shape_discrete(name  ="Carga")+ 
           scale_colour_discrete(name  ="Carga", labels=seq(-5,5)))
  graphics.off()
  
  p$vel=numeric(n)
  for (iter in 1:tmax) {
    f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
    delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
    p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
    p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
    v =    foreach(i = 1:n, .combine=c)%dopar% sqrt((delta * f[c(TRUE, FALSE)][i])^2 + (delta * f[c(FALSE, TRUE)][i])^2)
    p$vel=p$vel+v
    
    tl <- paste(iter, "", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    png(paste("p9_t", 0, tl, ".png", sep=""),width = 800,height = 700)
    print( ggplot(data=p, aes(x=x ,y=y, size=m, col=colores[p$g+6]) )
           +geom_point(show.legend =  TRUE)+xlim(c(0,1))+ylim(c(0,1))+  
             ggtitle(paste("Paso ", tl, sep=""))
           + scale_shape_discrete(name  ="Carga")+ 
             scale_colour_discrete(name  ="Carga", labels=seq(-5,5)))
    graphics.off()
  }
  stopImplicitCluster()
  datos = rbind(datos, p)
}
#GRAFICAS

#carga vs velocidad
ggplot(data = datos, aes(x = c, y = vel))+
  geom_point(colour= "#FC4B08", size =4)+
  geom_line(colour= "#FC4B08")+
  labs(x = "Carga de la partícula", y = " Velocidad de la particula")+
  geom_smooth(method = "lm", colour = "#000000")
#masa vs velocidad
ggplot(data = datos, aes(x = m, y = vel))+
  geom_point(colour= "#AA07B5", size = 4)+
  geom_line(colour = "#AA07B5")+
  labs(x = "Masa de la partícula", y = " Velocidad de la particula")+
  geom_smooth(method = "lm", colour = "#000000")


#PRUEBA DE NORMALIDAD
#con p menor a 0.05 se rechaza hipotesis nula H0
#H0: los datos proceden de una distribución normal
#H1: los datos no proceden de una distribución normal
tapply(datos$vel, datos$g, shapiro.test)

#PRUEBA ESTADISTICA
datos%>%
  group_by(g) %>%
  summarise(
    cantidad_de_participantes = n(),
    promedio = mean(vel, na.rm = TRUE),
    desviacion_estandar = sd(vel, na.rm = TRUE),
    varianza = sd(vel, na.rm = TRUE)^2,
    mediana = median(vel, na.rm = TRUE),
    rango_intercuartil =  IQR(vel, na.rm = TRUE)
  )

kruskal.test(vel ~ g, data = datos)
pairwise.wilcox.test(datos$vel, datos$g)

#GRAFICAS
datos$g = as.factor(datos$g)
ggplot(datos, aes(x=g , y=vel , fill= reply)) + 
  geom_boxplot(fill = "#30FF24", colour = "#008F39")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  coord_cartesian(ylim = c(0,0.8))+
  #scale_y_discrete(trans = 'log10')+
  labs(x="Carga en g", y= "Velocidad")

