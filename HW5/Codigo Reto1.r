#Reto 1
suppressMessages (library(tidyverse)) # paquete strings
suppressMessages (library(ggplot2)) #paquete boxplot
suppressMessages(library(distr)) # paquete montecarlo
suppressMessages(library(car)) # paquete 
suppressMessages(library(ggpubr)) # paquete 
muestra = c(10000, 50000, 100000) # puntitos en el cuadro
truepi = 3.141592 
datos  = data.frame() 

#Genera pi
for (muchos in muestra) {
  for (replica in 1:20){
    interior = 0
    for (r in 1:muchos) {
      x = runif(1, -1, 1)
      y = runif(1, -1, 1)
      d = sqrt(x*x + y*y)
      if (d < 1) {
        interior = interior + 1
      }
    }
    
    tasa = interior / muchos
    pi = 4 * tasa
    print(pi)
    #string
    resultado = pi
    at = as.character(truepi)
    ar = as.character(resultado)
    posicion = 3
    
    while (str_sub(at, 3, posicion) ==
           str_sub(ar, 3, posicion)) {
      print(posicion-2)
      posicion = posicion +1
    }
    data = c(muchos, posicion-3)
    datos = rbind(datos, data)
    names(datos) = c("Muestra", "DeCorrect")
  }
}

#Estadistica Shapiro Wilk
datos
str(datos)
names(datos)
shapiro.test(datos$DeCorrect)

#Estadistica Kruskal Wallis
datos%>%
  group_by(Muestra) %>%
  summarise(
    promedio = mean(DeCorrect, na.rm = TRUE),
    desviacion_std = sd(DeCorrect, na.rm = TRUE),
    varianza = sd(DeCorrect, na.rm = TRUE)^2,
    mediana = median(DeCorrect, na.rm = TRUE),
    rango_intercuartil = IQR(DecCorrect, na.rm = TRUE)
  )

kruskal.test(DeCorrect ~ Muestra, data = datos)
pairwise.wilcox.test(datos$DeCorrect, datos$Muestra) #diferencias entre grupos


# Boxplot
datos$Muestra = as.factor(datos$Muestra)
ggplot(datos, aes(x=Muestra, y=DeCorrect)) +
  geom_boxplot(fill = "#a85cd6", color = "#581f7a", alpha = 0.6) +
  labs(x="Muestra", y="Decimales Correctos")

#Resumen
print(shapiro.test(datos$DeCorrect))
print(kruskal.test(DeCorrect ~ Muestra, data = datos))

