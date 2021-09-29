#HW5 Monte Carlo
suppressMessages (library(tidyverse)) # paquete strings
suppressMessages (library(ggplot2)) #paquete boxplot
suppressMessages(library(distr)) # paquete montecarlo
suppressMessages(library(car)) # paquete 
suppressMessages(library(ggpubr)) # paquete 

desde <- 3
hasta <- 7
cuantos <- c(5000, 50000, 500000, 5000000, 50000000) # puntitos en el cuadro
numewolf = 0.048834
compara = data.frame()
repeticion = c(1:50)

for (i in cuantos) {
  for (n in repeticion) {
    f <- function(x) { return(1 / (exp(x) + exp(-x))) } # funcion que piden
    g <- function(x) { return((2 / pi) * f(x)) } # normalizado a distr
    generador  <- r(AbscontDistribution(d = g)) # creamos un generador
    valores <- generador(i) # generamos valores
    montecarlo = sum(valores >= desde & valores <= hasta) # checamos
    integral <- sum(montecarlo) / i # tasa: integral para g(x)
    resultado = ((pi / 2) * integral) # integral para f(x) (renorm)
    print(resultado)
    #String
    numerock  = resultado
    as.character(numewolf)
    as.character(numerock)
    posicion = 3
    str_sub(as.character(numewolf), 3, posicion)
    str_sub(as.character(numerock), 3, posicion)
    while (str_sub(as.character(numewolf), 3, posicion) == 
           str_sub(as.character(numerock), 3, posicion)) {
      print(posicion-2)
      posicion = posicion + 1
    }
    datos = c(i, posicion-3)
    compara = (rbind(compara, datos))
    names(compara) = c("Cantidad", "DecimalesCorrectos")
  }
}
#Estadistica Shapiro Wilk
compara
str(compara)
names(compara)
shapiro.test(compara$DecimalesCorrectos)


#Estadistica Kruskal Wallis
compara%>%
  group_by(Cantidad) %>%
  summarise(
    #  cantidad_puntos = a(),
    promedio = mean(DecimalesCorrectos, na.rm = TRUE),
    desviacion_std = sd(DecimalesCorrectos, na.rm = TRUE),
    varianza = sd(DecimalesCorrectos, na.rm = TRUE)^2,
    mediana = median(DecimalesCorrectos, na.rm = TRUE),
    rango_intercuartil = IQR(DecimalesCorrectos, na.rm = TRUE)
  )

kruskal.test(DecimalesCorrectos ~ Cantidad, data = compara)
pairwise.wilcox.test(compara$DecimalesCorrectos, compara$Cantidad) #diferencias entre grupos

# Boxplot
compara$Cantidad = as.factor(compara$Cantidad)
ggplot(compara, aes(x=Cantidad, y=DecimalesCorrectos)) +
  geom_boxplot(fill = "#efa9dd", color = "#cf30a6", alpha = 0.6) +
  labs(x="Puntos", y="Decimales Correctos") 

print(shapiro.test(compara$DecimalesCorrectos))
print(kruskal.test(DecimalesCorrectos ~ Cantidad, data = compara))
