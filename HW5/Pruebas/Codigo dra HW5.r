#HW5 Monte Carlo
desde <- 3
hasta <- 7
cuantos <- c(1000, 10000) # puntitos en el cuadro
numewolf = 0.048834
compara = data.frame()
repeticion = c(1:50)

for (i in cuantos) {
  for (n in repeticion) {
    f <- function(x) { return(1 / (exp(x) + exp(-x))) } # funcion que piden
    g <- function(x) { return((2 / pi) * f(x)) } # normalizado a distr
    suppressMessages(library(distr)) # paquete
    generador  <- r(AbscontDistribution(d = g)) # creamos un generador
    valores <- generador(i) # generamos valores
    montecarlo = sum(valores >= desde & valores <= hasta) # checamos
    integral <- sum(montecarlo) / i # tasa: integral para g(x)
    resultado = ((pi / 2) * integral) # integral para f(x) (renorm)
    print(resultado)
    
    suppressMessages (library(tidyverse))
    numewolf = 0.369258147
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
  }
}