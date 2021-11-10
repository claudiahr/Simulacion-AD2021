suppressMessages(library(ggplot2))    #Grafica
suppressMessages(library(car))        #Estadistica
suppressMessages(library(ggpubr))     #Estadistica
suppressMessages (library(tidyverse)) #Estadistica



datos = data.frame()
poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), 
                  coef=integer(), 
                  degree=integer())
  for (t in 1:termcount) {
    var <- sample(1:varcount, 1)
    deg <- sample(0:maxdeg, 1)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars) {
  value <- 0.0
  terms = dim(pol)[1]
  for (t in 1:terms) {
    term <- pol[t,]
    mult = term$coef
    valor = vars[term$variable]
    grado = term$degree
    value <-  value + mult * valor^grado
  }
  return(value)
}

## NUEVO!!!
domin.by <- function(target, challenger) {
  # sum sobre los TRUE/FALSE
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora (en maximizar)
  } # si no hay empeora, vemos si hay mejora
  # sum sobre los TRUE/FALSE
  return(sum(challenger > target) > 0)
}

vc <- 4 #Cuantas variables
md <- 3 #Grado maximo
tc <- 5#Cuantos terminos
funobj = c(2, 3, 4, 5) #Funciones objetivo para k(2, 3, 4, 5)
replicaa = 1:20
obj <- list()


for (k in funobj) {
  for (reply in replicaa) {
    for (i in 1:k) {
      obj[[i]] <- poli(md, vc, tc)
    }
    minim <- (runif(k) > 0.5)
    sign <- (1 + -2 * minim) # neg -> min, pos -> max
    n <- 200 # cuantas soluciones aleatorias
    sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
    val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
    for (i in 1:n) { # evaluamos las soluciones
      for (j in 1:k) { # para todos los objetivos
        val[i, j] <- eval(obj[[j]], sol[i,])
      }
    }
    mejor1 <- which.max(sign[1] * val[,1])
    mejor2 <- which.max(sign[2] * val[,2])
    cual <- c("max", "min")
    no.dom <- logical() # TRUE/FALSE segun si nadie lo domina
    dominadores = integer()
    for (i in 1:n) { # para cada asignacion
      d <- logical() # quienes le dominan (si / no)
      for (j in 1:n) { # para todos los demas
        # i es a quien le retan, j es quien esta retando
        # lo comparamos como si todo fuese max (min f = max -f)
        d <- c(d, domin.by(sign * val[i,], sign * val[j,]))
      }
      cuantos = sum(d)
      dominadores = c(dominadores, cuantos)
      no.dom <- c(no.dom, sum(d) == 0) # nadie le domina (puros FALSE)
    }
    # agarra solo los que tienen TRUE en no.dom
    frente <- subset(val, no.dom) # solamente las no dominadas
    porcentaje = (length(frente[,1])/n)*100
    resultado = c(k, reply, porcentaje)
    datos = rbind(datos, resultado)
    names(datos) = c("k", "replica", "porcentaje")
  }
}

datos$k = as.factor(datos$k)
gr = ggplot(datos, aes(x = k, y = porcentaje)) +
  geom_violin(fill = "#C1B3D7", color = "#A589C1")
gr + geom_boxplot(width = 0.1, fill = "#A5DEEE",
                  color = "black", lwd = 0.3)+
  theme(panel.background = element_rect(fill = "#FDDEEE",
                                        color = "black")) +
  labs(x = "Número de funciones objetivo",
       y = "Porcentaje de soluciones Pareto")



#Estadísitica prueba de normalidad - 
#con p menor a 0.05 se rechaza hipotesis nula H0
#H0: los datos proceden de una distribución normal
#H1: los datos no proceden de una distribución normal
tapply(datos$porcentaje, datos$k, shapiro.test)

#PRUEBA ESTADISTICA
datos%>%
  group_by(k) %>%
  summarise(
    cantidad_de_participantes = n(),
    promedio = mean(porcentaje, na.rm = TRUE),
    desviacion_estandar = sd(porcentaje, na.rm = TRUE),
    varianza = sd(porcentaje, na.rm = TRUE)^2,
    mediana = median(porcentaje, na.rm = TRUE),
    rango_intercuartil =  IQR(porcentaje, na.rm = TRUE)
  )

kruskal.test(porcentaje ~ k, data = datos)
pairwise.wilcox.test(datos$porcentaje, datos$k)





