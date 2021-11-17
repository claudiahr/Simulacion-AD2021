#https://machinelearningmastery.com/precision-recall-and-f-measure-for-imbalanced-classification/
#https://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
datos=data.frame()
suppressMessages(library(ggplot2)) #para graficar
suppressMessages(library(reshape2)) #para ordenar los datos
suppressMessages(library(car)) #Estadistica
suppressMessages(library(ggpubr)) #Estadistica
suppressMessages(library(tidyverse)) #Estadistica
binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

replica = 1:6 #Replicaaas
negro<-c(0.8, 0.9, 0.995) #probabilidad encender cuadritos negros
gris<-c(0.5, 0.8, 0.92) #probabilidad encender cuadritos grises
blanco<-c(0.002, 0.01, 0.5) #probabilidad encender cuadritos blancos

for(ne in negro){
  for(gr in gris){
    for(bl in blanco){
      for(reply in replica){
        modelos <- read.csv("digits.txt", sep=" ",
                            header=FALSE, 
                            stringsAsFactors=F)
        modelos[modelos=='n'] <- ne
        modelos[modelos=='g'] <- gr
        modelos[modelos=='b'] <- bl
        
        r <- 5
        c <- 3
        dim <- r * c
        
        tasa <- 0.15
        tranqui <- 0.99
        
        tope <- 9
        digitos <- 0:tope
        k <- length(digitos)
        contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
        rownames(contadores) <- 0:tope
        colnames(contadores) <- c(0:tope, NA)
        
        n <- floor(log(k-1, 2)) + 1
        neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
        
        for (t in 1:5000) { # entrenamiento
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,]
          correcto <- binario(d, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            if (deseada != resultado) {
              ajuste <- tasa * (deseada - resultado)
              tasa <- tranqui * tasa
              neuronas[i,] <- w + ajuste * pixeles
            }
          }
        }
        
        for (t in 1:300) { # prueba
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
          correcto <- binario(d, n)
          salida <- rep(FALSE, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            salida[i] <- resultado
          }
          r <- min(decimal(salida, n), k) # todos los no-existentes van al final
          contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
        }
        #F-SCORE = (2 * Precision * Recall) / (Precision + Recall)
        #PRECISION= TruePositives / (TruePositives + FalsePositives)
        #RECALL= TruePositives / (TruePositives + FalseNegatives)
        #recall,answers:what proportion of actual Positives is correctly classified
        precision = diag(contadores) / colSums(contadores[,1:10])
        recall <- diag(contadores) / rowSums(contadores)
        fscore <- (2 * precision * recall) / (precision + recall)
        eale = c(reply, ne, gr, bl, fscore)
        datos = rbind(datos, eale)
        names(datos) = c("Replica", "Negro", "Gris", "Blanco",
                         "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
      }
    }
  }
}

  yolo=rep(c("n1g1b1","n1g1b2","n1g1b3", "n1g2b1","n1g2b2","n1g2b3",
             "n1g3b1","n1g3b2","n1g3b3", "n2g1b1","n2g1b2","n2g1b3",
             "n2g2b1","n2g2b2","n2g2b3", "n2g3b1","n2g3b2","n2g3b3",
             "n3g1b1","n3g1b2","n3g1b3", "n3g2b1","n3g2b2","n3g2b3",
             "n3g3b1","n3g3b2","n3g3b3"), 
            times=c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                    6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                    6, 6, 6, 6, 6, 6, 6))
  datos$combo <- yolo #crea una nueva columna con las pruebas
    #reordenamos los datos
  dat.m <- melt(datos,id.vars='combo',
                measure.vars=c('0','1','2','3','4','5','6','7','8','9'))
    #graficamos
  gr = ggplot(dat.m, aes(x= combo, y= value, fill= combo)) +
      geom_violin(fill = "#D29BFD")
  gr + geom_boxplot(fill = colorRampPalette(c("#FF1694", "#FC46AA","#F25278", 
                                             "#FC4C4E", "#FE7D6A", "#FC9483",
                                            "#FC94AF", "#F79AC0", "#FA86C4"))(27), 
                                             width = 0.3, lwd = 0.3) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            panel.background = element_rect(fill = "#CDCDCD")) +
      labs(x="combinacion de probabilidades", y= "f-score")
  
    #Estadísitica - 
    #con p menor a 0.05 se rechaza hipotesis nula H0
    #H0: los datos proceden de una distribución normal
    #H1: los datos no proceden de una distribución normal
  tapply(dat.m$value, dat.m$combo, shapiro.test)
    
      
  ala = dat.m%>%
    group_by(combo) %>%
    summarise(
      cantidad_de_participantes = n(),
      promedio = mean(value, na.rm = TRUE),
      desviacion_estandar = sd(value, na.rm = TRUE),
      varianza = sd(value, na.rm = TRUE)^2,
      mediana = median(value, na.rm = TRUE),
      rango_intercuartil =  IQR(value, na.rm = TRUE)
    )
    
  kruskal.test(value ~ combo, data = dat.m)
      
      