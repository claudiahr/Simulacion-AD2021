#https://machinelearningmastery.com/precision-recall-and-f-measure-for-imbalanced-classification/
datos=data.frame()
suppressMessages(library(ggplot2))
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

replica = 1:2
negro<-c(0.995, 0.9, 0.8)
gris<-c(0.92, 0.9, 0.5)
blanco<-c(0.5, 0.01, 0.002)

for(ne in negro){
  for(gr in gris){
    for(bl in blanco){
      
      for(reply in replica){
        modelos <- read.csv("digits.txt", sep=" ", header=FALSE, stringsAsFactors=F)
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
                         "0", "1", "2", "3", "4",
                         "5", "6", "7", "8", "9")
      }
    }
  }
}
    names(datos) <- c("Replica", "Negro","Gris","Blanco",
                      "0", "1","2","3","4","5","6","7","8","9")
    pruebas=rep(c("n1g1b1","n1g1b2","n1g1b3",
                  "n1g2b1","n1g2b2","n1g2b3",
                  "n1g3b1","n1g3b2","n1g3b3",
                  "n2g1b1","n2g1b2","n2g1b3",
                  "n2g2b1","n2g2b2","n2g2b3",
                  "n2g3b1","n2g3b2","n2g3b3",
                  "n3g1b1","n3g1b2","n3g1b3",
                  "n3g2b1","n3g2b2","n3g2b3",
                  "n3g3b1","n3g3b2","n3g3b3"), 
                times=c(2, 2, 2, 2, 2,
                        2, 2, 2, 2, 2,
                        2, 2, 2, 2, 2,
                        2, 2, 2, 2, 2,
                        2, 2, 2, 2, 2,
                        2, 2)
