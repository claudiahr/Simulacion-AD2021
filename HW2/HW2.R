library(parallel)
library(openxlsx)
dim = 20
num = dim^2
dur = 10
p = c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
datos = data.frame()
unos = numeric()
ceros = numeric()
for (inicial in p) {
  for (replica in 1:30) { # minimo 30 1:30
    actual = matrix(round(runif(num) < p), nrow=dim, ncol=dim, byrow=TRUE) 
    paso = function(pos) {
      fila = floor((pos - 1) / dim) + 1
      columna = ((pos - 1) %% dim) + 1
      vecindad =  actual[max(fila - 1, 1) : min(fila + 1, dim), 
                         max(columna - 1, 1): min(columna + 1, dim)]
      return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
    }
    cluster <- makeCluster(detectCores() - 1)
    clusterExport(cluster, "dim")
    clusterExport(cluster, "paso")
    
    for (iteracion in 1:dur) {
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster, 1:num, paso)
      vivos = sum(siguiente)
      cat(inicial, replica, iteracion, vivos, '\n') #concatenar
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    }
    if (vivos > 0) {
      vivos = 1
      print(vivos)
    } else {
      vivos = 0
      print(0)
    }
    unos = c(unos, sum(vivos > 0)) #de aqui en adelante se hace mi vectro de 1 y 0s
    ceros = c(ceros, sum(vivos = 0))
    stopCluster(cluster)
    }
    print(unos)   
    dfunos = data.frame()
    dfunos = rbind(dfunos, unos)
}
write.xlsx(dfunos, file = "miarchivodf.xlsx")