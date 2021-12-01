suppressMessages(library(ggplot2))
#Calcular la viscosidad de la CMC simulando los datos de tasa de corte, esfuerzo cortante 
# viscosidad(en funcion de la tasa de corte y temp) = (A + (B*temperatura))tasa de corte^n-1
#cantcmc = 2 #Gramos
datos = data.frame()
nochidos = data.frame()
#Esf cort T = F /A
diamagit = 5 #Diametro agitador en cm
fuerza = 22 #Newton
area = pi * (diamagit/2)^2
forescor = fuerza/area
base = sort(runif(100, 0, 100))
esfcort = base * forescor

#Tasa de corte
tasadecorte = sort(runif(100, 1, 1000))


#Constantes de la CMC
A = 1.2606
B = -0.0211
temperatura = c(22, 30, 40, 50) #°C
#n = runif(2, 0, 1) #Solo esta tomando un valor de n, ocupo que el for tome los dos que genera
#n = as.data.frame.numeric()


for (temp in temperatura) {
  for (inpot in n) {
    n = runif(2, 0, 1) #indice de flujo n<1 =pseudoplastico n>=1 Newtoniano
  visc = ((A + (B * temp)) * tasadecorte^n-1)
  #ifelse(n < 1, print("Pseudoplastico"), print("Dilatante"))
  #print(n)
  resultado = c(temp, n, visc)
  nochidos = rbind(nochidos, resultado)
  names(nochidos) = c("Temperatura", "n", 1:100)#1:100
  #datos = data.frame(t(nochidos[-1]))
  #colnames(datos) = c("Tasa de corte", "Replica", nochidos[, 1])
  }
}


#Grafica
datos$`Tasa de corte`= as.factor(datos$`Tasa de corte`)
nochidos$Temperatura = as.factor(nochidos$Temperatura)
ggplot(datos, aes(x = tasadecorte, y = visc, colour = temp))+
  geom_point() +
  #geom_line() +
  labs(x = "Tasa de corte", y = "Viscosidad")



