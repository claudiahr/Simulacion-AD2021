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
n = runif(4, 0, 1)


for (temp in temperatura) {
  visc = (A + (B * temp)) * tasadecorte^n-1
  #ifelse(n < 1, print("Pseudoplastico"), print("Dilatante"))
  #print(n)
  resultado = c(temp, visc)
  nochidos = rbind(nochidos, resultado)
  names(nochidos) = c("Temperatura", 1:100)
  datos = data.frame(tasadecorte, t(nochidos[-1]))
  colnames(datos) = c("Tasa de corte", nochidos[, 1])
}


#Grafica
datos$`Tasa de corte`= as.factor(datos$`Tasa de corte`)
nochidos$Temperatura = as.factor(nochidos$Temperatura)
ggplot(datos, aes(x = tasadecorte, y = visc, colour = temp))+
  geom_point() +
  #geom_line() +
  labs(x = "Tasa de corte", y = "Viscosidad")



