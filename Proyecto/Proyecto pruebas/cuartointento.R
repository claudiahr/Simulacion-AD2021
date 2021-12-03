#toma diferentes n ya wujuuu fiesta fiesta
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
n = runif(2, 2, 3) # Con esto funcion a cool el nochidos (no cambiar de uno porque agrega una n al 22)


for (temp in temperatura) {
  for (inpot in n) {
    n = runif(2, 2, 3) #indice de flujo n<1 =pseudoplastico n>=1 Newtoniano (no cambiar el 1 porque agrega una columna de n)
    visc = ((A + (B * temp)) * tasadecorte^n-1)
    #ifelse(n < 1, print("Pseudoplastico"), print("Dilatante"))
    #print(n)
    resultado = c(temp, n, visc)
    nochidos = rbind(nochidos, resultado)
    names(nochidos) = c("Temperatura", "n", 1:100)#1:100
    #datos = data.frame(tasa de corte, t(nochidos[-1]))
    #colnames(datos) = c(, nochidos[, 1])
  }
}