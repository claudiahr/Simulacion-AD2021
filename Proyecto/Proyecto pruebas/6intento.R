#toma diferentes n ya wujuuu fiesta fiesta
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
#Calcular la viscosidad de la CMC simulando los datos de tasa de corte, esfuerzo cortante 
# viscosidad(en funcion de la tasa de corte y temp) = (A + (B*temperatura))tasa de corte^n-1
#cantcmc = 2 #Gramos
datos = data.frame()
nochidos = data.frame()
pepe = data.frame()
#Esf cort T = F /A
diamagit = 5 #Diametro agitador en cm
fuerza = 22 #Newton
area = pi * (diamagit/2)^2
forescor = fuerza/area
base = sort(runif(100, 0, 100))
esfcort = base * forescor

#Tasa de corte
#tasadecorte = sort(runif(100, 1, 1000))


#Constantes de la CMC
A = 1.2606
B = -0.0211
tasadecorte = sort(runif(100, 1, 1000))
temperatura = c(22, 30, 40, 50) #°C
nqty = 1:2 #Cuantas n


for (temp in temperatura) {
  for (inpot in nqty) {
    n = runif(1, 0, 1) #indice de flujo n<1 =pseudoplastico n>=1 Newtoniano
    visc = ((A + (B * temp)) * tasadecorte^n-1)
    resultado = c(n, visc)
    tassadecorte = c(tasadecorte)
    print(tassadecorte)
    nochidos = rbind(nochidos, n, temp, resultado)
  }
}
#names(nochidos) = c("Temperatura", "n", 1:100)
pepe = rbind(pepe, tassadecorte)
datos = data.frame(t(pepe), t(nochidos[-1]))
colnames(datos) = c("tasa de corte", "t1n1", "t11", "t1v1",
                    "t1n2", "t12", "t1v2",
                    "t2n1", "t21", "t2v1",
                    "t2n2", "t22", "t2v2",
                    "t3n1", "t31", "t3v1",
                    "t3n2", "t32", "t3v2",
                    "t4n1", "t41", "t4v1",
                    "t4n2", "t42", "t4v2")

ggplot(datos, aes(x = t.pepe. , y = visc)) + 
  geom_boxplot() +
  labs(x = "Tasa de corte", y = "Viscosidad")


cbind(datos[2:4], stack(datos[5:25]))

astringente = melt(datos, id.var = c('t1n1', 't11', 't1v1'), variable.name = 'outcomes')




