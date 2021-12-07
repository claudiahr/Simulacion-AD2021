# https://www.datasciencemadesimple.com/sorting-dataframe-r-using-dplyr/
    #sort a data frame

#toma diferentes n ya wujuuu fiesta fiesta
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(dplyr))
#Calcular la viscosidad de la CMC simulando los datos de tasa de corte, esfuerzo cortante 
# viscosidad(en funcion de la tasa de corte y temp) = (A + (B*temperatura))tasa de corte^n-1
#cantcmc = 2 #Gramos
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
nqty = 1:2 #Cuantas n


for (temp in temperatura) {
  for (inpot in nqty) {
    n = runif(1, 0, 1) #indice de flujo n<1 =pseudoplastico n>=1 Newtoniano
    visc = ((A + (B * temp)) * tasadecorte^n-1)
    tassadecorte = c(tasadecorte)
    tazzadecorte = rep(tassadecorte, 8)
    resultado = c(temp, n, visc)
    nochidos = rbind(nochidos, resultado)
    names(nochidos) = c("Temperatura", "n", 1:100)
  }
}
datos = as.data.frame(melt(nochidos, id = c("Temperatura", "n")))
karla = arrange(datos, Temperatura)

astringente = karla
astringente$new_col = tazzadecorte
names(karla) = c("asTemperatura", "nvalue", 
                       "dntmatter", "asviscosidad")
names(astringente) = c("asTemperatura", "nvalue", 
                       "dntmatter", "asviscosidad", "asTasadecorte")


cols = c("#FF35B8", "#C947F5", "#FFCC00", "#0000FF")
#BOXPLOT OK
karla$asTemperatura = as.factor(karla$asTemperatura)
ggplot(karla, aes(x = asTemperatura, y = asviscosidad,
                  fill = asTemperatura)) +
  geom_boxplot(alpha = 0.9) +
  labs(x = "Temperatura (°C)", y= "Viscosidad (mPa-s)", 
       fill = "Temperatura (°C)") +
  guides(fill="none")

#SCATTER PLOT OK
astringente$asTemperatura = as.factor(astringente$asTemperatura)
ggplot(astringente, aes(x = asTasadecorte, y = asviscosidad, 
                        color = asTemperatura, shape = asTemperatura)) +
  geom_point(size = 3) +
  scale_color_manual(values = cols)+
  labs( x = "Tasa de corte (1/s)", y = "Viscosidad (mPa-s)",
        color = "Temperatura (°C)", shape = "Temperatura (°C)")














