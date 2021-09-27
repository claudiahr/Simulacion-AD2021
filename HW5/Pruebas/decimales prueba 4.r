library(tidyverse)
numerock = 0.369258147
numetry  = 0.369777741
as.character(numerock)
as.character(numetry)

posicion = 3
str_sub(as.character(numerock), 3, posicion)
str_sub(as.character(numetry), 3, posicion)

while (str_sub(as.character(numerock), 3, posicion) == 
       str_sub(as.character(numetry), 3, posicion)) {
  print(posicion - 2)
  posicion = posicion + 1
}