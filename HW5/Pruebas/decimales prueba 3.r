library(tidyverse)
library(datos)
library(htmlwidgets)
#str_length(string1)
#str_length(string2)

numerock = 0.369258147
numetry  = 0.369258741
string1 = numerock
string2 = numetry

pos = 0
while (TRUE) {
 if (string2 == string1) {
  pos = pos +1
    print(pos)
 }
 else{
   break
  }
}  

#for (pos in string2) {
#if (string1 == string2) {
#print (pos)
# pos = pos + 1
#}
#else{
#  break
# }
#}
