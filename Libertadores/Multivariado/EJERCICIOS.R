Pokemon <- read_csv("PCA/Pokemon.csv")

#: Identificación para cada pokemon
#Nombre: Nombre de cada pokemon
#Tipo 1: Cada pokemon tiene un tipo, esto determina la debilidad/resistencia a los ataques
#Tipo 2: Algunos pokemon son de tipo dual y tienen 2
#Total: suma de todas las estadísticas que vienen después de esto, una guía general de lo fuerte que es un pokemon
#HP: los puntos de golpe, o salud, definen cuánto daño puede soportar un pokemon antes de desmayarse
#Ataque: el modificador de base para los ataques normales (por ejemplo, rascar, golpear)
#Defensa: la base daña la resistencia contra los ataques normales
#SP Atk: ataque especial, el modificador de base para ataques especiales (por ejemplo, explosión de fuego, rayo de burbujas)
#SP Def: la base de resistencia al daño contra ataques especiales
#Velocidad: determina qué pokemon ataca primero en cada ronda


#################
install.packages("tidyverse")
install.packages("tidyr")
library(tidyverse)
library(tidyr)

names(Pokemon)


#################
install.packages("haven")
library(haven)
Paises_Mundo <- read_sav("PCA/Paises Mundo.sav")
View(Paises_Mundo)
datos<-data.frame(Paises_Mundo)

head(datos)

mundo<-datos[,-1]

rownames(mundo) <- datos[,1]
head(mundo)

dic= c("Tasa.Crecimiento","Tasa.Mortalidad.Inf","Porc.Mujeres.Pob.Activa","PIB",
       "Prod.Electr","Consumo.Agua.PC","Lineas Telef", "Area.verde",
       "Porc.Deforesta.Anual", "Consumo.Energ.PC", "Emision.CO2")

colnames(mundo)<-dic
head(mundo)


