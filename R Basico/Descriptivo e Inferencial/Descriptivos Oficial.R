library(pastecs)
library(e1071)
library(readxl)
Alquiler <- read_excel("C:/Users/Diego/Desktop/Alquiler.xls")
View(Alquiler)
attach(Alquiler)
#Codigo usado para obtener la moda

mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))] # which.max da la posicion del maximo
}

#MEDIDAS DE TENDENCIA CENTRAL

#Área

mean(Area)
median(Area)
mode(Area)

#Precio de alquiler

mean(Precio)
median(Precio)
mode(Precio)

#Precio por M2

mean(PrecioM2)
median(PrecioM2)
mode(PrecioM2)

#MEDIDAS DE DISPERSIÓN (Varianza, Desviación Estandar, Rango Intercuartilico, Rango)

#Medidas de dispersión para el Área

var(Area, na.rm = FALSE)
sd(Area, na.rm = FALSE)
IQR(Area, na.rm = FALSE)
Rangoarea <- max(Area)-min(Area)
Rangoarea

#Medidas de dispersión para el Precio

var(Precio, na.rm = FALSE)
sd(Precio, na.rm =FALSE)
IQR(Precio, na.rm = FALSE)
Rangoprecio <- max(Precio)-min(Precio)
Rangoprecio

#Medias de dispersión para el PrecioM2

var(PrecioM2, na.rm = FALSE)
sd(PrecioM2, na.rm = FALSE)
IQR(PrecioM2, na.rm = FALSE)
Rangopreciom2 <- max(PrecioM2)-min(PrecioM2)
Rangopreciom2

#MEDIDAS DE POSICIÓN Y ASIMETRIA

#Cuantiles para cada variable evaluada

quantile(Area, probs = seq(0,1,0.25), na.rm = FALSE)
quantile(Precio, probs = seq(0,1,0.25), na.rm = FALSE)
quantile(PrecioM2, probs = seq(0,1,0.25), na.rm = FALSE)

#Simetria para cada variable evaluada

skewness(Area, na.rm = FALSE)
skewness(Precio, na.rm = FALSE)
skewness(PrecioM2, na.rm = FALSE)

#Curtosis para cada variable evaluada

kurtosis(Area, na.rm = FALSE)
kurtosis(Precio, na.rm = FALSE)
kurtosis(PrecioM2, na.rm = FALSE)

#Resumen de información

summary(Alquiler)
stat.desc(Alquiler, norm = FALSE)

#Box Plot
windows()
boxplot(Alquiler$Area)
boxplot(Alquiler$Precio)
boxplot(Alquiler$PrecioM2)

#Histograma

colors()[1:100] ##Se observan los codigos de la paleta de colores

windows() ##Muestra la gráfica en una ventana nueva
hist(Alquiler$Area, main = "Histograma de Área", sub = "Alquier de Viviendas", xlab = "Área", ylab = "Frecuencia", col = "saddlebrown", border = T, xlim=c(20,140), lwd=2, lty=3)
abline(v=56.25)
abline(v=95)
windows() ##Muestra la gráfica en una ventana nueva
hist(Alquiler$Precio, main = "Histograma del Precio de Alquiler", sub = "Alquier de Viviendas", xlab = "Precio de Alquiler en Euros", ylab = "Frecuencia", col = "forestgreen", border = T, lwd=2, lty=3)

windows() ##Muestra la gráfica en una ventana nueva
hist(Alquiler$PrecioM2, main = "Histograma del Precio de Alquiler por M2", sub = "Alquier de Viviendas", xlab = "Precio de Alquiler en Euros por M2", ylab = "Frecuencia", col = "forestgreen", border = T, lwd=2, lty=3)
