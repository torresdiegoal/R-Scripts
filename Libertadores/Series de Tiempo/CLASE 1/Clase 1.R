#################################################################
#################################################################
#####   Programa Clase 1 - Series de Tiempo                  ####
#####   Fecha de creación: 20201903                          ####
#####   Fecha de última edición: 20200321                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: ejlopezmor@gmail.com                         ####
#################################################################
#################################################################

rm(list = ls())
 

#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(TSA)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggfortify)   #el de autoplot
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html

#################################################################
#####                   Sistema de carpetas                 #####   
#################################################################

inpath  <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"
outpath <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/CLASE 1/"

#################################################################
#####                   Carga de información                #####   
#################################################################

data(larain)
data(AirPassengers)
data(tempdub)

TRM_COL <- read_excel(paste(inpath,"TRM COL.xlsx", sep = ""),
                      sheet = "BaseDatos")
class(TRM_COL$Fecha)
TRM     <- ts(TRM_COL$TRM, start = c(1991, 315), frequency=365)


datos.prueba   <- rnorm(1:12)
datos.prueba.2 <- cbind(c(rep(1,2),rep(2,2),rep(3,2),rep(4,2),rep(5,2),rep(6,2)),datos.prueba)
datos.prueba.2 <- data.frame(datos.prueba.2)
names(datos.prueba.2) <- c('bimes', 'valor')
datos.prueba.2 <- datos.prueba.2 %>%
  group_by(bimes) %>%
  summarise(sum(valor))

datos.prueba.2 <- data.frame(datos.prueba.2) 
datos.prueba.2 <- ts(datos.prueba.2$sum.valor.)
datos.prueba.2 <- ts(datos.prueba, start = c(2019, 6), frequency = 12)

#################################################################
#####                  Funciones recurrentes                #####   
#################################################################

#################################################################
#####                 Ejecución de programa                 #####   
#################################################################

### Comparación de librerias graficando

plot(larain,ylab='Inches',xlab='Year',type='o')
plot(y=larain,x=zlag(larain),ylab='Inches',
     xlab='Previous Year Inches')

autoplot(larain, ylab='Pulgadas',xlab='Año', main = 'Serie de tiempo de precipitación anual en Los Angeles')
ggplot(larain, aes(x=larain, y=zlag(larain))) + geom_point() + 
  labs(x = "Pulgadas Año Previo" , y = 'Pulgadas Año Actual') + 
  ggtitle("Dispersión de precipitación en Los Angeles")

  
datos <- cbind(larain, zlag(larain))
datos <- cbind(1878:1992, datos) 
datos <- data.frame(datos)
names(datos) <- c('year', 'larain', 'zlag.larain')
subset(datos, larain >= 40)

### Ejemplo de datos con tendencia

datos.lineal <- ts(1:100)
autoplot(datos.lineal)
ggplot(datos.lineal, aes(x=datos.lineal, y=zlag(datos.lineal))) + 
  geom_point() 

### Ejemplo con AirPassengers
autoplot(AirPassengers)
ggplot(AirPassengers, aes(x=AirPassengers, y=zlag(AirPassengers))) + 
  geom_point() 
ggplot(AirPassengers, aes(x=AirPassengers, y=zlag(AirPassengers))) + geom_point() + 
  labs(x = "Año Previo" , y = 'Año Actual') + 
  ggtitle("Dispersión AirPassengers")


### Ejemplo Tendencia

data(rwalk) # rwalk contains a simulated random walk
head(rwalk)
plot(rwalk,type='o',ylab='Random Walk')
#autoplot(rwalk)
#ggplot(rwalk, aes(x=rwalk, y=zlag(rwalk))) + 
# geom_point() 

modelo.1 <- lm(rwalk ~ time(rwalk) )
summary(modelo.1)
abline(modelo.1)

# cALCULO MANUAL
t <- 1:dim(rwalk)[1]
beta.1 <- sum((rwalk - mean(rwalk)) * (t - mean(t))) / sum((t - mean(t))^2)
bata.0 <- mean(rwalk) - beta.1 * mean(t) 


### EJEMPLO SERIE Estacional

data(tempdub)
plot(tempdub,type='o',ylab='Modelo estacional')

modelo.prev <- lm(tempdub ~ time(tempdub))
summary(modelo.prev)
abline(modelo.prev)

mes.     <- season(tempdub) # period added to improve table display
mes.
modelo.2 <- lm(tempdub ~ mes. - 1) # -1 removes the intercept term
summary(modelo.2)

modelo.3 <- lm(tempdub ~ mes.) # -1 removes the intercept term
summary(modelo.3)

autoplot(stl(tempdub, s.window = 'periodic'), ts.colour = 'blue')

plot(y=rstudent(modelo.2),x=as.vector(time(tempdub)),
     xlab='Time',ylab='Standardized Residuals',type='o')

plot(y=rstudent(modelo.2),x=as.vector(fitted(modelo.3)),
     xlab='Fitted Trend Values',
     ylab='Standardized Residuals',type='n')
points(y=rstudent(modelo.2),x=as.vector(fitted(modelo.2)),
       pch=as.vector(season(tempdub)))

hist(rstudent(modelo.2),xlab='Standardized Residuals')
qqnorm(rstudent(modelo.2))

acf(rstudent(modelo.2))

#acf(tempdub)
#autoplot(acf(tempdub, plot = FALSE))

#################################################################
#####                  Salidas o exportación                #####   
#################################################################






