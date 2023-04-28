#################################################################
#################################################################
#####   Programa Clase 2 - Series de Tiempo                  ####
#####   Fecha de creación: 20200325                          ####
#####   Fecha de última edición: 202003                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: edgar.lopez@libertadores.edu.co              ####
#################################################################
#################################################################

rm(list = ls())
# https://rpubs.com/riazakhan94/arima_with_example
#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(TSA)
library(tseries)
library(forecast)
library(ggplot2)
library(readxl)
library(ggfortify)

#################################################################
#####                   Sistema de carpetas                 #####   
#################################################################

#inpath  <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"
inpath  <- "C:/Users/Diego Torres/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"
outpath <- "C:/Users/Diego Torres/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"

#################################################################
#####                   Carga de información                #####   
#################################################################

TRM_COL <- read_excel(paste(inpath,"TRM_Colombia_11-11-2021.xlsx", sep = ""),
                      sheet = "TRM_historico")
TRM     <- ts(TRM_COL$TRM, start = c(1991, 315), frequency=365)

#################################################################
#####                 Ejecución de programa                 #####   
#################################################################

autoplot(TRM)
adf.test(TRM)

### Simulación IMA(2,2)
# se diferencia para volver estacionaria en media
set.seed(1236)
ts.sim.ima <- arima.sim(list(order = c(0,2,2), ma = c(1, -0.6)), n = 300)
autoplot(ts.sim.ima, ylab=' ')
ts.sim.ima.d1 <- autoplot(diff(ts.sim.ima),ylab='Primera diferencia')
autoplot(ts.sim.ima.d1)
ts.sim.ima.d2 <- autoplot(diff(ts.sim.ima, difference=2),ylab='Segunda diferencia')  #maximo dos diferenciaciones, despues de aho no hace nada
autoplot(ts.sim.ima.d2)

### Transformación en varianza
#SOLO SI se observa varianza movil, lambda = 1 no afecta en nada
# si lambda = 0, se aplica log a la serie
data("electricity")
autoplot(electricity)
autoplot(log(electricity), ylab = 'Log(Electricity)')
BoxCox.ar(electricity)


### Ejercicio Practico 1
data(oil.price)
autoplot(oil.price)
adf.test(oil.price)
BoxCox.ar(oil.price)
log.oil.price <- log(oil.price) 
autoplot(log.oil.price)


# Diferenciando
new.oil.price <- diff(log.oil.price)
autoplot(new.oil.price)
adf.test(new.oil.price)

autoplot(acf(new.oil.price, plot = FALSE))
autoplot(pacf(new.oil.price, plot = FALSE))

# Podriamos ajustar un ARIMA(1,1,0)

### Ejericico Practico 2

autoplot(TRM)
diff.TRM <- diff(TRM, difference=1)
autoplot(diff.TRM)
adf.test(diff.TRM)
autoplot(acf(diff.TRM, plot = FALSE))
autoplot(pacf(diff.TRM, plot = FALSE))
# Podriamos ajustar un ARIMA(5,1,0)
auto.arima(diff.TRM)


ton <- read.csv(paste(inpath,"Exportaciones.csv", sep = "")
                , sep = ";")
ton <- subset(ton, select = c(1,3))


ton.ts <- ts(ton$ton.metricas, start = c(1992,1), frequency = 12)
autoplot(ton.ts)

optimo <- BoxCox.ar(ton.ts)
ton.ts.opt <- ton.ts^optimo$mle
autoplot(ton.ts.opt)
# Diferenciando en 1
diff.ton.opt <- diff(ton.ts.opt, difference=1)
adf.test(diff.ton.opt)
autoplot(diff.ton.opt)

# Autocorrelograma
autoplot(acf(diff.ton.opt, plot = FALSE))
autoplot(pacf(diff.ton.opt, plot = FALSE))


auto.arima(diff.ton.opt)


acf(arima.sim(list(order = c(0,1,1), ma = c(-0.8097)), n = 300))


acf(arima.sim(list(order = c(0,0,1), ma = c(1)), n = 300))



acf(arima.sim(list(order = c(1,0,0), ar = c(-.9)), n = 300))


acf(arima.sim(list(order = c(0,0,2), ma = c(-1, 1)), n = 300))

acf(arima.sim(list(order = c(3,0,2), ar = c(-.2, -.4, -.3), ma = c(.1,-1)), n = 300))


acf(arima.sim(list(order = c(1,0,2), ar = c(.85), ma = c(-1.6,.68)), n = 300))
