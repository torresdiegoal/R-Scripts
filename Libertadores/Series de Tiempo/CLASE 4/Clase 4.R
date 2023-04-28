#################################################################
#################################################################
#####   Programa Clase 4 - Series de Tiempo                  ####
#####   Fecha de creación: 20200415                          ####
#####   Fecha de última edición: 20200415                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: edgar.lopez@libertadores.edu.co              ####
#################################################################
#################################################################

rm(list = ls())
# https://stats.stackexchange.com/questions/229948/plotting-predicted-values-in-arima-time-series-in-r/353375
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

inpath  <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"
outpath <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/CLASE 4/"

#################################################################
#####                   Carga de información                #####   
#################################################################

### Metodo Momentos
set.seed(1236)
ma.sim.1 <- arima.sim(list(order = c(0,0,1), ma = c(-0.9)), n = 120)
ma.sim.2 <- arima.sim(list(order = c(0,0,1), ma = c(0.9)), n = 120)
ma.sim.3 <- arima.sim(list(order = c(0,0,1), ma = c(-0.9)), n = 60)
ma.sim.4 <- arima.sim(list(order = c(0,0,1), ma = c(0.9)), n = 60)
ma.sim.5 <- arima.sim(list(order = c(0,0,1), ma = c(0.5)), n = 60)

Arima(ma.sim.1, order = c(0,0,1), method = 'CSS', include.mean = F)
Arima(ma.sim.2, order = c(0,0,1), method = 'CSS', include.mean = F)
Arima(ma.sim.3, order = c(0,0,1), method = 'CSS', include.mean = F)
Arima(ma.sim.4, order = c(0,0,1), method = 'CSS', include.mean = F)
Arima(ma.sim.5, order = c(0,0,1), method = 'CSS', include.mean = F)

ar.sim.1 <- arima.sim(list(order = c(1,0,0), ar = c(0.9)), n = 60)
ar.sim.2 <- arima.sim(list(order = c(1,0,0), ar = c(0.4)), n = 60)
ar.sim.3 <- arima.sim(list(order = c(2,0,0), ar = c(1.5, -0.75)), n = 120)

ar(ar.sim.1, order.max=1, AIC=F, method='yw')
ar(ar.sim.2, order.max=1, AIC=F, method='yw')
ar(ar.sim.3, order.max=2, AIC=F, method='yw')


### Ejemplo real de abundancia de liebres canadienses de 1905 a 1935

data(hare)
autoplot(hare)
BoxCox.ar(hare)
acf(hare^0.5)
pacf(hare^0.5)
# Puede ser un AR(2) o AR(3)
ar(hare^0.5, order.max=2, AIC=F, method='yw')
ar(hare^0.5, order.max=3, AIC=F, method='yw')
# r1 = 0.736 y r2 = 0.304


### Esquema de simulación

data(ar1.s); data(ar1.2.s)
autoplot(ar1.s)
autoplot(ar1.2.s)
length(ar1.s); length(ar1.2.s)

ar(ar1.s,order.max=1,AIC=F,method='yw')
ar(ar1.s,order.max=1,AIC=F,method='ols')
ar(ar1.s,order.max=1,AIC=F,method='mle')

ar(ar1.2.s,order.max=1,AIC=F,method='yw')
ar(ar1.2.s,order.max=1,AIC=F,method='ols')
ar(ar1.2.s,order.max=1,AIC=F,method='mle')


data(ar2.s)
autoplot(ar2.s)
ar(ar2.s,order.max=2,AIC=F,method='yw') # método momentos
ar(ar2.s,order.max=2,AIC=F,method='ols') # método minimos cuadrados
ar(ar2.s,order.max=2,AIC=F,method='mle') # método máxima verosimilitud

data(arma11.s)
autoplot(arma11.s)
Arima(arma11.s, order=c(1,0,1),method='CSS') # método momentos
Arima(arma11.s, order=c(1,0,1),method='ML') # método minimos cuadrados
Arima(arma11.s, order=c(1,0,1),method ='CSS-ML') # método máxima verosimilitud



### Ejemplo con hare

# Especificación del modelo

data(hare)
autoplot(hare, main = 'Serie de tiempo de abundancia de liebres en Canada (1905-1935)')
BoxCox.ar(hare)
adf.test(hare^0.5)
acf(hare^0.5, main = 'ACF Liebres^1/2')
pacf(hare^0.5, main = 'PACF Liebres^1/2')


# Estimación y diagnostico

m1.hare <- Arima(hare^0.5, order=c(3,0,0),method='ML')
m2.hare <- Arima(hare^0.5, order=c(2,0,0),method='ML')
plot(m1.hare, type='b', xlab='Year',ylab='Sqrt(hare)', main = 'Raices AR(3)')
plot(m2.hare, type='b', xlab='Year',ylab='Sqrt(hare)', main = 'Raices AR(2)')

qqnorm(residuals(m1.hare), main = 'Q-Q Plot AR(3)'); qqline(residuals(m1.hare))
qqnorm(residuals(m2.hare), main = 'Q-Q Plot AR(2)'); qqline(residuals(m2.hare))
acf(residuals(m1.hare), main = 'ACF Residuales AR(3)')
acf(residuals(m2.hare), main = 'ACF Residuales AR(2)')

acf(residuals(m2.hare))
signif(acf(residuals(m2.hare),plot=F)$acf[1:6],2)
tsdiag(m2.hare, gof=15, omit.initial=F)
Box.test(resid(m2.hare),type="Ljung",lag=5,fitdf=1)
shapiro.test(residuals(m2.hare))
# Pronostico

autoplot(forecast(m1.hare, h = 30), main = 'Pronóstico 30 pasos adelante - AR(3)')
autoplot(forecast(m2.hare, h = 30), main = 'Pronóstico 30 pasos adelante - AR(2)')



### Ejemplo Oil

# Especificación

data(oil.price)
autoplot(oil.price)
autoplot(log(oil.price))
autoplot(diff(oil.price))
autoplot(diff(log(oil.price)))
acf(diff(log(oil.price)))
pacf(diff(log(oil.price)))
adf.test(diff(log(oil.price)))

# Estimación y diagnostico
Arima(log(oil.price),order=c(0,1,1),method='CSS')
Arima(log(oil.price),order=c(0,1,1),method='ML')
Arima(log(oil.price),order=c(0,1,2),method='ML')


m1.oil <- Arima(log(oil.price),order=c(0,1,1))#, include.drift = T)
plot(rstandard(m1.oil),ylab='Standardized residuals',type='l')
abline(h=0)
#autoplot(rstandard(m1.oil))

qqnorm(residuals(m1.oil)); qqline(residuals(m1.oil))
acf(residuals(m1.oil))
hist(window(rstandard(m1.oil), start=c(1986,1)),
     xlab='Standardized Residuals', 
     main = 'Histograma Residuales')
tsdiag(m1.oil, gof=15, omit.initial=F)
Box.test(resid(m1.oil),type="Ljung",lag=5,fitdf=1)

#autoplot(forecast(m1.oil, h = 50))

hw <- HoltWinters(oil.price)
plot(hw)
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)
#https://www.r-bloggers.com/holt-winters-forecast-using-ggplot2/
