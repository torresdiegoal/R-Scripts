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

#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(TSA)
library(ggplot2)
library(readxl)
library(ggfortify)

#################################################################
#####                   Sistema de carpetas                 #####   
#################################################################

inpath  <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"
outpath <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/CLASE 2/"

#################################################################
#####                   Carga de información                #####   
#################################################################


# METODOLOGIA BOX-jENKINS PARA AJUSTE DE POSIBLES MODELOS

########################
#### Ejericios con MA(q)
########################

#### Ejercicio comportamiento de rho en MA(1)

theta <- c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0,
           0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
rho.1 <- -theta/(1 + theta^2)
datos <- data.frame(cbind(theta, rho.1))
ggplot(datos, aes(theta, rho.1)) + geom_line() + 
  ggtitle(paste("Comportamiento de la correlación Vs Theta", sep = ""))


#### Simulación de proceso MA(1) 
set.seed(999)

theta.sim <- 0.9
rho.1.sim <- -theta.sim/(1 + theta.sim^2)

# generando 101 puntos de ruido blanco - asumiendo que son Normales
ruido.blanco <- rnorm(101, 0, 1)
hist(ruido.blanco)
y.t <- zlag(ruido.blanco) - theta.sim * ruido.blanco

y.t <- y.t[-1]
hist(y.t)
y.t <- ts(y.t)

autoplot(y.t, main = paste("Simulación de serie de tiempo MA(1) con theta = ", 
                           theta.sim, sep = "")) 
acf(y.t, main = "Ar(1)")
ggplot(y.t, aes(x=y.t, y=zlag(y.t))) + geom_point() + 
  labs(x = expression(Y[t-1]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión primer rezago para MA(1) con theta = ", theta.sim, sep = ""))

ggplot(y.t, aes(x=y.t, y= zlag(zlag(y.t)) )) + geom_point() + 
  labs(x = expression(Y[t-2]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión Segundo rezago para MA(1) con theta = ", theta.sim, sep = ""))

cbind(theta.sim, rho.1.sim)

#### Simulación de proceso MA(2) 
set.seed(999)

theta.1.sim <- 1
theta.2.sim <- -0.6
rho.1.sim   <- (-theta.1.sim + theta.1.sim * theta.2.sim) / 
  (1 + theta.1.sim^2 + theta.2.sim^2)
rho.2.sim   <- (-theta.2.sim) / (1 + theta.1.sim^2 + theta.2.sim^2)


# generando 102 puntos de ruido blanco - asumiendo que son Normales
ruido.blanco <- rnorm(102, 0, 1)
y.t <- zlag(zlag(ruido.blanco)) - (theta.1.sim * zlag(ruido.blanco)) -
  (theta.2.sim * ruido.blanco)
y.t <- y.t[-c(1,2)]
y.t <- ts(y.t)

autoplot(y.t, main = paste("Simulación de serie de tiempo MA(2) con theta.1 = ", 
                           theta.1.sim," y theta.2 = ", theta.2.sim, sep = ""))

acf(y.t)

ggplot(y.t, aes(x=y.t, y=zlag(y.t))) + geom_point() + 
  labs(x = expression(Y[t-1]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para MA(2) con theta.1 = ", 
                theta.1.sim," y theta.2 = ", theta.2.sim, sep = ""))

ggplot(y.t, aes(x=y.t, y= zlag(zlag(y.t)) )) + geom_point() + 
  labs(x = expression(Y[t-2]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para MA(1) con theta.1 = ", 
                theta.1.sim," y theta.2 = ", theta.2.sim, sep = ""))

ggplot(y.t, aes(x=y.t, y= zlag(zlag(zlag(y.t))) )) + geom_point() + 
  labs(x = expression(Y[t-3]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para MA(1) con theta.1 = ", 
                theta.1.sim," y theta.2 = ", theta.2.sim, sep = ""))

cbind(theta.1.sim, theta.2.sim, rho.1.sim, rho.2.sim)


########################
#### Ejericios con AR(p)
########################

#### Ejercicio comportamiento de rho en AR(1)

k <- 10
phi.sim <- -0.5
rho.sim <- phi.sim^c(1:k) 

datos.sim <- data.frame(c(1:k),rho.sim)
names(datos.sim) <- c('k','rho')

ggplot(datos.sim, aes(k, rho)) +
  geom_segment(aes(xend = k, yend = 0), size = 3, lineend = "butt") + 
  ggtitle(paste("Autocorrelograma de AR(1) con phi = ", phi.sim, sep = ""))

  

#### Simulación de proceso AR(1) 
set.seed(999)

phi  <- 0.8
y    <- NULL
y[1] <- rnorm(1,0,1) 
for( t in 2:100 ) y[t] <- phi * y[t-1] + rnorm(1,0,1) 
y.t <- ts(y)

#y.t <- arima.sim(model = list(ar=c(phi)), n = 100) 
autoplot(y.t, main = paste("Simulación de serie de tiempo AR(1) con phi = ", 
                           phi, sep = "")) 

ggplot(y.t, aes(x=y.t, y=zlag(y.t))) + geom_point() + 
  labs(x = expression(Y[t-1]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para AR(1) con phi = ", phi, sep = ""))

ggplot(y.t, aes(x=y.t, y= zlag(zlag(y.t)) )) + geom_point() + 
  labs(x = expression(Y[t-2]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para AR(1) con phi = ", phi, sep = ""))

ggplot(y.t, aes(x=y.t, y= zlag(zlag(zlag(y.t))) )) + geom_point() + 
  labs(x = expression(Y[t-3]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para AR(1) con phi = ", phi, sep = ""))



#### Ejercicio comportamiento de rho en AR(2)

k <- 12
phi.1.sim <- 1.5
phi.2.sim <- -0.75
est <- c(phi.1.sim + phi.2.sim, phi.1.sim + phi.2.sim, abs(phi.2.sim))
est
rho.sim   <- NULL
rho.sim[1]<- 1
rho.sim[2]<- phi.1.sim / (1 - phi.2.sim)
for( kk in 3:13 ) rho.sim[kk] <- phi.1.sim * rho.sim[kk-1] + phi.2.sim * rho.sim[kk-2] 
rho.sim <- rho.sim[-1]

datos.sim <- data.frame(cbind(c(1:k), rho.sim))
names(datos.sim) <- c('k','rho')

ggplot(datos.sim, aes(k, rho)) +
  geom_segment(aes(xend = k, yend = 0), size = 3, lineend = "butt") + 
  ggtitle(paste("Autocorrelograma de AR(2) con phi.1 = ", 
                phi.1.sim, " y phi.2 = ", phi.2.sim, sep = ""))



#### Simulación de proceso AR(2) 
set.seed(999)

phi.1 <- 1.5
phi.2 <- -0.75 
est <- c(phi.1 + phi.2, phi.1 + phi.2, abs(phi.2))
est

y    <- NULL
y[1] <- rnorm(1,0,1)
y[2] <- rnorm(1,0,1)
for( t in 3:100 ) y[t] <- phi.1 * y[t-1] + phi.2 * y[t-2] + rnorm(1,0,1) 
y.t <- ts(y)

#y.t <- arima.sim(model = list(ar=c(phi)), n = 100) 
autoplot(y.t, main = paste("Simulación de serie de tiempo AR(2) con phi.1 = ", 
                           phi.1, " y phi.2 = ", phi.2, sep = "")) 

ggplot(y.t, aes(x=y.t, y=zlag(y.t))) + geom_point() + 
  labs(x = expression(Y[t-1]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para AR(2) con phi.1 = ", 
                phi.1, " y phi.2 = ", phi.2, sep = ""))

ggplot(y.t, aes(x=y.t, y= zlag(zlag(y.t)) )) + geom_point() + 
  labs(x = expression(Y[t-2]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para AR(2) con phi.1 = ", 
                phi.1, " y phi.2 = ", phi.2, sep = ""))

ggplot(y.t, aes(x=y.t, y= zlag(zlag(zlag(y.t))) )) + geom_point() + 
  labs(x = expression(Y[t-3]) , y = expression(Y[t])) + 
  ggtitle(paste("Dispersión para AR(2) con phi.1 = ", 
                phi.1, " y phi.2 = ", phi.2, sep = ""))


#############################################
#################### PRACTICA ################
###############################################

## MA(1)


set.seed(999)

theta.sim <- 0.9
rho.1.sim <- -theta.sim/(1 + theta.sim^2)

ruido.blanco <- rnorm(101, 0, 1)
y.t <- zlag(ruido.blanco) - theta.sim * ruido.blanco
y.t <- y.t[-1]

y.t <- ts(y.t)
acf(y.t, main = "MA(1)")


## MA(2)

set.seed(999)

theta.1.sim <- 1
theta.2.sim <- -0.6
rho.1.sim   <- (-theta.1.sim + theta.1.sim * theta.2.sim) / 
  (1 + theta.1.sim^2 + theta.2.sim^2)
rho.2.sim   <- (-theta.2.sim) / (1 + theta.1.sim^2 + theta.2.sim^2)

ruido.blanco <- rnorm(102, 0, 1)
y.t <- zlag(zlag(ruido.blanco)) - (theta.1.sim * zlag(ruido.blanco)) -
  (theta.2.sim * ruido.blanco)
y.t <- y.t[-c(1,2)]

y.t <- ts(y.t)
acf(y.t, main = "MA(2)")


## AR(1)

set.seed(999)

phi  <- 0.8
y    <- NULL
y[1] <- rnorm(1,0,1) 

for( t in 2:100 ) {
  y[t] <- phi * y[t-1] + rnorm(1,0,1) 
}

y.t <- ts(y)
acf(y.t, main = "AR(1)")


# Ar(2)

set.seed(999)

phi.1 <- 1.5
phi.2 <- -0.75 
est <- c(phi.1 + phi.2, phi.1 + phi.2, abs(phi.2))

y    <- NULL
y[1] <- rnorm(1,0,1)
y[2] <- rnorm(1,0,1)
for( t in 3:100 ) y[t] <- phi.1 * y[t-1] + phi.2 * y[t-2] + rnorm(1,0,1) 

y.t <- ts(y)
autoplot(y.t)
acf(y.t, main = "AR(2)")


# ARMA(1,1)
set.seed(999)
phi  <- 0.8
theta <- 0.9
ruido.blanco <- rnorm(101,0,1)
et <- zlag(ruido.blanco)
ruido.blanco <- ruido.blanco[-1]
et <- et[-1]


y    <- NULL
y[1] <- rnorm(1,0,1)

for(t in 2:101){
  y[t] <- (phi * y[t-1]) - (theta * ruido.blanco[t-1]) + et[t-1]
}
y
yt <- ts(y)
autoplot(yt)
