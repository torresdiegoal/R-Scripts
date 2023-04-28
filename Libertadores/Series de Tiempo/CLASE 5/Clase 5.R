#################################################################
#################################################################
#####   Programa Clase 5 - Series de Tiempo                  ####
#####   Fecha de creación: 20200424                          ####
#####   Fecha de última edición: 20200424                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: edgar.lopez@libertadores.edu.co              ####
#################################################################
#################################################################

rm(list = ls())

#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(MTS)
library(mvtnorm)
library(ggplot2)
library(readxl)
library(xtable) # Tablas en latex
# https://faculty.chicagobooth.edu/ruey.tsay/teaching/mts/sp2015
# http://www.math.ttu.edu/~atrindad/tsdata/index.html
# https://feng.li/files/ts2016spring/TS-L4-MultivariateTimeSeriesModels.R

#################################################################
#####                   Sistema de carpetas                 #####   
#################################################################

inpath  <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"
outpath <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/CLASE 5/"

#################################################################
#####                   Carga de información                #####   
#################################################################

GDP_EEUU   <- read_excel(paste(inpath,"GDP EEUU.xlsx", sep = ""),
                         col_types = c("numeric", "numeric", "numeric","numeric"))
GDP_UKCAUS <- read_excel(paste(inpath,"GDP UK CA US.xlsx", sep = ""))

#################################################################
#####                   Ejecución Programa                  #####   
#################################################################

head(GDP_EEUU)
MTSplot(GDP_EEUU) 
gdp  <- ts(diff(GDP_EEUU$gdp), start = c(1948, 4), frequency = 12)
rate <- ts(diff(GDP_EEUU$rate), start = c(1948, 4), frequency = 12)
GDP.DIFF <- cbind(gdp, rate)
MTSplot(GDP.DIFF)
#mq(GDP.DIFF,lag=10) 


#### Ejemplo CCM
sig <- diag(2) 
Y   <- rmvnorm(300,rep(0,2),sig)
MTSplot(Y) 
ccm(Y)


#### Ejemplo Ljung-Box
sig <- diag(3) 
Z   <- rmvnorm(200,rep(0,3),sig)
MTSplot(Z) 
mq(Z,10)


#### Especificación

gdp     <- log(GDP_UKCAUS[,3:5])
uk <- ts(diff(gdp$uk), start = c(1980, 1), frequency = 4)
ca <- ts(diff(gdp$ca), start = c(1980, 1), frequency = 4)
us <- ts(diff(gdp$us), start = c(1980, 1), frequency = 4)
gdp.ret <- cbind(uk,ca,us)
MTSplot(gdp.ret)
criterios <- data.frame(cbind(round(0:13,0),VARorder(gdp.ret)$aic,
                              VARorder(gdp.ret)$bic,
                              VARorder(gdp.ret)$hq))
names(criterios) <- c('p', 'AIC', 'BIC', 'HQ')
#xtable(criterios)






