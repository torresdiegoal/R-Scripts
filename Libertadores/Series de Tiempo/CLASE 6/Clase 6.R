#################################################################
#################################################################
#####   Programa Clase 6 - Series de Tiempo                  ####
#####   Fecha de creación: 20200430                          ####
#####   Fecha de última edición: 20200430                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: edgar.lopez@libertadores.edu.co              ####
#################################################################
#################################################################

rm(list = ls())

#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(TSA)
library(MTS)
library(mvtnorm)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(xtable)
# https://faculty.chicagobooth.edu/ruey.tsay/teaching/mts/sp2015
# http://www.math.ttu.edu/~atrindad/tsdata/index.html
# https://feng.li/files/ts2016spring/TS-L4-MultivariateTimeSeriesModels.R

#################################################################
#####                   Sistema de carpetas                 #####   
#################################################################

inpath  <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"
outpath <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/CLASE 6/"

#################################################################
#####                   Carga de información                #####   
#################################################################

GDP_EEUU   <- read_excel(paste(inpath,"GDP EEUU.xlsx", sep = ""),
                         col_types = c("numeric", "numeric", "numeric","numeric"))
GDP_UKCAUS <- read_excel(paste(inpath,"GDP UK CA US.xlsx", sep = ""))

#################################################################
#####                   Ejecución Programa                  #####   
#################################################################

gdp<- log(GDP_UKCAUS[,3:5])
uk <- ts(diff(gdp$uk), start = c(1980, 1), frequency = 4)
ca <- ts(diff(gdp$ca), start = c(1980, 1), frequency = 4)
us <- ts(diff(gdp$us), start = c(1980, 1), frequency = 4)
gdp.ret <- cbind(uk,ca,us)*100
MTSplot(gdp.ret)
var <- VARorder(gdp.ret)

criterios <- data.frame(cbind(round(0:13,0),var$aic,
                              var$bic,
                              var$hq))
names(criterios) <- c('p', 'AIC', 'BIC', 'HQ')

plot.crite <- data.frame(rbind(cbind(round(0:13,0), 'AIC', var$aic),
                               cbind(round(0:13,0), 'BIC', var$bic),
                               cbind(round(0:13,0), 'HQ', var$hq)))
names(plot.crite) <- c('p', 'Criterio', 'Valores')
plot.crite$p       <- as.numeric(as.character(plot.crite$p)) 
plot.crite$Valores <- as.numeric(as.character(plot.crite$Valores)) 

ggplot(plot.crite, aes(x = p,y = Valores, group = Criterio)) + 
  geom_line(aes(color = Criterio))

modelo <- VAR(gdp.ret, 2)

### Residuales
names(modelo)
resi <- modelo$residuals 
mq(resi,adj=18)

### Simplificación del modelo
modelo.2 <- refVAR(modelo, thres=1.96) # Model refinement.
MTSdiag(modelo.2,adj=12)

resi.2 <- modelo.2$residuals 
mq(resi.2, adj=18)

### Pronóstico
VARpred(modelo.2, 8)

### Funciones de impulso
Phi <- modelo.2$Phi 
Sig <- modelo.2$Sigma
VARirf(Phi,Sig) 

### Descompisición de la varianza
Theta <- NULL
FEVdec(Phi,Theta,Sig,lag=5)




