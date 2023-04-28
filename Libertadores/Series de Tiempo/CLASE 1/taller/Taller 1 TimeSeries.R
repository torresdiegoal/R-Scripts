rm(list = ls())

#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(TSA)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggfortify)   #el de autoplot
library(tseries)
library(forecast)


inpath  <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/Datos/"
outpath <- "D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Series de Tiempo/CLASE 1/taller/"

ton <- read.csv(paste(inpath,"Exportaciones.csv", sep = ""), sep = ";")

ton.ts <- ts(ton$ton.metricas, start = c(1992,1), frequency = 12)
str(ton.ts)

adf.test(ton.ts)

plot(ton.ts,type='o',ylab='Random Walk')
#autoplot(rwalk)
#ggplot(rwalk, aes(x=rwalk, y=zlag(rwalk))) + 
# geom_point() 

modelo <- lm(ton.ts ~ time(ton.ts) )
summary(modelo)
abline(modelo)


autoplot(ton.ts, s.window = 'periodic')
autoplot(stl(tempdub, s.window = 'periodic'), ts.colour = 'blue')


