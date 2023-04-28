rm(list = ls())

#########################
## TALLER 3 REGRESIONES
### Analisis residual

library(dplyr)

#Hacer dataframes una gonorrea    
df <- data.frame(x=rnorm(1000,mean=rep(c(0,3,6,9),each=250)),
                 y=rep(c("Staff","Mgt"),each=500),
                 z=rep(c("High","Low"),each=250))

# Y plotear sus histogramas aun mas gonorrea
par(mfrow=c(2,2),mar=c(3,4,1,1))
lapply(split(df,list(df$y,df$z)),
       function(d)hist(d$x,main=paste(unique(d$y),unique(d$z),sep=".")))

## Este grafico es basicamente lo mismo pero en ggplot
library(ggplot2)
ggplot(df) + 
  geom_histogram(aes(x=x,fill=interaction(z,y)),color="grey70")+
  facet_grid(z~y)

### Ahora, ejercicio con T3
library(lmtest)
library(car)
library(readxl)

setwd("D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Regresiones/regresiones")
T3 <- read_excel("T3.xlsx")
attach(T3)


### Punto 1
#Regresión multivariada
model <- lm(y~., data = T3)


#¿Normalidad en los residuos? 
hist(resid(model), col = "pink", border = "white", 
     main = "Histograma de residuos", xlab = "Residuos", freq = F)

par(mfrow = c(1,1))
qqPlot(resid(model), col = "orangered", col.lines = "black", ylab = "std residuals",
       lwd = 1, grid = T, pch = 1, main =  " Q-Q Plot")

# Aqui desmenuza el grafico anterior
qqnorm(resid(model), main = " Q-Q Plot", col = "orange")
qqline(resid(model), col = "black", lwd = 0.5)


## H_0:Las muestras provienen de una distribución normal
## H_1:Las muestras no provienen de una distribución normal
shapiro.test(resid(model)) # p valor 0.36, son normales


### Punto 2
#Gráfico residual
plot(fitted(model), resid(model), col = "orangered",
      pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals")
abline(h = 0, lty = 2, col = "black", lwd = 2)

# Breusch-Pagan para Homoscedasticidad
## H_0:Hay homoscedasticidad en los errores
## H_1:No hay homoscedasticidad en los errores
bptest(model)
    
    
### Punto 3
# Residuos estandarizados
rstandard(model)

# Residuos estudentizados
rstudent(model)

