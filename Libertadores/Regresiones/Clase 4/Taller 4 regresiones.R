rm(list = ls())

##########################
## TALLER 4 REGRESIONES #
### Regresion aplicada

library(lmtest)
library(car)
library(readxl)

setwd("D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Regresiones/regresiones")
T4 <- read_excel("T4.xlsx")
attach(T4)

model <- lm(y ~ x1 + x6)
summary(model)

# Distanca de Cook
plot(model, 4)

## Analisis de Comparacion de Varianzas
anova(model)

# Intervalo de confianza para los coeficientes del modelo
model$coefficients # coeficientes del modelo
confint(model)
confint(model)[2,] # interv para solamente x1
model

confint(T4$x1)
str(T4)


#¿Normalidad en los residuos? 
qqPlot(resid(model), col = "orangered", col.lines = "black", ylab = "std residuals",
       lwd = 1, grid = T, pch = 1, main =  " Q-Q Plot")

#Gráfico residual
plot(fitted(model), resid(model), col = "orangered",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals")
abline(h = 0, lty = 2, col = "black", lwd = 2)

#Normalidad
shapiro.test(resid(model)) 

#Homoscedasticidad
bptest(model)

#Covarianza
dwtest(model)

par(mfrow=c(2,2))
plot(model)

#Estimar intervalo de confianza de un valor puntual
predict(model, data.frame(x1 = 275 , x6 = 2), interval = "confidence")

#Estimar intervalo de predicción de un valor puntual
predict(model, data.frame(x1 = 275 , x6 = 2), interval = "prediction")


###############################################################
# EJEMPLO GRAFICOS CON INTERVALOS DE CONFIANZA Y DE PREDICCIÓN
###############################################################

grasas <- read.table('http://verso.mat.uam.es/~joser.berrendero/datos/EdadPesoGrasas.txt', header = TRUE)
nuevas.edades <- data.frame(edad = seq(20, 60))
regresion <- lm(grasas ~ edad, data = grasas)
# Grafico de dispersion y recta
plot(grasas$edad, grasas$grasas, xlab='Edad', ylab='Grasas')
abline(regresion)

# Intervalos de confianza de la respuesta media:
# ic es una matriz con tres columnas: la primera es la prediccion, las otras dos son los extremos del intervalo
ic <- predict(regresion, nuevas.edades, interval = 'confidence')
lines(nuevas.edades$edad, ic[, 2], lty = 2)
lines(nuevas.edades$edad, ic[, 3], lty = 2)

# Intervalos de prediccion
ic <- predict(regresion, nuevas.edades, interval = 'prediction')
lines(nuevas.edades$edad, ic[, 2], lty = 2, col = 'red')
lines(nuevas.edades$edad, ic[, 3], lty = 2, col = 'red')


# Plotear bien chimbita los diagramas residuales con intervalos de confi. y de predic.
library("ggplot2")
p <- ggplot(mydata, aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")



############
# PARTE II
###########

library(readxl)
T4_2 <- read_excel("T4_2.xlsx")
attach(T4_2)

library("ggplot2")
p <- ggplot(T4_2, aes(Xi, Yi)) +
  geom_point()
p

model <- lm(Yi~Xi)
summary(model)
plot(Xi, Yi, col = "orangered",
     pch = 20, cex = 2, xlab = "Xi", ylab = "Yi", main = "Scatterplot")
abline(model, col = "black", lwd = 1)

install.packages("fpp") # Data package from Book Forecasting, principles and practice
                          # by Hyndman & Athanasopoulos
library(fpp)
library(forecast)

library(MASS)
library(faraway)

bc<- boxcox(model, plotit = TRUE, lambda = seq(0.5, 1.5, by = 0.1))

# El valor que maximiza la función de maxima verosimilitud. 
(lambda_est<- bc$x[bc$y==max(bc$y)])

# genera nuevas predicciones de y con el valor de lambda transformado
y_trans<- (Yi^lambda_est-1)/lambda_est

# nuevo modelo con los datos transformados
model_bc = lm(y_trans ~ ., data = T4_2)
summary(model_bc)


model_log = lm(log10(Yi) ~ Xi, data = T4_2)

summary(model_log)

plot(Xi, log10(Yi), col = "orangered",
     pch = 20, cex = 2, xlab = "Xi", ylab = "log10(Yi)", main = "Scatterplot")
abline(model_log, col = "black", lwd = 1)


#Gráfico residual
plot(fitted(model_log), resid(model_log), col = "orangered",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals")
abline(h = 0, lty = 2, col = "black", lwd = 2)

#¿Normalidad en los residuos? 
qqPlot(resid(model_log), col = "orangered", col.lines = "black", ylab = "std residuals",
       lwd = 1, grid = T, pch = 1, main =  " Q-Q Plot")
