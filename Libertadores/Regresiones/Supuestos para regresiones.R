library(lmtest)
library(MASS)
library(faraway)

initech <- read_excel("Initech.xlsx")

plot(salario ~ a�os, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority", data= initech)
initech_fit = lm(salario ~ a�os, data = initech)
abline(initech_fit, col = "darkorange", lwd = 2)
# Hasta ahí, es un buen análisis? 
summary(initech_fit)

#Realicemos un análisis descriptivo de los residuos
par(mfrow = c(1, 2))
#Residuos vs valores ajustados
plot(fitted(initech_fit), resid(initech_fit), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
# Note la forma de "cono". Según algunos autores, esto "grita" heteroscedasticidad.
#Normalidad en los residuos? 
qqnorm(resid(initech_fit), main = " Q-Q Plot", col = "darkgrey")
qqline(resid(initech_fit), col = "dodgerblue", lwd = 2)


# Mal modelo. 😞 

## Es necesario transformar variables para poder estabilizar varianza y reducir el sesgo de no-normalidad 
# Consideremos la transformación natural (log)

initech_fit_log = lm(log(salario) ~ a�os, data = initech)

plot(log(salario) ~ a�os, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
abline(initech_fit_log, col = "darkorange", lwd = 2)

plot(salario ~ a�os, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
curve(exp(initech_fit_log$coef[1] + initech_fit_log$coef[2] * x),
      from = 0, to = 30, add = TRUE, col = "darkorange", lwd = 2)

par(mfrow = c(1, 2))
#Realicemos un análisis descriptivo de los residuos para este nuevo modelo
plot(fitted(initech_fit_log), resid(initech_fit_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
#Normalidad en los residuos? 
qqnorm(resid(initech_fit_log), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(initech_fit_log), col = "dodgerblue", lwd = 2)

# Qué dicen las pruebas de hipótesis? (Residuos originales)
#Normalidad
shapiro.test(resid(initech_fit_log))
#Homoscedasticidad
bptest(initech_fit_log)

#Independencia 
dwtest(initech_fit_log)

#Mucho mejor. 


## Otra forma de notar problemas
# Uno de los supuestos pedía distribución normal de media cero en los errores. 
sqrt(mean(resid(initech_fit) ^ 2))
# Recordemos que los supuest

11:17

## Otra forma de notar problemas
# Uno de los supuestos pedía distribución normal de media cero en los errores. 
sqrt(mean(resid(initech_fit) ^ 2))
# Recordemos que los supuestos en los errores, se verifican en los residuos
sqrt(mean(resid(initech_fit_log) ^ 2))
# El resumen del modelo corregido 
summary(initech_fit_log)
### MUCHO CUIDADO EN LA FORMA QUE SE INTERPRETA ESTE MODELO. 


## Ejemplos (BOX-COX)
savings_model = lm(sr ~ ., data = savings)   ## el punto indica el resto
## de variables independientes
# Ver cran.r-project.org/web/packages/faraway/faraway.pdf

##Forma rápida de ver la validación de supuestos
# Residuos vs ajustados (+ línea de tendencia)
plot(savings_model, 1)
# Qqplot
plot(savings_model, 2)
# Residuos estandarizados vs ajustados (+ línea de tendencia)
plot(savings_model, 3)
# Distancia de Cook
plot(savings_model, 4)
## O sencillamente
par(mfrow=c(2,2))
plot(savings_model)
## Las pruebas
#Normalidad
shapiro.test(resid(savings_model))
#Homoscedasticidad
bptest(savings_model)
#Independencia 
dwtest(savings_model)
### EL modelo ya es bueno, pero veamos qué ocurre con la transformación de Box-Cox 
par(mfrow=c(1,1))
#La función tiene buenas intenciones
bc<- boxcox(savings_model, plotit = TRUE)
#Pero de vez en vez hay que ayudarle
bc<- boxcox(savings_model, plotit = TRUE, lambda = seq(0.5, 1.5, by = 0.1))
# El valor de la transformación es muy cercano a 1. 
(lambda_est<- bc$x[bc$y==max(bc$y)])
# Es decir que habría que montar el modelo con 
y_trans<- (savings$sr^lambda_est-1)/lambda_est
savings_model_bc = lm(y_trans ~ ., data = savings[,-1])

# Residuos vs ajustados (+ línea de tendencia)
plot(savings_model_bc, 1)
# Qqplot
plot(savings_model_bc, 2)
# Residuos estandarizados vs ajustados (+ línea de tendencia)
plot(savings_model_bc, 3)
# Distancia de Cook
plot(savings_model_bc, 4)
## O sencillamente
par(mfrow=c(2,2))
plot(savings_model_bc)
## Las pruebas
#Normalidad
shapiro.test(resid(savings_model_bc))
#Homoscedasticidad
bptest(savings_model_bc)
#Independencia 
dwtest(savings_model_bc)



### OTRO EJEMPLO



#ver cran.r-project.org/web/packages/faraway/faraway.pdf
par(mfrow=c(1,1))
gala_model = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
plot(fitted(gala_model), resid(gala_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
## Forma de "cono".
par(mfrow=c(2,2))
plot(gala_model)
## Las pruebas
#Normalidad
shapiro.test(resid(gala_model))
#Homoscedasticidad
bptest(gala_model)
#Independencia 
dwtest(gala_model)
par(mfrow=c(1,1))

###El modelo está mal. Habrá que "dar un poquito de palo" para que mejore. 
## Busquemos el lambda. 
bc<- boxcox(gala_model, lambda = seq(-0.25, 0.75, by = 0.05), plotit = TRUE)
(lambda_est<- bc$x[bc$y==max(bc$y)])
#Se define la nueva variable
Species_bc<- ((gala$Species ^ lambda_est) - 1) / lambda_est
#El nuevo modelo
gala_model_cox = lm(Species_bc ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
# Mucho mejor


# Retorna valores predecidos por la regresi�n en un 
#intervalo de valores de la variable predictoria
nuevas.edades <- data.frame(edad = seq(30, 50))
predict(regresion, nuevas.edades)
            
            
            
            
            