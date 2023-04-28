rm(list = ls())

new_route <- "C:/Users/Diego/Desktop/proyecto regresiones"
setwd(new_route)


felicidad <- read.csv("C:/Users/Diego/Desktop/proyecto regresiones/felicidad.csv")
suicidios <- read.csv("C:/Users/Diego/Desktop/proyecto regresiones/suicidios.csv")
terrorismo <- read.csv("C:/Users/Diego/Desktop/proyecto regresiones/terrorismofixed.csv")

borrar <- c("approxdate","extended", "resolution", "attacktype2","alternative",
            "attacktype2_txt", "attacktype3", "specifity", "vicinity", "location",
            "attacktype3_txt","targtype2","targtype2_txt","targsubtype2_txt","targsubtype2",
            "targtype3_txt","targtype3", "natlty2_txt","natlty2","corp2","target2","targsubtype3_txt","targsubtype3",
            "natlty3_txt","natlty3","corp3","target3","gsubname","gsubname2","gsubname3","gname2","gname3","guncertain1","guncertain2","guncertain3",
            "individual","nperps","nperpcap","claimed","claimmode","claimmode_txt","claimmode2_txt","claimmode2","claim2","claimmode3","claimmode3_txt",
            "claim3","compclaim","weapsubtype2","weapsubtype2_txt","weaptype2","weaptype2_txt","weapsubtype3","weapsubtype3_txt","weaptype3","weaptype3_txt",
            "weapsubtype4","weapsubtype4_txt","weaptype4","weaptype4_txt","ishostkid","nhostkid","nhostkidus","nhours","ndays","divert","kidhijcountry","ransom",
            "ransomamt","ransomamtus","ransompaid","ransompaidus","ransomnote","addnotes","nreleased","hostkidoutcome_txt","hostkidoutcome","related")
terrorismo <- terrorismo[ , !(names(terrorismo) %in% borrar)]






#15-24 years
suicid_X <- subset(suicidios, generation == "Generation X")

#25-34 years
suicid_Boom <- subset(suicidios, generation == "Boomers")

#35-54 years
suicid_Silent <- subset(suicidios, generation == "Silent")

#55-74 years
suicid_GI1 <- subset(suicidios, age == "55-74 years")

#75+ years
suicid_GI2 <- subset(suicidios, age == "75+ years")


#######################################

###### AQUI EMPIEZA EL PROYECTO ######

#######################################

library(lmtest)
library(tseries)

datos <- read.csv("C:/Users/Diego/Desktop/proyecto regresiones/union_bases.csv")
borrar <- c("Happ_MaxRank", "Country")
datos <- datos[ , !(names(datos) %in% borrar)]
write.csv(datos, file = "datos", row.names = F)

datos <- read.csv("C:/Users/Diego/Desktop/proyecto regresiones/datos")
attach(datos)


names(datos)

suicid <- lm(log(suicides_prom_anual)~ popul_prom + HappynessScore + GDPperCap + 
               Family + Generosity + Trust_GovCorruption + FreedomMD )

suicid1 <- lm(suicides_prom_anual~ popul_prom + HappynessScore + 
               GDPperCap + Family + Generosity + Trust_GovCorruption + FreedomMD)
summary(suicid)

step(suicid, direction = "both", trace = 0)

######modelo plot


plot(pobl, xlab='Edad', ylab='Grasas')
abline(pobl)
# Intervalos de confianza de la respuesta media:
# ic es una matriz con tres columnas: la primera es la prediccion, las otras dos son los extremos del intervalo
ic <- predict(suicid, nuevas.edades, interval = 'confidence')
lines(nuevas.edades$edad, ic[, 2], lty = 2)
lines(nuevas.edades$edad, ic[, 3], lty = 2)

# Intervalos de prediccion
ic <- predict(regresion, nuevas.edades, interval = 'prediction')
lines(nuevas.edades$edad, ic[, 2], lty = 2, col = 'red')
lines(nuevas.edades$edad, ic[, 3], lty = 2, col = 'red')



plot(HappynessScore,suicides_prom_anual,
     main = "Absolute studentized residuals vs predicted values", pch = 20,
     col = "grey30", ylab = "N° Suicidios")
abline(a = 2.573, b = 4.436, h = 2.573, lty = 4.436, col = "orangered")

plot(popul_prom,suicides_prom_anual,
     main = "Absolute studentized residuals vs predicted values", pch = 20,
     col = "grey30", ylab = "N° Suicidios")
abline(a = 2.573, b = 2.481e02, h = 2.573, lty = 4.436, col = "orangered")




#Nos dice con cuales variables quedarnos para un buen modelo
step(suicid1, direction = "both", trace = 0)

#Plot de correlaciones a color, bien bonito
install.packages("corrplot")
require(corrplot)
par(mfrow=c(1,1))
corrplot.mixed(corr = cor(datos[,c("suicides_prom_anual","popul_prom", "HappynessScore",
                                   "GDPperCap",  "Family","Generosity","Trust_GovCorruption","DystopiaResidual")],
                          method = "pearson"))

require(car)
vif(suicid)


library("ggplot2")
p <- ggplot(datos, aes(fitted(suicid), rstandard(suicid))) +
  geom_point() +
  stat_smooth(method = lm)

p
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

#Normalidad
jarque.bera.test(resid(suicid))

#Homoscedasticidad
bptest(suicid)

#Covarianza
dwtest(suicid)




library(fpp)
library(forecast)

library(MASS)
library(faraway)


bc<- boxcox(suicid, plotit = TRUE, lambda = seq(0.5, 1.5, by = 0.1))

# El valor que maximiza la función de maxima verosimilitud. 
(lambda_est<- bc$x[bc$y==max(bc$y)])

# genera nuevas predicciones de y con el valor de lambda transformado
y_trans<- (suicides_prom_anual^lambda_est-1)/lambda_est

# nuevo modelo con los datos transformados
model_bc = lm(y_trans ~ .- X -country - Health_LifeExpectancy, data = datos)
summary(model_bc)
