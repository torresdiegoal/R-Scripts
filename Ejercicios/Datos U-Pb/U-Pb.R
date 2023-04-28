library(readxl)
library(psych)
library(ggplot2)
library(moments)
U_Pb <- read_excel("C:/Users/Diego/Desktop/U_Pb.xlsx")
View(U_Pb)

#función moda
mode <- function(x) 
{ux <- unique(x)
ux[which.max(tabulate(match(x, ux)))]
}

#Función coeficiente de variación
cv <- function(x, y){
  ux <- (y/x)*100
}


#Descripción variable concentración Uranio (ppm)
mean_U <- mean(U_Pb$U_ppm)
median_U <- median(U_Pb$U_ppm)
var_U <- var(U_Pb$U_ppm)
quartiles_U <- quantile(U_Pb$U_ppm)
rango_U <- max(U_Pb$U_ppm) - min(U_Pb$U_ppm)
IQR_U <- IQR(U_Pb$U_ppm)
sd_U <- sd(U_Pb$U_ppm)
var_U <- var(U_Pb$U_ppm)
mode_U <- mode(U_Pb$U_ppm)
cv_U <- cv(mean_U, sd_U)
skew_U <- skewness(U_Pb$U_ppm)
kurt_U <- kurtosis(U_Pb$U_ppm)

#Descripción variable edades en Pb206/Pb207
mean_Pbx2 <- mean(U_Pb$`206Pb_207Pb`)
median_Pbx2 <- median(U_Pb$`206Pb_207Pb`)
var_Pbx2 <- var(U_Pb$`206Pb_207Pb`)
quartiles_Pbx2 <- quantile(U_Pb$`206Pb_207Pb`)
rango_Pbx2 <- max(U_Pb$`206Pb_207Pb`) - min(U_Pb$`206Pb_207Pb`)
IQR_Pbx2 <- IQR(U_Pb$`206Pb_207Pb`)
mode_Pbx2 <- mode(U_Pb$`206Pb_207Pb`)
sd_Pbx2 <- sd(U_Pb$`206Pb_207Pb`)
var_Pbx2 <- var(U_Pb$`206Pb_207Pb`)
cv_Pbx2 <- cv(mean_Pbx2, sd_Pbx2)
skew_Pbx2 <- skewness(U_Pb$`206Pb_207Pb`)
kurt_Pbx2 <- kurtosis(U_Pb$`206Pb_207Pb`)

#Descripción variable edades en Pb206/U238.
mean_Pb_U <- mean(U_Pb$`206Pb_238U`)
median_Pb_U <- median(U_Pb$`206Pb_238U`)
var_Pb_U <- var(U_Pb$`206Pb_238U`)
quartiles_Pb_U <- quantile(U_Pb$`206Pb_238U`)
rango_Pb_U <- max(U_Pb$`206Pb_238U`) - min(U_Pb$`206Pb_238U`)
IQR_Pb_U <- IQR(U_Pb$`206Pb_238U`)
sd_Pb_U <- sd(U_Pb$`206Pb_238U`)
mode_Pb_U <- mode(U_Pb$`206Pb_238U`)
var_Pb_U <- var(U_Pb$`206Pb_238U`)
cv_Pb_U <- cv(mean_Pb_U, sd_Pb_U)
skew_Pb_U <- skewness(U_Pb$`206Pb_238U`)
kurt_Pb_U <- kurtosis(U_Pb$`206Pb_238U`)

#histograma general de las variables
multi.hist(U_Pb, bcol = "snow2", dcol = c("red","black"), mar=c(2,1,1,1)+.6, main = c("U (ppm)","206Pb/207Pb","206Pb/238U"))
hist(U_Pb$U_ppm,  main = "Distribución Notas", xlab= "Cases", ylab = "Frequency", col = "pink", border = "white")

#Boxplots individuales para cada variable
par(mar=c(6, 6.1, 3.1, 13.1))
boxplot(U_Pb$U_ppm, main = "Concentración Uranio (ppm)", ylab = "concentración (ppm)", col = "snow2", boxwex = 0.5, frame = F, xaxt="n" )

par(mar=c(6, 6.1, 3.1, 13.1))
boxplot(U_Pb$`206Pb_207Pb`, main = "Edades por Pb206/Pb207", ylab = "edades (Ma)", col = "snow2", boxwex = 0.5, frame = F, xaxt="n" )

par(mar=c(6, 6.1, 1.1, 13.1))
boxplot(U_Pb$`206Pb_238U`, main = "Edades por Pb206/U238", ylab = "edades (Ma)", col = "snow2", boxwex = 0.5, frame = F, xaxt="n" )
