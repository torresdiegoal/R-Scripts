rm(list = ls())


#Paquetes
library(tidyverse)
library(ISwR) # PAQUETE DE TABLAS
library(lattice) ## OTRO TIPO DE GRAFICOS ESTILO ESTANDAR

#Preparación de datos
mtcarss <- mtcars
str(mtcarss)
mtcarss$am <- as.factor(mtcarss$am)
mtcarss$vs <- as.factor(mtcarss$vs)
summary(mtcarss)


#Visualización de datos
boxplot(mtcarss$mpg~mtcarss$cyl)
ggplot(mtcarss, aes(x=factor(cyl), y=mpg, color=factor(cyl)))+geom_boxplot()


#Regresión
#la estructura es var.depend ~ var.indep (numerica ~ factor)
ggplot(mtcarss, aes(x=factor(cyl), y=mpg))+geom_point()

plot(mtcarss$cyl, mtcarss$mpg)
abline(model, col=2)

model <- lm( data = mtcarss, mpg~cyl)
summary(model)

confint(model, 'cyl', level = 0.95)

res <- residuals(model)
mean(res)   ### La media de los residuos es CERO


#####################################
##### Regresiones multilineales #####
#####################################

#Paquetes
library(lattice)
library(calibrate) # calibrate axis in scatterplots
library(gclus) # Clustering graphics, similar a corrplot
library(readxl)

#Preparación de datos
iris   #base de datos de 5 variables
corr <- cor(iris[,-5])  #tabla de coeficientes de correlación de todas las variables menos la 5

my.abs <- abs(corr)
my.abs
my.colors <- dmat.color(my.abs) # gclus Package, obtiene matriz de colores acorde con su imput
my.colors
my.ordered <- order.single(corr)
my.ordered
cpairs(iris[,-5],  panel.colors = my.colors, gap=.5)

multiple <- read_excel()
model <- lm(tiempo~productos+distancia, data = multiple)
summary(model)
X <- cbind(productos, distancia)
betas <- solve(t(X)%*%x)%*%t(X)%*%tiempo

library("scatterplot3d")
s3d <- scatterplot3d(model)
s3d$plane3d(model)  ##añade el plano en donde termina el vector resultante


datos <- read.table("defectuosos.txt", header = T)
setwd("C:/Users/Diego/Desktop")
cor(datos)

model <- lm(Defective~Temperature+Density+Rate, data = datos)
summary(model)
