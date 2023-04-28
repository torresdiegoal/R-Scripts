rm(list = ls())

install.packages("MVN")
install.packages("kableExtra")
install.packages("gdtools") #Necessary to MVN

library(kableExtra) #Necessary to MVN
library(MVN) # Assess multivariate normality
library(readxl)
library(dplyr)
library(psych) ## multi.hist

data <- read_excel("C:/Users/Diego/Downloads/Ejercicio_UD.5.xlsx",
                   sheet = "Data")
summary(data)
data <- data[-1]

multi.hist(data)

normality <- mvn(data)
normality$univariateNormality