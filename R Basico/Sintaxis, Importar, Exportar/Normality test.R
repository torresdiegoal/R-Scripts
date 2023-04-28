rm(list = ls())

########################################################
### HOW TO MEASURE NORMALITY IN DIFERENT TYPES OF DATA
########################################################

library(kableExtra) #Necessary to Install, not to load, for using MVN
library(MVN) # Assess multivariate normality
library(readxl)
library(dplyr)
library(psych) ## multi.hist

geoquimica <- read_excel("D:/OneDrive - Universidad de Los Andes/R scripts/R Basico/Datos/Geoquimica_Adri.xlsx",
                   sheet = "Data")
summary(geoquimica)
geoquimica <- geoquimica[-1] # remueve la primera columna
rowSums(is.na(geoquimica))

# Histograma multiple 
multi.hist(geoquimica)

# Ahora bien, MVN 
normality_raw <- mvn(geoquimica)# Performs multivariate normality tests, including Marida, 
# Royston, Henze-Zirkler, Dornik-Haansen, E-Statistics, and graphical approaches and implements
# multivariate outlier detection and univariate normality of marginal distributions through
# plots and tests, and performs multivariate Box-Cox transformation.
normality_sqrt <- mvn(sqrt(geoquimica)) 
normality_ln <- mvn(log(geoquimica)) 
# Vamos a comparar el test de normalidad de los raw data y el cuadrado de la misma.
## Esto con el proposito de ver si la transformacion afecta en la normalidad o se requieren
# transformaciones adicionales
A <- normality_raw$univariateNormality
colnames(A) <- c("Test", "Variable", "Statistic", "p value Raw", "Normality Raw")
head(A)
A_NotNorm <- A %>% filter(`p value Raw` > 0.05)

B <- normality2$univariateNormality
colnames(B) <- c("Test", "Variable", "Statistic", "p value Sqrt", "Normality Sqrt")

normality2$multivariateNormality
normality2$Descriptives

# Unimos en un dataframe para observar, cualquiera de estas formas genera el mismo resultado
AB <- full_join(A, B, by = c("Test", "Variable"))
AB1 <- merge(A, B, by = c("Test", "Variable"))

AB[,c(-3,-6)]
### CONCLUSION ##
# 

class(A[1,5])

library(moments) # Functions to calculate: moments, Pearson's kurtosis,
# Geary's kurtosis and skewness; tests related to them
# (Anscombe-Glynn, D'Agostino, Bonett-Seier).