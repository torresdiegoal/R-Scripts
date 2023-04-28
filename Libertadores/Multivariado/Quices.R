##################
## QUICES ##
##################

install.packages("FactoMineR")
install.packages("factoextra")

library("FactoMineR")
library("factoextra")
library("corrplot")

socioecon <- read.csv("D:/Disco D/R/Datos/Multivariado/Clase 2/SocioEcon.csv", header=T)
str(socioEcon)

socioEcon <- socioecon[,c(2:9)]

se.pca <- PCA(socioEcon, graph = FALSE)
print(se.pca)
## descripción de los nuevos ejes 
se.pca$eig
eig.val <- get_eigenvalue(se.pca)
print(eig.val)

#Grafica que muestra el porcentaje explicado de la varianza de cada nueva dimension
fviz_eig(se.pca, addlabels = TRUE, ylim=c(0,80))

# variables del Analisis de Componentes Principales 
#### $coord
#### $cor
#### $cos2
#### $contrib
h <- se.pca$var
var.<-get_pca_var(se.pca)
print(var.)

#CONTRIBUCIÓN DE LAS VARIABLES EN LOS NUEVOS EJES
head(var.$contrib) #componentes principales DIm1, Dim2, Dim3
corrplot(var.$contrib, is.corr = FALSE)


## grafico de barras que muestra la contribucion de la variables en dim1 y dim2
fviz_contrib(na.pca, choice="var", axes=1:2, top=10)

#CORRELACIÓN ENTRE LAS VARIABLES Y LOS NUEVOS EJES
# (CALIDAD DE LA REPRESENTACIÓN)
head(var.$cos2)
corrplot(var.$cos2, is.corr = FALSE)

# Grafico de barras de la calidad de representación de las variables en los dos principales ejes
fviz_cos2(na.pca, choice="var", axes=1:2, TOP=10)

# Grafica de correlación circular
fviz_pca_var(se.pca, col.var="cos2")
fviz_pca_var(se.pca, col.var="contrib")

