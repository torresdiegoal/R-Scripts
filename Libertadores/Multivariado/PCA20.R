install.packages("FactoMineR")
install.packages("factoextra")
library("FactoMineR")
library("factoextra")
library("corrplot")


orange <- read.table("D:/Disco D/R/Datos/Multivariado/Clase 2/orange.csv", header=TRUE, sep =";",
                      row.names=1)
#descriptiva
View(orange)
str(orange)
names(orange)
summary(orange)
colMeans(orange)
apply(orange,2,mean)

#nueva Base
naranja <- orange[,c(1:14)]
str(naranja)
#matriz de correlacion
cor(naranja)
#matriz de covarianza y correlacion
cov(naranja)

#seleccion de variables
naranja. <- orange[,c(1:8)]

#matriz de correlacion
cor.naranja<-cor(naranja.)
#matriz de covarianza
cov.naranja<-cov(naranja.)


################################
# Funciones iniciadas con fviz #
#       son graficas
################################



###### Analisis de componentes principales
na.pca<-PCA(naranja., graph = FALSE)
print(na.pca)
fviz_eig(na.pca, addlabels = TRUE, ylim=c(0,80))

## variables del Analisis de Componentes Principales 
#### $coord
#### $cor
#### $cos2
#### $contrib
var.<-get_pca_var(na.pca)

#coordenadas
head(var.$coord)

#CONTRIBUCIÓN DE LAS VARIABLES EN LOS NUEVOS EJES
head(var.$contrib) #componentes principales DIm1, Dim2, Dim3
corrplot(var.$contrib, is.corr = FALSE)

## grafico de barras que muestra la contribucion de la variables en Dim1
fviz_contrib(na.pca, choice="var", axes=1, top=10)


## grafico de barras que muestra la contribucion de la variables en dim1 y dim2
fviz_contrib(na.pca, choice="var", axes=1:2, top=10)

#CORRELACIÓN ENTRE LAS VARIABLES Y LOS NUEVOS EJES
# (CALIDAD DE LA REPRESENTACIÓN)
head(var.$cos2)
corrplot(var.$cos2, is.corr = FALSE)

# Grafico de barras de la calidad de representación de las variables en los dos principales ejes
fviz_cos2(na.pca, choice="var", axes=1:2, TOP=10)

# Grafica de correlación circular
fviz_pca_var(na.pca, col.var="cos2")
##################

dimdesc<-dimdesc(na.pca, axes=c(1,2), proba=0.05)
dimdesc$Dim.1
dimdesc$Dim.2

####plot
###coeficientes de correlacion entre cada una de las variables y los primeros
### componentes 
na.pca$var$coord[,1:3]
fviz_pca_var(na.pca, col.var = "black")

## descripción de los nuevos ejes 
na.pca$eig
## Los valores anteriores, pero organizados en un DataFrame
eig.val<-get_eigenvalue(na.pca)

#distancia de los individuos al centro de la nube
na.pca$ind$dist

#contribucion de los individuos en la construcion de los ejes
na.pca$ind$contrib[,1:3]

###contribucion de las variables en la construccion de los ejes
na.pca$var$contrib[,1:3]#componentes principales P1, P2, P3

###Individuos
plot.PCA(na.pca, choix="ind")
plot.PCA(na.pca, choix="var")

#por dimension
plot.PCA(na.pca, choix="ind",axes=2:3)
plot.PCA(na.pca, choix="var",axes=2:3)

cbind(na.pca$ind$coord[,1:3], na.pca$ind$cos2[,1:3],na.pca$ind$contrib[,1:3])

#contribcion de los individuos
fviz_pca_ind(na.pca, col.ind = "cos2")
fviz_contrib(na.pca, choice = "ind", axes=1:2)

##############################################
#### PCA mediante la matriz de correlacion ###
##############################################
cor.nar<-cor(naranja.)
eig.val.<-eigen(cor.nar)
eig.val.$values # Valor propio desde la matriz de correlaciÃ³n
eig.val.$vectors# Vector propio
Traza<-sum(diag(cor.nar))
Vt<-(c(eig.val.$values)/Traza)*100
View(naranja.)

##Vectores propios
res.pca <- prcomp(naranja., scale = TRUE)
#nuevas variables CP1
res.pca$x
res.pca$x

v.1<-eig.val.$vectors[,1]
x.1<-naranja.[1,]
media<-apply(naranja.,2,mean)
sd<-apply(naranja.,2,sd)
z=(x.1-media)/sd
PC1<-sum(v.1*z)






















