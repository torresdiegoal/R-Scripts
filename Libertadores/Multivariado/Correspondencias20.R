install.packages("FactoMineR")
install.packages("factoextra")
library("FactoMineR")
library("factoextra")

install.packages("ca")
library(ca)

#orange <- read.table("PCA/orange.csv", header=TRUE, sep =";",
#                     row.names=1)
rm(list = ls())


tabla<-matrix(c(688,116,584,188,4,
                326,38,241,110,3,
                343,84,909,412,26,
                98,48,403,681,85),ncol=5,byrow = TRUE)

dimnames(tabla) <-list(ojos=c("claro","azules","castaños","oscuro"),
                       pelo=c("rubio","pelirrojo","castaño","oscuro","negro"))

str(tabla)

#####Test X2

#*Asociación
#a probar la asociación entre dos variables cualitativas medidas en una muestra
#prueba de chi-cuadrado X2
# Ho:las dos variables son independientes
# Ha:Las dos variable no son independientes

test.x2<-chisq.test(tabla)
test.x2

##Tablas

addmargins(tabla)

#Perfiles-filas y Perfiles columnas
margin.table(tabla, 1)# filas
margin.table(tabla, 2)# columna

round(prop.table(tabla),3)#frecuencia relativa
round(prop.table(tabla,1),3)# P(C|F)=(C y F)/F
round(prop.table(tabla,2),3)# P(F|C)=(C y F)/C



#Descomposición de x2 por celdas,filas y columnas, Cuando el efecto observado es inferior al efecto teorico
#añadimos  el signo (-) a cada valor

test.x2$observed
test.x2$expected
test.x2$residuals^2

test.x2$residuals^2/test.x2$stdres

########
##ANALISIS DE CORRESPONDENCIAS
#######

res.ca<-CA(tabla)
summary(res.ca)
plot(res.ca, invisible = "col")
plot(res.ca, invisible = "row")


##Representación de los baricentris exactos 
plot(res.ca, invisible = "col")
coord.col<-sweep(res.ca$col$coord,2,sqrt(res.ca$eig[,1]),FUN="*")
points(coord.col, pch=17, col="red")

plot(res.ca, invisible = "row")
coord.row<-sweep(res.ca$row$coord,2,sqrt(res.ca$eig[,1]),FUN="*")
points(coord.row, pch=17, col="red")


##Valores propios
res.ca$eig
barplot(res.ca$eig[,1],main="Valores propios", names.arg=1:nrow(res.ca$eig))

##tabla de contribuciones
cbind(res.ca$row$coord,res.ca$row$contrib,res.ca$row$cos2)#filas
cbind(res.ca$col$coord,res.ca$col$contrib,res.ca$col$cos2)#columnas


#Matriz F de frecuencia relativa 
F.<-tabla # F es la tabla de contigencia
#matriz R: matriz de frecuencia relativa al total de filas
#filas
filas<-margin.table(F., 1)
#col
col<-margin.table(F., 2)

############ANALISIS MULTIVARTIADO############
##############################################
#FILA 
#diagonal
dig.f<-diag(filas)
dig.c<-diag(col)
###MATRIZ DE FRECUENCIA RELATIVA CONDICIONADA AL TOTAL DE LA FILA
Rf<-solve(dig.f)%*%F.
##MATRIZ DE FRECUENCIA RELATIVA CONDICIONADA AL TOTAL DE LA COLUMNA
Rc<-F.%*%solve(dig.c)

############ANALISIS MULTIVARTIADO############
##############################################

#FILA 
#diagonal
dig.f<-diag(filas)
dig.c<-diag(col)
###MATRIZ DE FRECUENCIA RELATIVA CONDICIONADA AL TOTAL DE LA FILA
Rf<-solve(dig.f)%*%F.
##MATRIZ DE FRECUENCIA RELATIVA CONDICIONADA AL TOTAL DE LA COLUMNA
Rc<-F.%*%solve(dig.c)

##Proyección por filas Y (MATRIZ)
Pfilas<-Rf%*%solve(sqrt(diag(col))) #(7.4)
##Variable estandarizada
Ve<-Rf%*%solve(sqrt((1/1)*diag(col))) #(7.4)
##para determinar los valors propios y vectores propios
A<-t(Ve)%*%dig.f%*%Ve 
eigen(A)

###COLUMNA

##Proyección por columnas Z (MATRIZ)
Z<-solve(sqrt(diag(filas)))%*%F.%*%solve(sqrt(diag(col)))

B<-t(Z)%*%Z ##el primer valor propio siempre es 1
B.<-Z%*%t(Z)
eigen(B)
eigen(B.)


