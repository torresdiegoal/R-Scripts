S<- matrix(c(0.35,0.15,-0.19
             ,0.15,0.13,-0.03,
             -0.19,-0.03,0.16)
           ,ncol=3)

##calculo de autovectores
eigen<-eigen(S)
sum(eigen$vectors[,1]^2)
####Proporción de las componentes

Pc1<-eigen$values[1]/sum(diag(S))
Pc2<-eigen$values[2]/sum(diag(S))
Pc3<-eigen$values[3]/sum(diag(S))

###varianza total porcentual
sum(Pc1,Pc2,Pc3)

###############################################
##############################################


S.1<- matrix(c(56.97,5.17,5.17,0.89)
             ,ncol=2)
##calculo de autovectores
eigen.1<-eigen(S.1)
#La varianza de una componente principal es el autovalor de S
eigen.1$values

#autovectores e1 y e2
eigen.1$vectors

#proporción de VT(S) explicada por Y1 es

Pc1<-eigen.1$values[1]/sum(diag(S.1))
Pc2<-eigen.1$values[2]/sum(diag(S.1))

##las correlaciones entre las primera componnete de las variables X son

corrY1X1<-eigen.1$vectors[1]*sqrt(eigen.1$values[1])/sqrt(S.1[1])
corrY1X2<-eigen.1$vectors[2]*sqrt(eigen.1$values[1])/sqrt(S.1[4])

########################################################
#########################################################

Pueblos <- read_excel("C:/Users/mfromeroo/Desktop/Multivariado/Archivo Componentes  Pricipales/Pueblos.xlsx")
View(Pueblos)

View(Pueblos[,-1])

Base<-Pueblos[,-1]

colMeans(Base)

MV<-cov(Base)

##calculo de autovectores
eigen.<-eigen(MV)

##autovalor
eigen.$values

##autovectores----componentes principales
eigen.$vectors

###varianza Total
TR<-sum(diag(MV))
lambda<-c(eigen.$values)

cbind(autovalor=c(eigen.$values),
      VT=((lambda/TR)*100),
      Acumulado=cumsum(VT))

#######################################
#A data frame with 41 observations on the following 7 variables.

#ya numeric vector: sulpher dioxide concentration in air mgs. per cubic metre in 41 cities in the USA

#x1a numeric vector: average annual temperature in degrees F

#x2a numeric vector: number of manufacturers employing >20 workers

#x3a numeric vector: population size in thousands

#x4a numeric vector: average annual wind speed in miles per hour

#x5a numeric vector: average annual rainfall in inches

#x6 a numeric vector: average number of days rainfall per year
#Source


install.packages("gamlss")
library(gamlss)
data(usair)
View(usair)
str(usair) 
plot(usair)

Corr.usair<-cor(usair[,-1])

##calculo de autovectores
eigen.<-eigen(Corr.usair)
round(eigen.$vectors,3) #componentes principales


###varianza Total
TR<-sum(diag(Corr.usair))
lambda<-c(eigen.$values)

cbind(autovalor=c(eigen.$values),
      VT=((lambda/TR)*100),
      Acumulado=cumsum(VT))


