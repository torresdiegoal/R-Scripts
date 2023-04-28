##Inferencia sobre el vector de media
X1<-c(1.9,0.8,1.1,0.1,-0.1,4.4,5.5,1.6,4.6,3.4)
X2<-c(0.7,-1.6,-0.2,-1.2,-0.1,3.4,3.7,0.8,0.0,2.0)
mean(X1)
mean(X2)
data<-matrix(c(X1,X2),ncol=2)

cov(data)
cor(data)

########################################
#####matriz con covarianza conocida####
#####################################

###en la siguientye tabla  se registran la estatura  X1 en pulgadas y el peso X2 EN LIBRAS, 
## para una muestra  de 20 estudiantes de educación media. 
##suponga  que quiere verificar la hipótesis que la muestra  media es 70 y el peso medio es 170.
X1<-c(69,74,68,70,72,67,66,70,76,68,72,79,74,67,66,71,74,75,75,76)
X2<-c(153,175,155,135,172,150,115,137,200,130,140,265,185,112,140,150,165,185,210,220)
data.<-matrix(c(X1,X2),ncol=2)

X1m<-mean(X1)
X2m<-mean(X2)
M1<-70
M2<-170

##se asume que esta muestra es generada en una población normal bivariada:

var<-matrix(c(20,100,100,1000),2)


CHI.2<-length(X1)*t(matrix(c(c(X1m-M1),c(X2m-M2)),2))%*%solve(var)%*%matrix(c(c(X1m-M1),c(X2m-M2)),nrow=2)

#para un alpha de 0.05 con dos grados 

chi2<-qchisq(0.05, 2, ncp=0, lower.tail = F)# Menores

chi2.<-pchisq(CHI.2, 2, ncp=0, lower.tail = F)# Menores

library(psych)
pairs.panels(data.[,1:2], scale=TRUE)


###########################################################
####### matriz con varianza desconocida #################
##########################################################

#ESTADISTICO T2 DE HOTELLING

# Los datos contenidos en la base corresponden a los pesos en centigramos del corcho
# encontrado  en muestras  tomadas en la dirección Norte (N), Este (E), Sur (S) y Oste (O) del tronco
# de 28 árboles  cultivados en una parcela experimental,  En este caso las variables  corresonden al peso de 
# las cuatro muestras tomadas sobre cada árbol (medias repetidas).

data.2<- read.csv("D:/Disco D/R/Datos/Multivariado/ACTIVIDAD.CSV", 
                    sep =";")

colMeans(data.2)

#covarianza
cov(data.2)

N<-mean(data.2$N)
E<-mean(data.2$E)
S<-mean(data.2$S)
O<-mean(data.2$O)


me<-matrix(c(N,E,S,O),ncol=1)


## contraste la hipotest 
C<-matrix(c(1,0,0,1,-1,0,0,-1),nrow = 2)

n=dim(data.2)[1]

CX.t<-t(C%*%t(me))
CX<-(C%*%t(me))
CXC<-C%*%cov(data.2)%*%t(C)

T2<-n%*%CX.t%*%solve(CXC)%*%CX
T2


qf(0.05,dim(data.2)[2]-2,
   dim(data.2)[1]-2, lower.tail=FALSE)

pf(T2,dim(data.2)[2]-2,
   dim(data.2)[1]-2, lower.tail=FALSE)



####

diag(cov(data.2))
sum(diag(cov(data.2)))
################
p=matrix(c(5.9, 2.9,2.3,6.0, 4.8,1.9,2.4,1.9), ncol=2)

A<-c(5.9, 2.9,2.3,6.0)
B<-c(4.8,1.9,2.4,1.9)
C<-c(2.0,0.2,0.5,2.9)
D<-c(0.7,0.9,0.5,1.0)
E<-c(1.2,0.5,0.9,0.6)

P.1<-cbind(A,B,C,D,E)
rownames(P.1)<-c("NOROESTE", "SUR","OESTE MEDIO" ,"OESTE")
P.1

colMeans(P.1)

p.2<-data.frame(P.1)
mean(p.2$A)

var(A)


str(p)
dim(p)

str(p.2)

t(P.1)

cor(P.1)

round(cov(P.1),2)

install.packages("GGally")
library(GGally)

ggpairs(p.2)

###graficos
install.packages("corrplot")
library(corrplot)
pairs(p.2, pch = ".", cex = 1.5)


correlacion<-round(cor(p.2), 1)
corrplot(correlacion, method="number", type="upper")
corrplot(round(cor(p.2),2) , type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

?chart.Correlation

chart.Correlation(p.2, histogram=TRUE, pch=19)


install.packages("psych")
library(psych)

pairs.panels(p.2[,1:2], scale=TRUE)



install.packages("GGally")
library(GGally)
ggpairs(var1)


P.3<-cbind(A,B,C,D)
rownames(P.3)<-c("NOROESTE", "SUR","OESTE MEDIO")
P.3

det(P.3)

solve(P.3)




colMeans(P.1)

p.2<-data.frame(P.1)
mean(p.2$A)

str(p)
dim(p)

str(p.2)

t(P.1)
#################
install.packages("tidyverse")
install.packages("tidyr")
library(tidyverse)
library(tidyr)
##################
?select
?filter
?arrange
?mutate
?summarise
?group_by
#crea nueva varibales
data(mtcars)
head(mtcars)
?mtcars
dim(mtcars)
str(mtcars)
names(mtcars)
summary(mtcars)

mtcars$vs<-as.factor(mtcars$vs)
mtcars$am<-as.factor(mtcars$am)
str(mtcars)

attach(mtcars)

mean(mtcars$mpg)
median(mtcars$mpg)
table(mtcars$mpg)

mpg.1<-sort(table(mtcars$mpg),TRUE)

var(mtcars$mpg)

sqrt(var(mtcars$mpg))
sd(mtcars$mpg)

(cv<-(sd(mpg)/mean(mpg))*100)

stem(mtcars$mpg)
summary(mtcars$mpg)
boxplot(mtcars$mpg)
hist(mtcars$mpg)
plot(mpg)
edit(mtcars)
#######################

#seleccionar varibales
names(mtcars)
var1<-select(mtcars,hp,drat)
summary(var1)
cov(var1)
select(mtcars, disp:drat)
cov(select(mtcars, disp:drat))

#eliminar variables
select(mtcars, -wt)
select(mtcars, -(disp:drat))

#Selecciona columnas cuyo nombre contiene un string
select(mtcars,starts_with("v"))
#Selecciona columnas cuyo nombre termina con un string
select(mtcars, ends_with("t"))

#
with(mtcars, {
  print(summary(mpg))
  plot(mpg, disp)
  plot(mpg, wt)
})

cv<-function(data){
  mean<-mean(data)
  sd<-sd(data)
  cv<-(sd/mean)*100
  print(cv)
}

cv(mpg)
cv(hp)


#rename
nueva_base<-rename(mtcars, Miles.gallon=mpg,
                   cylinders=cyl, 
                   Displacement=disp, 
                   Grosshorsepower=hp,
                   Rear.axle.ratio=drat,
                   Weight1000lbs=wt,
                   miletime=qsec)

View(nueva_base)

dim(mtcars)


#filter
filter(mtcars, drat >= mean(drat))


filter(mtcars, drat>=mean(drat) & hp< mean(hp))


da<-filter(mtcars, 
           drat>=mean(drat),
           mtcars %in% c("Mazda RX4",
                         "Honda Civic",
                         "Mazda RX4 Wag"))

#arrange
arrange(mtcars, drat) #Por defecto arrange() ordena las filas por orden ascendente:
arrange(mtcars, cyl)
arrange(mtcars, desc(cyl))
arrange(mtcars, cyl, drat, mpg)

#mutate
mutate(mtcars, freq.acumulative = cumsum(drat))

#resumenes
summarise(mtcars, 
          mediana = median(drat), 
          variance = var(drat), 
          mean=mean(drat))

# group_by() agrupa un conjunto de filas seleccionado en un conjunto
#de filas de resumen de acuerdo con los valores de una o más columnas o expresiones.

grupo<-group_by(mtcars, cyl)
View(grupo)
#
mtcars %>%  group_by(cyl) %>% 
  summarise(mean = mean(cyl), sum = sum(cyl), n = n())

#El operador pipe %>%
#El operador pipeline %>% es útil para concatenar múltiples dplyr operaciones
mtcars %>% filter(drat >= median(drat))

data.4<-mtcars %>% as_tibble() %>% mutate(
  cyl2 = cyl * 2,
  cyl4 = cyl2 * 2
)
View(data.4)


#cambia y remueve variables
mtcars %>% as_tibble() %>% mutate(
  mpg = NULL,
  disp = disp * 0.0163871 # convert to litres
)

mtcars %>%
  group_by(cyl) %>%
  mutate(rank = min_rank(desc(mpg)))

mtcars %>%
  count(cyl)

#########################################
install.packages("ggplot2")
library(ggplot2)
data(mpg)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)
#graficos
names(mpg)
?mpg

mpg$manufacturer<-as.factor(mpg$manufacturer)
mpg$model<-as.factor(mpg$model)
mpg$year<-as.factor(mpg$year)
mpg$cyl<-as.factor(mpg$cyl)
mpg$trans<-as.factor(mpg$trans)
mpg$drv<-as.factor(mpg$drv)
mpg$fl<-as.factor(mpg$fl)
mpg$class<-as.factor(mpg$class)

str(mpg)
summary(mpg)


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(mpg, aes(displ, hwy)) +
  geom_point()

ggplot(mpg, aes(x=displ, y=cty, 
                colour = class)) +
  geom_point()

ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point(aes(colour = "Manuel"))


ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point(colour = "blue")

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point() +
  facet_wrap(~class)

ggplot(mpg, aes(x=displ, y=hwy,
                colour = cyl)) +
  geom_point()

ggplot(mpg, aes(x=displ, y=hwy, colour = class)) +
  geom_point()


#### Datos Iris
head(iris,-100)
dim(iris)
names(iris)

plot(x=iris$Sepal.Length, y=iris$Sepal.Width, 
     xlab="Sepal Length", ylab="Sepal Width", 
     main="Sepal Length-Width")

scatter <- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width))


scatter + geom_point(aes(color=Species, shape=Species)) +
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")

scatter + geom_point(aes(color = Petal.Width, shape = Species), size = 2, alpha = I(1/2)) +
  geom_vline(aes(xintercept = mean(Sepal.Length)), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = mean(Sepal.Width)), color = "red", linetype = "dashed") +
  scale_color_gradient(low = "yellow", high = "red") +
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")

boxplot(Sepal.Length~Species,data=iris, 
        xlab="Species", ylab="Sepal Length", main="Iris Boxplot")

box <- ggplot(data=iris, aes(x=Species, y=Sepal.Length))
box + geom_boxplot(aes(fill=Species)) + 
  ylab("Sepal Length") + ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 

##histogramas
hist(iris$Sepal.Width, freq=NULL, density=NULL, breaks=12,
     xlab="Sepal Width", ylab="Frequency", 
     main="Histogram of Sepal Width")

?hist

histogram <- ggplot(data=iris, aes(x=Sepal.Width))
histogram + geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Sepal Width") +  ylab("Frequency") + ggtitle("Histogram of Sepal Width")



#barras

nrow(iris)
set.seed(123)
iris1 <- iris[sample(1:nrow(iris), 50),]
View(iris1)

sum(table(iris1$Species))
hline <- data.frame(Species=c("setosa", "versicolor", "virginica"), hline=as.vector(table(iris$Species)))
hline

barplot(table(iris1$Species), col="red", xlab="Species", ylab="Count", main="Bar plot of Sepal Length")

bar <- ggplot(data=iris1, aes(x=Species))
bar + geom_bar() + 
  xlab("Species") +  ylab("Count") + ggtitle("Bar plot of Sepal Length") +
  geom_errorbar(data=hline, aes(y=hline, ymin=hline, ymax=hline), col="red", linetype="dashed")



install.packages("reshape2")
library(reshape2)
iris2 <- melt(iris, id.vars="Species")
iris2[1:10,]

bar1 <- ggplot(data=iris2, aes(x=Species, y=value, fill=variable))
bar1 + geom_bar(stat="identity", position="dodge") + 
  scale_fill_manual(values=c("orange", "blue", "darkgreen", "purple"),
                    name="Iris\nMeasurements",
                    breaks=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                    labels=c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"))


###Pie Chart
quan <- as.vector(table(iris1$Species))
pos <- cumsum(quan) - quan/2
quantity <- data.frame(Species=c("setosa", "versicolor", "virginica"), quantity=quan, position = pos)                       

pie <- ggplot(iris1, aes(x=factor(1), fill=Species)) + geom_bar(width=1) + geom_text(data=quantity, aes(x=factor(1), y=position, label=quantity) , size=5) + labs(x="", y="")
pie

pie + coord_polar()
pie + coord_polar(theta="y")

###Density Curve
d <- density(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks=12, 
     prob=TRUE, xlab="Sepal Width", 
     main="Histogram & Density Curve")
lines(d, lty=2, col="blue")

density <- ggplot(data=iris, aes(x=Sepal.Width))
density + geom_histogram(binwidth=0.2, color="black", fill="steelblue", aes(y=..density..)) +
  geom_density(stat="density", alpha=I(0.2), fill="blue") +
  xlab("Sepal Width") +  ylab("Density") + ggtitle("Histogram & Density Curve")

density2 <- ggplot(data=iris, aes(x=Sepal.Width, fill=Species))
density2 + geom_density(stat="density", alpha=I(0.2)) +
  xlab("Sepal Width") +  ylab("Density") + ggtitle("Histogram & Density Curve of Sepal Width")


ggplot(data=iris,
       aes(x=Sepal.Width,color=Species)) + geom_density() +theme_minimal()


##Agregar suavizadores
smooth <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
  geom_point(aes(shape=Species), size=1.5) + xlab("Sepal Length") + ylab("Sepal Width") + 
  ggtitle("Scatterplot with smoothers")

# Linear model
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + 
  geom_point() + geom_smooth(method = "lm", se = F)
smooth + geom_smooth(method="lm")

# Local polynomial regression
smooth + geom_smooth(method="loess")

smooth + geom_smooth(method="gam", formula= y~s(x, bs="cs"))

###Facetas

facet <- ggplot(data=iris, aes(Sepal.Length, y=Sepal.Width, color=Species)) + 
  geom_point(aes(shape=Species), size=1.5) + geom_smooth(method="lm") +
  xlab("Sepal Length") + ylab("Sepal Width") + ggtitle("Faceting")

# Along rows
facet + facet_grid(. ~ Species)

# Along columns
facet + facet_grid(Species ~ .)

##Jitter
head(mpg)
jitter <- ggplot(mpg, aes(x=class , y=hwy)) 
jitter + scale_x_discrete() +
  geom_jitter(aes(color = class, x = class), 
              position = position_jitter(width = .05), alpha = 0.5) +
  geom_boxplot(aes(color = class), outlier.colour = NA, position = "dodge") +
  xlab("Class") + ylab("Highway miles per gallon")

###Volcano

vol <- ggplot(data=iris, aes(x = Sepal.Length))
vol + stat_density(aes(ymax = ..density..,  ymin = -..density.., 
                       fill = Species, color = Species), 
                   geom = "ribbon", position = "identity") +
  facet_grid(. ~ Species) + coord_flip() + xlab("Sepal Length") 

ggplot(data=iris, aes(x=Sepal.Length, Petal.Length)) + geom_point() + 
  geom_rug(col="steelblue",alpha=0.1) + xlab("Sepal Length") + ylab("Petal Length")


library(plotly)

x <- 1:10
y <- jitter(x^2)

DF <- data.frame(x, y)

ggplot(DF, aes(x = x, y = y)) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), aes(colour = 'Exponential'), se = FALSE, start = list(a=1,b=1))

ggplotly()



############################

