install.packages("gplots")
install.packages("graphics")
library("graphics")
library("gplots")

##############################
# CA: ANALISIS FACTORIAL #
##############################

Base<- read_excel("C:/Users/Caldas/Downloads/rstudio-export (1)/Base_uno.xls")
str(Base)
View(Base)
head(Base, 10)
attach(Base)
summary(Base)

# cut permite categorizar variables numericas a nominales
Obesidad <- cut(Base$IMC,breaks=c(14,20,25,30,35),right = FALSE)
table(Obesidad)

Estatura.<-cut(Base$Estatura,breaks=4,right = FALSE)
table(Estatura.)

mytable.22<-table(Obesidad,Estatura.)
mytable.22


#Ho: las variables col y fil son independientes
#Ha: las variables col y fil NO son independientes.

# 
chisq.test(mytable.22) # chi-cuad porque se comparan proporciones o varianzas

# Analisis factorial
res.ca <- CA(mytable.22, graph = TRUE)
print(res.ca)
summary(res.ca)
plot(res.ca)




########################################
##########Ejercicio Dos############
########################################

# Import the data
#Un marco de datos que contiene la frecuencia de ejecución de 13 tareas domésticas en la pareja.
file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)

head(housetasks)
str(housetasks)


#convertir a tabla
dt <- as.table(as.matrix(housetasks))
# 2. grafico
#Trace una matriz grafica donde cada celda contiene un punto cuyo tamano refleja la
#magnitud relativa del componente correspondiente.
#Util para visualizar la tabla de contingencia formada por dos variables categoricas
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

mosaicplot(dt, shade = TRUE, las=2,
           main = "housetasks")


# 
chisq <- chisq.test(housetasks)
chisq
chisq$observed
round(chisq$expected,2)

library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

fit<-CA(housetasks)
get_eigenvalue(fit)#tabla de eigevalores
fviz_eig(fit)#grafico

#filas
get_ca_row(fit)
fviz_ca_row(fit)

#columnas
get_ca_col(fit)
fviz_ca_col(fit)

#filas y columnas
fviz_ca_biplot(fit)


###analisis 

eig.val<-get_eigenvalue(fit)
fviz_screeplot(fit, addlabels=TRUE, ylim=c(0,50))


##Biplot Fila columna
fviz_ca_biplot(fit,repel=TRUE)


## variable fila
fila<-get_ca_row(fit)

fila$coord
fila$contrib
fila$cos2


##calidad de las filas
fviz_ca_row(fit)
fviz_ca_row(fit, col.row = "red", shape.row = 15)
fviz_ca_row(fit, alpha.row = "cos2")
#correlación
corrplot(fila$cos2, is.corr = FALSE)
fviz_cos2(fit,choice = "row", axes = 1:2)


###contribución de las filas en las dimensiones
corrplot(fila$contrib, is.corr = FALSE)
fviz_contrib(fit,choise="row", axes=1, top=10)
fviz_contrib(fit,choise="row", axes=2, top=10)
fviz_contrib(fit,choise="row", axes=1:3, top=10)


##variables columnas
col<-get_ca_col(fit)
col$coord
col$contrib
col$cos2

##calidad
fviz_ca_col(fit)
fviz_cos2(fit, choice = "col", axes = 1:2)

##biplot
#Simetria
fviz_ca_biplot(fit, repel = TRUE)
#Asimetria
#las columnas se representan en el espacio de filas
fviz_ca_biplot(fit, map="rowprincipal", 
               arrow=c(TRUE, TRUE),
               repel=TRUE)
#las filas se representan en el espacio de columnas
fviz_ca_biplot(fit, map="colprincipal", 
               arrow=c(TRUE, TRUE),
               repel=TRUE)

######descripción de las dimensiones
fit.desc<-dimdesc(fit, axes=c(1,2))
fit.desc[[1]]$row
fit.desc[[1]]$col

fit.desc[[2]]$row
fit.desc[[2]]$col


####taller
biom <- read.table("http://ares.inf.um.es/00Rteam/pub/clas/data/biom2003.dat")
head(biom)














