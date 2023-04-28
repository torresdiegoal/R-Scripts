rm(list = ls())

library(lattice)
library(PASWR2)
data("EPIDURAL")

names(t)

#Los médicos quieren saber la estatura promedio de las mujeres que reciben anestesia epidural en posición sentada 
#tradicional. Sospechan que el La altura media es superior a 163 cm.

cm <- subset(EPIDURAL, EPIDURAL$treatment == "Traditional Sitting")
t <- t.test(cm$cm, mu=163, alternative = "less", conf.level = 0.95)
t

eda(cm$cm) #plotea histog, boxplot, qq plot y la linea de tendencia de densidad 

t <- binom.test(cm$cm, mu=163, alternative = "less", conf.level = 0.95)

#Con el conjunto de datos EPIDURAL, realice una prueba de hipotesis para ver si la estatura media de las mujeres 
#atendidas por el Dr. A es igual a la estatura media de mujeres atendidas por el Dr. B a nivel de significancia = 0.05, 
#donde ahora el

A <- subset(EPIDURAL, EPIDURAL$doctor == "Dr. A")
C <- subset(EPIDURAL, EPIDURAL$doctor == "Dr. C")

#boxplot(EPIDURAL$cm~EPIDURAL$doctor) pa ver mejor la distribución de los datos y sus valores descriptivos

t.test(A$cm, C$cm, alternative = "t", conf.level = .95, var.equal = T)
#t <- t.test(x, mu=45, alternative = "less", conf.level = 0.95) #less, greater, two.sided (default)

xbar <- 69 
ybar <- 64
m <- 14
n <- 11
S1 <- 71
S2 <- 52
d <- 0


Test <- ((xbar - ybar) - d)/((((n-1)*S1)+((m-1)*S2))/(m+n-2))

boxplot(esbel$IE~esbel$Tratamiento)

Ctrl <- subset(esbel, esbel$Tratamiento == "Ctrl")
eda(Ctrl$IE)
Fert <- subset(esbel, esbel$Tratamiento == "Fert")
eda(Fert$IE)

t.test(Ctrl$IEE, Fert$IEE, alternative = "t", conf.level = .95, var.equal = T)
prop.test(Ctrl$IEE, 21, alternative = "t", conf.level = .95)
binom.test(Ctrl$IEE, 21, 0.5, alternative = "t", conf.level = .95)