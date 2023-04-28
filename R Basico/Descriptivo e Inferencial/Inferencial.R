rm(list = ls())

library(tigerstats)

alturas <- read.csv("C:\Users\PC-36137\Downloads\alturas.csv", sep = "")
alturas <- read.csv("C:/Users/PC-36137/Downloads/alturas.csv", sep="")

alt <- alturas$alturas                    
hist(hist(alt, main = "Histograma altura", col = "pink", border = "white", freq = F))
lines(density(alt), col = "red", lwd = 1) #lwd es el grosor
curve(dnorm(x, mean(alt), sd(alturas$alturas)), xlim = c(150, 180), add = T) #xlim me da el dominio en x


#es una prueba para saber si los datos siguen una distribución normal
qqnorm(alt, col = "blue")
qqline(alt, col = "red", lwd = 2)

pnorm(72,78,36, lower.tail = F)

qnorm(.25, 78, 36)

pnorm(58.71, 78, 36)

pnormGC(c(.9,-.9), region="between", mean = 0, sd = 1, graph = T)
ptGC(c(0.8889, 2.3060), region = "between",8) #T-student
pchisqGC(.75, region = "above", df = 5, graph = T) #distr. chi cuadrado con grafiquita