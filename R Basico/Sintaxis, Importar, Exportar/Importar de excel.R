library(readxl)
Mayo_2019 <- read_excel("C:/Users/PC-36137/Downloads/Mayo_2019.xlsx")
View(Mayo_2019)
attach(U_Pb)
detach(U_Pb)

#medidas de tendencia para 3 variables del dataset mpg
mean(hwy)
median(hwy)
sort(table(hwy),decreasing = T) #hay que corregir para moda
var(hwy) #no se interpretahisto
quantile(hwy) #da todos los quartiles
quantile(hwy, probs = 0.25)
rango_hwy = max(hwy) - min(hwy)
IQR(hwy)

#medidas de forma
install.packages("moments")
library(moments)
skewness(hwy)
kurtosis(hwy)

y <- seq(min(0), max(60), length = 234)

ggplot(mpg) + geom_histogram(mapping = aes(hwy, color = "red"), bins = 10) + geom_smooth(hwy, y) 
