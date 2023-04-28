
iris
summary(iris)
library(plotly)
fig <- plot_ly(y = ~iris$Sepal.Length, type = "box",quartilemethod="linear", name="Sepal.Length")
fig <- fig %>% add_trace(y = ~iris$Sepal.Width, quartilemethod="linear", name="Sepal.Width")
fig <- fig %>% add_trace(y = ~iris$Petal.Length, quartilemethod="linear", name="Petal.Length")
fig <- fig %>% add_trace(y = ~iris$Petal.Width, quartilemethod="linear", name="Petal.Width")
fig

fig<-plot_ly(y=~iris$Sepal.Length, type="box", quartilemethod="linear",
             name="Sepal.Length", boxpoints="all", jitter =0.2,
             pointpos=-1.8)
fig
fig<-fig%>%add_trace(y=~iris$Petal.Length, type="box", quartilemethod="linear",
             name="Petal.Length", boxpoints="all", jitter =0.2,
             pointpos=-1.8)
fig


#Histogramas



fig1<-plot_ly(x=~iris$Sepal.Length, type="histogram")
dat<-plot_ly(x=~iris$Sepal.Length, type="histogram",
             cumulative=list(enabled=TRUE))
fig <- subplot(fig1, dat)



# Resolver
# Generar una serie 4 series de tiempo
#  Agruparlas y graficarlas.  Serie de tiempo, Histograma
# distribución de Boxplot

tiempo = 10 minutos






