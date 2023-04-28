rm(list = ls())

library(ggplot2)
ggplot2::mpg
ggplot2::ggplot()
ggplot2::diamonds

# Plotea bar charts vs Coxcomb chart (Un tipo de diagrama de pie, pero es realmente de barras, ya que se basa en coord polares), width reduce a 0 el espaciado entre barras, theme(aspect.ratio = 1) aumenta o disminuye el tamaño cuadro de fondo de la gráfica 
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

#Subplot de dos histogramas
par(mfrow=c(1,2))
hist(velocidades,col='14',nclass=10,ylim=c(0,10),
    labels=TRUE,main= 'Velocidad autom´oviles',
    sub='Via el Volador',
    ylab='Frecuencias',xlab='Velocidad en Km/h')
hist(velocidades,col='14',nclass=10,freq=FALSE,
    ylim=c(0,0.04), main='Velocidad autom´oviles',
    sub='V´ia el Volador', ylab='Frecuencias
    relativas',xlab='Velocidad en Km/h')

#Ejemplos de boxplots
boxplot(U_Pb$U_ppm, main = "U (ppm)", ylab="U (ppm)", ylim = c(0,1500), range = 1.5)

boxplot(U_Pb$U_ppm, U_Pb$`206Pb_207Pb`, U_Pb$`206Pb_238U`, col = "snow2", main = c("U (ppm)", "Pb206/Pb207", "Pb206/U238"), ylab="U (ppm)", ylim = c(0,1500), range = 1.5)


par(mfrow=c(1,3), mar=c(2,2,2,2),names(c("U (ppm)", "Pb206/Pb207","Pb206/U238")) )
boxplot(U_Pb$U_ppm, ylab="U (ppm)",
        col = "snow2", 
        main= "Concentración", names = "U (ppm)", 
        range = 1.5, boxwex=0.6, outwex = 0.1, staplewex = 0.1)
boxplot(U_Pb$`206Pb_207Pb`, ylab="Pb206/Pb207",
        col = "snow2", 
        main= "Edades", names = "Pb206/Pb207", 
        range = 1.5, boxwex=0.6, outwex = 0.1, staplewex = 0.1)
boxplot(U_Pb$`206Pb_238U`, ylab="Pb206/U2038",
        col = "snow2", 
        main= "Edades \nPb206/U2038", names = "Pb206/U238", 
        range = 1.5, boxwex=0.6, outwex = 0.1, staplewex = 0.1)
