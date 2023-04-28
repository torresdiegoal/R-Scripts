rm(list = ls())
########################################
### Fundación Universitaria          ###
### los Libertadores                 ###
###                                  ###
###                                 ###
###                                  ###
###                                  ###
### Wilson Sandoval                  ###
### Octubre de  2019                  ###
###                                  ###
### Manejo y visualización de datos  ###
########################################

## Carga de paquetes
install.packages("gapminder")
install.packages("tidyverse") 
library(gapminder)  # datos
library(dplyr)      # Paquete para manipular los datos

gapminder <- gapminder

## Visualización de la base de datos. Recuerde: Filas=individuos y Columnas=variables
attach(gapminder)
View(gapminder)
str(gapminder)
## Cuántos individuos?


## Idea general: presentar funciones generales que pueden ser útiles para el análisis 
## de la información presentada en un base de datos. 

### PRIMERA FUNCIÓN: filter (Filtrar por valores de una variable)

# Por ejemplo: Sólo queremos ver los paises cuyos valores en la variable "year" es 2007
gapminder %>% filter(year == 2007)


# Por ejemplo: Sólo queremos ver las observaciones EEUU
gapminder %>% filter(country == "United States")
# Por ejemplo: ambas al tiempo (EEUU y 2007)
gapminder %>% filter(country == "United States", year == 2007)

# Ejercicio: Filtre la base de datos por país (China) en 2002

### SEGUNDA FUNCIÓN: arrange (Ordenar de acuerdo a valores en una variable)

# Por ejemplo: Ver los paises de menor a mayor a mayor GPD (PIB)
gapminder %>% arrange(gdpPercap)
# Por ejemplo: Ver los paises de mayor a menor a mayor GPD (PIB)
gapminder %>% arrange(desc(gdpPercap))
# Por ejemplo: Ver TODOs (no solamente 10) los paises de mayor a menor a mayor GPD (PIB)
gapminder %>% arrange(desc(gdpPercap)) %>% print(n = nrow(gapminder))
# Como todo: Cosas interesantes pasan cuando se mezclan nociones.
# GDP (PIB) del año 2007 en orden descendiente. 
# RECOMENDACIÓN para estas tareas: Siemore de la grande a la pequeña:
gapminder %>% filter(year == 2007) %>% arrange(desc(gdpPercap))

# Ejercicio: Ordene la base de datos por Expectativa de vida (LifeExp) en ambos órdenes
# Ejercicio: Filtre por el año 1957 y reordene (descendente) por tamaño de población

### TERCER FUNCIÓN: mutate (Agregar o cambia  variables)

# Por ejemplo: Crear la columna GPD (GPDpercap*pop)
gapminder %>% mutate(GPD=gdpPercap*pop)
# Por ejemplo: Estandarizar el tamaño de la población) en nueva variable
gapminder %>% mutate(population= pop/1000000)
# Por ejemplo: Estandarizar el tamaño de la población) en la misma variable que ya existía
gapminder %>% mutate(pop= pop/1000000)


#Ejercicio: Cree la variable Expectativa de vida, en meses. 
#Ejercicio: Qué hace la siguiente línea? 
gapminder %>% filter(year ==2007) %>% mutate(lifeExpMonths=12*lifeExp) %>%arrange(lifeExpMonths)


# Ahora, es de nuestro interés generar figuras como soporte visual. 
# Hay dos formas de hacer esta tarea: las funciones nativas de R o el paquete ggplot2

#Crear una base de datos con una parte de la información que es de interés
gapminder_1952<-gapminder %>% filter(year==1952)
gapminder_2007<-gapminder %>% filter(year==2007)

# Dibujar la nueva base de datos 
# Lógica: NombreDeLaBaseDeDatos$NombreDeLaVariable
plot(gapminder_1952$gdpPercap,gapminder_1952$lifeExp)
# Es funciona? Sí. Es sofisticado? no mucho
library(ggplot2)
# PIB vs expectativa de vida
ggplot(gapminder_1952, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() ## Este!
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()
# Población vs PIB
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()
ggplot(gapminder_2007, aes(x = pop, y = gdpPercap)) +
  geom_point()
# Población vs Expectativa de vida
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()
ggplot(gapminder_2007, aes(x = pop, y = lifeExp)) +
  geom_point()

## Cosas ñoñas: Cuando las curvas son como en la figura Este!, es razonable aplicar 
## escalamiento de los datos, en este caso, logaritmo base 10 en la variable del eje x. 
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10()

#Ejericio: Qué hace la siguiente línea de código? 
ggplot(gapminder_1952,aes(x=pop,y=gdpPercap))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()


## Hasta ahí, lindo, sí. Interesante poder incorporar más información en el mismo gráfico
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp,color=continent)) +
  geom_point() + 
  scale_x_log10()

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color=continent, size=pop)) +
  geom_point() + 
  scale_x_log10()

## Ejercicio: Dibuje Tamaño de población (en escala logarítmica) vs expectativa de vida, y coloree por continente
## Ejercicio: Qué hace la siguiente línea de código? 
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, size=gdpPercap)) +
  geom_point() +
  scale_x_log10()

### Otra forma interesante de presentar la información es usar varias figuras "por partes"  
ggplot(gapminder_1952,aes(x=pop,y=lifeExp))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~continent)
#Toda la información en una sóla figura
ggplot(gapminder, aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~year)


### Volviendo a la tarea de tener que "masear" con una base de datos

## CUARTA FUNCIÓN: summarize (Crear resúmenes de información)
#Por ejemplo: Expectativa mediana de vida
gapminder %>% summarize(medianLifeExp=median(lifeExp))
#Por ejemplo: Expectativa media de vida
gapminder %>% summarize(meanLifeExp=mean(lifeExp))
#Por ejemplo: Expectativas de vida
gapminder %>% summarize(medianLifeExp=median(lifeExp),meanLifeExp=mean(lifeExp))


#Nuevamente, cuando se mezclan las funciones, se eleva enormemente la capacidad de la herramienta
# Ejemplo: Expectativa de vida mediana en 1957
gapminder %>% filter(year ==1957) %>% summarize(medianLifeExp=median(lifeExp))
# Ejemplo: Expectativa mediana de vida y máximo GDP en 1952 
gapminder %>% filter(year == 1957) %>% summarize(medianLifeExp=median(lifeExp),maxGdpPercap=max(gdpPercap))

### QUINTA FUNCIÓN: gruop_by (Hacer resúmenes pero por grupos, no por toda la base de datos)

# Ejemplo: Mediana de expectativa de vida y GDP máximo por año
gapminder %>% group_by(year) %>% summarize(medianLifeExp=median(lifeExp),maxGdpPercap=max(gdpPercap))
# Ejemplo: Mediana de expectativa de vida y GDP máximo en 2007 por continente
gapminder %>% filter(year==2007) %>% group_by(continent) %>% summarize(medianLifeExp=median(lifeExp),maxGdpPercap=max(gdpPercap))
# Ejemplo: Mediana de expectativa de vida y GDP máximo por año y continente
gapminder %>% group_by(continent,year) %>% summarize(medianLifeExp=median(lifeExp),maxGdpPercap=max(gdpPercap))
#Ejercicio: Qué hace la siguiente línea de código?
gapminder %>% group_by(year) %>% summarize(meanLifeExp=mean(lifeExp),totalPop=sum(pop))
#Ejercicio: Qué hace la siguiente línea de código?
gapminder %>% filter(year==2007) %>% group_by(continent) %>% summarize(meanLifeExp=mean(lifeExp),totalPop=sum(pop))
#Ejercicio: Qué hace la siguiente línea de código?
gapminder %>% group_by(year, continent) %>% summarize(meanLifeExp=mean(lifeExp)) %>% print(n = 60)


## Volviendo a la visualización de datos.
# Crear una objeto que contenga los tamaños de población media y expectitava de vida media por año
by_year<-gapminder %>% group_by(year) %>% summarize(totalPop=mean(pop),meanLifeExp=mean(lifeExp))
# Dibuje incorporado el límite inferior del eje y (y=0)
ggplot(by_year,aes(x=year,y=totalPop)) + geom_point()+ expand_limits(y=0)
# Crear una objeto que contenga los tamaños de población media y expectitava de vida media por año y continente
by_year_continent<-gapminder %>% group_by(year,continent) %>% summarize(totalPop=mean(pop),meanLifeExp=mean(lifeExp))
# Dibuje incorporado el límite inferior del eje y (y=0)
ggplot(by_year_continent,aes(x=year,y=totalPop,color=continent)) + 
  geom_point() +
  expand_limits(y=0)

# Ejercicio: Dibuje la evolución de la expectativa mediana de vida a través de los años. 
# Ejercicio: Dibuje la evolución de la GPD per capita mediano a través de los años y dibuje por continente
# Ejercicio: Qué hacen las siguientes líneas de código?  
by_continent_2007<- gapminder %>% 
  filter( year== 2007) %>% 
  group_by(continent) %>% 
  summarize(medianLifeExp=median(lifeExp),medianGdpPercap=median(gdpPercap))
ggplot(by_continent_2007, aes(x=medianGdpPercap,y=medianLifeExp,color=continent)) + 
  geom_point() +
  expand_limits(y=0)

### A continuación se realizarán 4 tipos de gráficos adicionales. Pueden ser útilespara mostrar
# diferentes tipos de datos, dependiendo del tipo de datos. 

#### FIGURAS CON LÍNEAS

#GPD mediano por año
by_year <- gapminder %>% group_by(year) %>% summarize(medianGdpPercap=median(gdpPercap))

# Figura con puntos
ggplot(by_year,aes(x=year,y=medianGdpPercap))+
  geom_point()+
  expand_limits(y = 0)

# Figura con líneas
ggplot(by_year,aes(x=year,y=medianGdpPercap))+
  geom_line()+
  expand_limits(y = 0)

# Resumir la mediana del GDP por año y continente
by_year_continent <- gapminder %>% 
  group_by(year, continent) %>% 
  summarize(medianGdpPercap=median(gdpPercap))

# Una figura que muestra el cambio de dicho resumen por continente
ggplot(by_year_continent,aes(x=year,y=medianGdpPercap, color=continent))+
  geom_line()+
  expand_limits(y = 0)

#### FIGURAS CON BARPLOTS

#Expectativa mediana de vida para el año 2007 y por continentes
by_continent<- gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp=mean(lifeExp))
# Figura en barplot 
ggplot(by_continent,aes(x=continent, y=meanLifeExp))+
  geom_col()
#GPD per capita mediana para el año 1952 y por continentes
by_continent<- gapminder %>% 
  filter(year == 1952) %>% 
  group_by(continent) %>% 
  summarize(medianGdpPercap=median(gdpPercap))

# Figura en barplot
ggplot(by_continent,aes(x=continent, y=medianGdpPercap))+
  geom_col()

# Ejercicio: Filtre las observacioens para Oceania el 1952 y realice un barplot del GDP per capita

### FIGURAS CON HISTOGRAMAS

# Distribución de la expectativa de vida en el año 2007
ggplot(gapminder_2007,aes(x=lifeExp))+
  geom_histogram()+
  # Datos de 1952
  gapminder_1952 <- gapminder %>%  filter(year == 1952)
# Distribución de la población en 1952 
ggplot(gapminder_1952, aes(x=pop))+ 
  geom_histogram()
# Distribución de la población (en logaritmo) en 1952 
ggplot(gapminder_1952,aes(x=pop))+
  geom_histogram()+
  scale_x_log10()


### FIGURAS CON BOXPLOT
#Datos del año 2007
gapminder_2007 <- gapminder %>%
  filter(year == 2007)
#Distribución expectativa de vida por continente
ggplot(gapminder_2007,aes(x=continent, y=lifeExp))+
  geom_boxplot()
#Datos del año 1952
gapminder_1952 <- gapminder %>%
  filter(year == 1952)
#Distribución GPD per capita (en logaritmo) por continente
ggplot(gapminder_1952,aes(x=continent,y=gdpPercap))+
  geom_boxplot()+
  scale_y_log10()
## Misma figura pero con título y nombres específicos en los ejes
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Comparando PIB per capita por continentes")+
  xlab("PIB per capita") + 
  ylab("Continentes")


#### Ahora, volviendo a funciones para poder manipular datos
#### Nuestro interés ahora: Cruzar bases de datos. 

# Concepto de "Tidy data": Datos que deben ser: 
# - Filas son individuos
# - Columnas son variables
# - Valores entre filas y columnas son observaciones. 

## SEXTA FUNCIÓN: gather (cruzar información en una misma variable)

#Note la diferencia entre table4a y el resultado de aplicar gather 
tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b) 
## nueva función left_join (unir por izquierda, busca los nombres en tidy4a y los cruza con tidy4b)





### Objetivo! 
# Datos: 
setwd("G:/Mi unidad/U libertadores/2019.1/Diplomado Big Data/Visualización de información")
library(readxl)
dat<- read_excel("Economist.xlsx")
# Figura básica
pc1 <- ggplot(dat, aes(x = CPI, y = HDI, color = Region))
pc1 + geom_point()
#Agregar la línea de tendencia con el modelo y~x+log(x)
pc2 <- pc1 +
  geom_smooth(mapping = aes(linetype = "r2"),
              method = "lm",
              formula = y ~ x + log(x), se = FALSE,
              color = "red")
pc2 +  geom_point(shape = 1, size = 4)
pc3 <- pc2 + geom_point(shape = 1, size = 2.5, stroke = 1.25)
# Etiquetas de los paises
puntos <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
            "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
            "India", "Italy", "China", "South Africa", "Spane",
            "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
            "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
            "New Zealand", "Singapore")
# Figura que incorpora las etiquetas
(pc4 <- pc3 +
    geom_text(aes(label = Country),
              color = "gray20",
              data = dat %>% filter(Country %in% puntos)))


library(ggrepel) # Se usa paquete ggrepel (ver:https://cran.r-project.org/web/packages/ggrepel/ggrepel.pdf)
# para no sobreponer textos
(pc4 <- pc3 +
    geom_text_repel(aes(label = Country),
                    color = "gray20",
                    data = filter(dat, Country %in% puntos),
                    force = 10))
# Volver factor   
dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe","Americas","Asia Pacific","East EU Cemt Asia","MENA","SSA"),
                     labels = c("OECD","Americas","Asia &\nOceania","Central &\nEastern Europe",
                                "Middle East &\nnorth Africa","Sub-Saharan\nAfrica"))

pc4$data <- dat
pc4

## Se usa el paquete grid
(pc5 <- pc4 +
    scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                       limits = c(.9, 10.5),
                       breaks = 1:10) +
    scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",
                       limits = c(0.2, 1.0),
                       breaks = seq(0.2, 1.0, by = 0.1)) +
    scale_color_manual(name = "",
                       values = c("#24576D","#099DD7","#28AADC","#248E84","#F2583F","#96503F")) +
    ggtitle("Corruption and Human development"))

(pc6 <- pc5 +
    theme_minimal() + # Theme más básico para ir agregando cosas
    theme(text = element_text(color = "gray20"),
          legend.position = c("top"), # Posición: superior izquierda
          legend.direction = "horizontal",
          legend.justification = 0.1, # punto de anclaje para legend.position.
          legend.text = element_text(size = 11, color = "gray10"),
          axis.text = element_text(face = "italic"),
          axis.title.x = element_text(vjust = -1), # Título lejos del eje
          axis.title.y = element_text(vjust = 2), # Título lejos del eje
          axis.ticks.y = element_blank(), # remover elementos
          axis.line = element_line(color = "gray40", size = 0.5),
          axis.line.y = element_blank(),
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank()))
#Resumen estadístico
mR2 <- summary(lm(HDI ~ CPI + log(CPI), data = dat))$r.squared
mR2 <- paste0(format(mR2, digits = 2), "%")

png(file = "salida.png", width = 700, height = 500)
p <- ggplot(dat,
            mapping = aes(x = CPI, y = HDI)) +
  geom_smooth(mapping = aes(linetype = "r2"),
              method = "lm",
              formula = y ~ x + log(x), se = FALSE,
              color = "red") +
  geom_point(mapping = aes(color = Region),
             shape = 1,
             size = 4,
             stroke = 1.5) +
  geom_text_repel(mapping = aes(label = Country, alpha = labels),
                  color = "gray20",
                  data = transform(dat,
                                   labels = Country %in% c("Russia","Venezuela","Iraq","Mayanmar","Sudan",
                                                           "Afghanistan","Congo","Greece","Argentina",
                                                           "Italy","Brazil","India","China","South Africa",
                                                           "Spain","Cape Verde","Bhutan","Rwanda","France",
                                                           "Botswana","France","US","Germany","Britain",
                                                           "Barbados","Japan","Norway","New Zealand",
                                                           "Sigapore"))) +
  scale_x_continuous(name = "Corruption Perception Index, 2011 (10=least corrupt)",
                     limits = c(1.0, 10.0),
                     breaks = 1:10) +
  scale_y_continuous(name = "Human Development Index, 2011 (1=best)",
                     limits = c(0.2, 1.0),
                     breaks = seq(0.2, 1.0, by = 0.1)) +
  scale_color_manual(name = "",
                     values = c("#24576D","#099DD7","#28AADC","#248E84","#F2583F","#96503F"),
                     guide = guide_legend(nrow = 1, order=1)) +
  scale_alpha_discrete(range = c(0, 1),
                       guide = FALSE) +
  scale_linetype(name = "",
                 breaks = "r2",
                 labels = list(bquote(R^2==.(mR2))),
                 guide = guide_legend(override.aes = list(linetype = 1, size = 2, 
                                                          color = "red"), order=2)) +
  ggtitle("Corruption and human development") +
  labs(caption="Sources: Transparency International; UN Human Development Report") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        text = element_text(color = "gray20"),
        axis.title.x = element_text(face="italic"),
        axis.title.y = element_text(face="italic"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust=0),
        plot.title = element_text(size = 16, face = "bold"))
p
dev.off()


## ¡Felicidades! Acaba de crear una figura que se usa en 
## medios de comunicación. 
## https://www.economist.com/graphic-detail/2011/12/02/corrosive-corruption