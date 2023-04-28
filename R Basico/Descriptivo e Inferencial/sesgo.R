#Para ilustrar el sesgo basta obtener una muestra aleatoria de una determinada
#distribuci´on, Ejemplo considere 10 muestras de una N(10,4), determine o
#vies no R.
set.seed(20)
sesgo=mean(rnorm(10,10,4))-10
sesgo


#Algoritmo con simulaciónn de Monte Carlo:
set.seed(20)
ns= seq(2, 100,by=2)#inicia en 2 termina en 1000 a cada 2
sesgo= numeric(length(ns)) # prepara 500 elementos (vector de ceros)
for (i in 1:length(ns)){ # cadena o lazo
  muestra= rnorm(ns[i],10,4) # números aleatorios N(10,4)
  sesgo[i]= mean(muestra)-10 # diferencia de medias (muestra y parametro)
}
# gráfico del tamañoo y la media de la muestra
plot(ns, sesgo, main="Desempeño del sesgo  para varios tamanos de n",
     type="s",ylab="vies")
abline(h=0,col=2)





#####consistencia
###################################3

#### Algoritmo #####
#media de la distribución Normal con media 10 y varianza 16
#el estimador es la media de la muestra en diferentes tamaños normales.
ns= c(2, seq(5, 1000, by=5), seq(1010, 5000, by=10))
estim= numeric(length(ns)) #prepara 601 elementos (vector de ceros)
for (i in 1:length(ns)){ #lazo (de 1 a 601 realizar)
  muestra= rnorm(ns[i], 10, 4) #números aleatorios N(10,16)
  estim[i]=mean(muestra) #aloca la media de la muestra en estim
}
plot(ns, estim,type="l", main="Consistencia de un estimador
\n N(10,16)") #gráfico del tamaño y la media de la muestra
abline(h=10, col=2) # l´inea de la media =10
