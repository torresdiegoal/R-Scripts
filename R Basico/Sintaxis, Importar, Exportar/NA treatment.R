rm(list = ls())

#####################################
# HOW TO FILL NA'S WITH MEAN VALUES?

library(ggplot2)

## EJEMPLO ##

aq <- data.frame(airquality)
colSums(is.na(aq)) # Funcion base. Retorna el número de NA's por columnas del set de datos
rowSums(is.na(aq)) # Lo mismo pero con las filas

# EX: imput monthly mean in Ozone but in another df just to avoid affect the original one
aq2 <- aq
for(i in 1:nrow(aq)){
  if(is.na(aq[i, "Ozone"])){
    aq2[i,"Ozone"] <- round(mean(aq[which(aq[,"Month"] == aq[i,"Month"]), "Ozone"], na.rm = T), 2)
  }
}

ggplot(data=aq) + geom_histogram(mapping=aes(Ozone))


## Identify and Filter non-NA rows as rows with NA values cause problems 

rows <- rowSums(is.na(aq)) == 0 # = 0 es que ninguna fila tenga ni siquiera un NA
aq2 <- aq[rows,]
# O
aq2 <- na.exclude(aq) # Una funcion mas rapida

