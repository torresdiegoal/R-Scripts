rm(list = ls())

library(MASS)
library(neuralnet)
library(dplyr)
data<-Boston


####################################
#### OPCION 1: con max y min #######
#Preparación de la Red Neuronal
maxs<-apply(data, 2, max)
mins<-apply(data, 2, min)

scaled <-as.data.frame(scale(data, center=mins, scale=maxs-mins))



#### OPCION 2: Funcion scale que resta la media #######
# Favorita
scaled2 <- data %>%    #ws with_scale
  #dplyr::select(-c('Freq','CLIENTE_ACT')) %>%
  #mutate_at(vars(-c('PUERTO_NIT_EMP_COL',)), scale ) %>%
  mutate_all(scale) %>% 
  #mutate_at(vars(-c('PUERTO_NIT_EMP_COL')), list(~ round(., 3)) ) %>%
  mutate_all(list(~ round(., 3))) %>% 
  setNames(gsub("[,1]", "", names(.))) %>%
  as.data.frame()


