rm(list = ls())

######### SEGMENTACION POR QUANTILES ####
# Generar un vector de datos de ejemplo
datos <- c(12, 15, 17, 20, 22, 25, 28, 30, 32, 35)
datos
# Segmentar en cuartiles (4 grupos)
cuartiles <- quantile(datos, probs = seq(0, 1, 0.20))
cuartiles

# Segmenta los datos según los cuartiles
segmentos <- cut(datos, breaks = cuartiles, 
                  include.lowest = TRUE)
segmentos


######### SEGMENTACION POR RANGOS ####
# Generar un vector de datos de ejemplo
datos <- c(12, 15, 17, 20, 22, 25, 28, 30, 32, 35)

# Calcular el rango de los datos
rango <- max(datos) - min(datos)

# Especificar el número de grupos deseados
num_grupos <- 5

# Calcular el tamaño del intervalo para la segmentación
intervalo <- rango / num_grupos

# Calcular los límites de los grupos
limites <- seq(min(datos), max(datos), by = intervalo)
limites

# Etiquetar los datos según los límites de los grupos
segmentos <- cut(datos, breaks = limites, labels = paste("Grupo", 1:num_grupos))

# Mostrar los segmentos resultantes
segmentos