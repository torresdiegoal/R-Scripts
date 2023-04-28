rm(list = ls())

####################
#### Mapeo con R ###
####################

# Para cargar varias librerias al tiempo
library(pacman)

#Para carga de información raster
library(raster)

#Para manejo de información tanto vectorial como raster
library(maptools)
 
# manipulación de información geografica especialmente raster
library(rgdal)


###  Sistema de carpeta ### 
inpath <- "C:/Users/Diego/Dropbox/Mapeo R/Datos/Col_deptos/"
outpath <- "C:/Users/Diego/Dropbox/Mapeo R/Datos/Col_deptos/"

# Cargar archivos raster (paquete raster)
tx_prom_2140 <- raster(paste(inpath,"CNRM_21_40_ssp585/wc2.1_10m_tmax_CNRM-CM6-1_ssp585_2021-2040.tif", sep = ""))
plot(tx_prom_2140)

#Cargar features de un shapefile (paquete rgdal)
Col_tx <- readOGR(paste(inpath,"deptos_wgs84.shp", sep = ""))
plot(Col_tx)

#Recortar raster al area del poligono (Paquete raster)
tx_prom2041_col <- crop(tx_prom_2140, Col_tx)
plot(tx_prom2041_col)

#Plotear raster y polígono a la vez
plot(tx_prom2041_col)
plot(Col_tx, add = TRUE)
