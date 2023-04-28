rm(list = ls())

library(pacman)

#broom permite contruir geodatabase a partir de shapes
pacman::p_load(rgdal, tidyverse, broom, raster, rgeos, sf)

###  Sistema de carpeta ### 
inpath <- "C:/Users/Diego/Dropbox/Mapeo R/Datos/Col_municip/meanTemp/"
#inpath <- "C:/Users/57311/Dropbox/Mapeo R/Datos/Col_municip/meanTemp/"
outpath <- "C:/Users/Diego/Dropbox/Mapeo R/Datos/Col_municip/"

### Cargar features de un shapefile (paquete rgdal)
col_tm <- readOGR(paste(inpath,"mean_temp.shp", sep = ""))
col_tm$ID_ESPACIA <- as.character(col_tm$ID_ESPACIA)

### coordinate reference system, consulta el sistema de referencia actual del objeto
raster::crs(col_tm)

### permite que un vector de caracteres pueda integrar las tildes del español
col_tm$NOM_MUNICI <- iconv(col_tm$NOM_MUNICI,from = "UTF-8", to = "latin1")
col_tm$NOMBRE_DPT <- iconv(col_tm$NOMBRE_DPT,from = "UTF-8", to = "latin1")

### convierte el shapefile en tabla, lo cual permite que pueda plotearse en ggplot
col_tm_tdy <- tidy(col_tm, 'ID_ESPACIA') #broom
col_mun <- tibble(id_espac = col_tm$ID_ESPACIA, mpio = col_tm$NOM_MUNICI, temp = col_tm$MEAN)
col_num <- inner_join(col_tm_tdy, col_mun, by = c("id" = "id_espac"))
#col_tm_tdy2 <- st_as_sf(col_tm)

### limites de paises con raster::gedData
 #### name = GADM: global administrative boundaries
 #### name = alt: altitude
 #### name = worldclim is a database of global interpolated climate data
pnm <- raster::getData('GADM', country = 'PAN', level = 0) # cero descarga unicamente el shape
#col1 <- raster::getData('GADM', country = 'COL', level = 1) # UNO para división politica departamental
#col2 <- raster::getData('GADM', country = 'COL', level = 2) # DOS para división politica municipal
bra <- raster::getData('GADM', country = 'BRA', level = 0)
per <- raster::getData('GADM', country = 'PER', level = 0)
ecu <- raster::getData('GADM', country = 'ECU', level = 0)
ven <- raster::getData('GADM', country = 'VEN', level = 0)

pnm <- tidy(pnm, 'NAME_0')
bra <- tidy(bra, 'NAME_0')
per <- tidy(per, 'NAME_0')
ecu <- tidy(ecu, 'NAME_0')
ven <- tidy(ven, 'NAME_0')

# Creación del mapa
mapa <- ggplot() +
  geom_polygon(data = col_num, aes (x = long, y = lat, group = id, fill = temp), colour = 'grey40')+
  scale_fill_distiller(palette = 'Spectral')+
  geom_polygon(data = pnm, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = ven, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = ecu, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = bra, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  geom_polygon(data = per, aes(x = long, y = lat, group = group), fill = NA, colour = 'grey')+
  coord_equal(xlim = c(-80,-66), ylim = c(-4.22, 13))+
  xlab('Longitud')+
  ylab('Latitud')+
  labs(fill = 'Temperatura (C)')  + 
  ggtitle("Temperatura Promedio por Municipio de Colombia")+ 
  theme_bw() + theme_light() + 
  theme(axis.line = element_line(colour = "gray40", 
                                 size = .5, linetype = "solid"))+
  theme(legend.justification = c(0,0),
        legend.position = c(0.005, 0.005),
        legend.background = element_rect(fill= alpha('white', 1), colour = alpha('grey40', 0.4)))

ggsave(plot = mapa, filename = "C:/Users/Diego/Dropbox/Mapeo R/Datos/Col_municip/mapaTemperatura.png", units = "in", width =6, height = 7, dpi = 300)
