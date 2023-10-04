
#your function here
rm(list = ls())

ptm <- proc.time()


##################
## PAQUETES
#Indica la ruta a donde library carga los paquetes
libraryFold <- .libPaths()[1]


library(pacman)
pacman::p_load(data.table,tidyr,pROC,dplyr,plyr,stringr,lubridate,cluster,reshape2,FactoMineR,factoextra)


# suppressWarnings(suppressMessages(library(tidyr, lib=libraryFold)))
# #suppressWarnings(suppressMessages(library(tidyr)))
# suppressWarnings(suppressMessages(library(data.table, lib=libraryFold)))
# suppressWarnings(suppressMessages(library(dplyr, lib=libraryFold)))
# suppressWarnings(suppressMessages(library(plyr, lib=libraryFold)))
# suppressWarnings(suppressMessages(library(stringr, lib=libraryFold)))
# suppressWarnings(suppressMessages(library(lubridate, lib=libraryFold)))
# suppressWarnings(suppressMessages(library(cluster, lib=libraryFold)))
# suppressWarnings(suppressMessages(library(reshape2, lib=libraryFold))) # para pivot tablas
# suppressWarnings(suppressMessages(library(FactoMineR, lib=libraryFold)))
# suppressWarnings(suppressMessages(library(factoextra, lib=libraryFold)))# graficos
## permite guardar el environment y sus objetos creados
# save.image(file = paste(outpath,'env_estandariz_PCA.RData', sep = ''))


###########################
## SISTEMA CARPETAS
inpath  <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de Propensión/04_Realización/Segmentacion/"
outpath <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de Propensión/04_Realización/Segmentacion/"


#inpath  <- "C:/QlikSense_Target/Comercio Exterior/90_Propension/TableFiles/Import/"
#outpath  <- "C:/QlikSense_Target/Comercio Exterior/90_Propension/TableFiles/Import/"

###########################
## FUNCIONES
mode_f <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### 1da moda
mode_one <- function (x) {
  with(rle(sort(x)), values[order(lengths, decreasing = TRUE)][1])
}

### 2da moda
mode_two <- function (x) {
  with(rle(sort(x)), values[order(lengths, decreasing = TRUE)][2])
}

### 1ra max
max_one <- function (x) {
   x[order(x, decreasing = TRUE)][1]
}

### 2da max
max_two <- function (x) {
  x[order(x, decreasing = TRUE)][2]
}

### 2do which.max
which.max2 <- function(x) {
  which( x == max_two(x) )[1]
}
# mode_one(SISMAR$PUERTO)
# mode_two(SISMAR$PUERTO)



############################################################


#################################################################
#####                I) FASE DE TRANSFORMACION          #####   
#################################################################
### 1. CARGA DE INFORMACION
SISMAR_Segmentacion_CA <- read.csv(paste(inpath,"SISMAR_pivot.csv", sep = ""),
                       sep = '|', dec = ',')



row.names(SISMAR_Segmentacion_CA) <- 
  SISMAR_Segmentacion_CA$PUERTO_NIT_EMP_COL


SISMAR_Segmentacion_CA = SISMAR_Segmentacion_CA %>%
  dplyr::select(-PUERTO_NIT_EMP_COL)



# # TONELADAS viene como chr, convertimos a numeric
# bd.ca$TONELADAS <- gsub( ",", ".", bd.ca$TONELADAS)
# bd.ca$TONELADAS <- as.numeric(bd.ca$TONELADAS)


##########################################################################
#########             II) ALGORITMO DE SEGMENTACION              #########
##########################################################################
set.seed(315)
# Aplicamos el metodo de analisis de correspondencia (CA)
#res.ca = CA(bd.ca, graph = F)
res.pca = PCA(SISMAR_Segmentacion_CA,
              scale.unit = TRUE,
              ncp = 25,
              graph = F)

#
res.pca$var
res.pca$ind$coord
pca_variability <- as.data.frame(get_eig(res.pca)) %>% 
  filter(cumulative.variance.percent <= 85 )
# Link between the variable and the continuous variables (R-square)
dimdesc(res.pca)

# A parte obtenemos el dataset de pesos traidos desde PCA
pca_coord <- res.pca$ind$coord %>% 
  as.data.frame()

# Generamos la segmentacion utilizando el anterior dataset
res.hcpc = HCPC(pca_coord,
                nb.clust=5,
                graph = FALSE)

# Revisamos resultados
res.hcpc$data.clust$clust %>% 
  table()

# summary(res.hcpc)
# res.hcpc$desc.var

# Agregamos el valor resultante de la segmentacion a la base pivotada
sismar_hcpc <- res.hcpc$data.clust %>% 
  mutate(PUERTO_NIT_EMP_COL = row.names(.)) %>% 
  dplyr::select(c(PUERTO_NIT_EMP_COL, clust)) %>% 
  #mutate(PUERTO_NIT_EMP_COL = as.integer(PUERTO_NIT_EMP_COL)) %>% #Iba a pasar a int pero el valor limite de enteros es de 2147483647, salian 2 NAs 8900900943-10986468608
  #arrange(PUERTO_NIT_EMP_COL) %>%
  dplyr::rename(NIT_EMP_COL = PUERTO_NIT_EMP_COL)
str(sismar_hcpc)


sismar_hcpc %>% filter(is.na(NIT_EMP_COL))



SISMAR_melt <- merge(SISMAR_melt,sismar_hcpc,by='PUERTO_NIT_EMP_COL')
#########################################

write.table(SISMAR_melt,
            file = paste(outpath, 'SISMAR_Segmentacion_CA.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')


# Base agrupada por el campo clust generado por el HCPC
SISMAR_clust_mean <- res.hcpc$data.clust %>%
  group_by(clust) %>%
  summarise_all(mean)
SISMAR_clust_mean

colvars <- colnames(SISMAR_clust_mean) %>% 
  as.data.frame()


# Traemos el vector categ que almacena el nombre de TODAS los campos de la
# matriz de frecuencias
categ <- categ

#Tabla donde almacenaremos las descripciones
descripciones <- data.frame()

#En este loop filtraremos SISMAR_clust_mean con los campos relacionados
# a cada variable original, obtendremos los 2 campos o categorías con 
# mayor promedio.
for (i in categ) {
  tmp_colvars  <- colvars %>%
    filter(grepl(paste("^", all_of(i),sep = ''), .))
    #filter(grepl(paste("^", 'DEPTO_EMP_COL',sep = ''), .))
  tmp <- SISMAR_clust_mean %>%
    dplyr::select(c(tmp_colvars$.)) %>% 
    mutate(mean_1 = round(apply(., 1, max),4),
           #maximo_cat = apply(.,1,which.max)
           descr_1 = colnames(.)[apply(.,1,which.max)],
           mean_2 = round(apply(., 1, max_two),4),
           descr_2 = colnames(.)[apply(.,1,which.max2)],
           Variable = all_of(i),
           #Variable = 'DEPTO_EMP_COL',
           descripcion_1=gsub("^.*_", "", descr_1),#reemplaza ese patron a vacio
           descripcion_2=gsub("^.*_", "", descr_2),
           Segmento = paste('Segmento ',1:nrow(.),sep = '')) %>% 
    dplyr::select(Segmento,Variable,descripcion_1,mean_1,descripcion_2,mean_2)
  
  # Concatenamos cada columna resultado en una sola tabla
  if(dim(descripciones)[1] == 0) {
    descripciones <- tmp
  }
  else {
    descripciones <- rbind(descripciones, tmp)
  }
}

## Agregamos aparte la descripcion de toneladas ya que no posee categorias
descripciones2 <-  cbind(Segmento=unique(descripciones$Segmento),
                         Variable='TONELADAS',descripcion_1=NA,
                         mean_1=round(unique(SISMAR_clust_mean$TONELADAS),3),
                         mean_2=NA,descripcion_2=NA) %>%
  as.data.frame()

descripciones <- rbind(descripciones, descripciones2)
#Ordenamos la tabla por segmentos
descripciones <- descripciones %>% arrange(Segmento)


#Exportamos las descripciones
write.table(descripciones,
            file = paste(outpath, 'descripciones.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')

# Limpieza
#rm('colvars','tmp','tmp_colvars','transf_tmp')

# SISMAR %>%  filter(NIT_EMP_COL=='890300554')
# 890300554

############################################################



print('Propension_102_Segmentacion DONE')
time  <- proc.time() - ptm
print(time)


# Guardamos proyecto
save.image(file = paste(outpath,'Propension_102_Segmentacion.RData', sep = ''))
