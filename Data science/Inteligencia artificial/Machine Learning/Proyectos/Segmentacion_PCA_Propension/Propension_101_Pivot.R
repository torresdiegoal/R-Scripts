
#your function here
rm(list = ls())

ptm <- proc.time()


##################
## PAQUETES
#Indica la ruta a donde library carga los paquetes
#libraryFold <- 'C:/Users/qlikservices/AppData/Local/R/win-library/4.2'
libraryFold <- .libPaths()[1]

suppressWarnings(suppressMessages(library(tidyr, lib=libraryFold)))
#suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(data.table, lib=libraryFold)))
suppressWarnings(suppressMessages(library(dplyr, lib=libraryFold)))
suppressWarnings(suppressMessages(library(plyr, lib=libraryFold)))
suppressWarnings(suppressMessages(library(stringr, lib=libraryFold)))
suppressWarnings(suppressMessages(library(lubridate, lib=libraryFold)))
suppressWarnings(suppressMessages(library(cluster, lib=libraryFold)))
suppressWarnings(suppressMessages(library(reshape2, lib=libraryFold))) # para pivot tablas
suppressWarnings(suppressMessages(library(FactoMineR, lib=libraryFold)))
suppressWarnings(suppressMessages(library(factoextra, lib=libraryFold)))# graficos
## permite guardar el environment y sus objetos creados
# save.image(file = paste(outpath,'env_estandariz_PCA.RData', sep = ''))


###########################
## SISTEMA CARPETAS
inpath  <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de Propensión/03_Exploración/"
#inpath  <- "C:/QlikSense_Target/Comercio Exterior/90_Analitica/TableFiles/Export/"

outpath <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de Propensión/04_Realización/Segmentacion/"
#outpath <- "C:/QlikSense_Target/Comercio Exterior/90_Analitica/TableFiles/Import/"


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
#################################################################
#####                I) FASE DE TRANSFORMACION          #####   
#################################################################
### 1. CARGA DE INFORMACION
#SISMAR_tmp <- read.csv(paste(inpath,"SISMAR_CSV.csv", sep = ""),
SISMAR_tmp <- read.csv(paste(inpath,"SISMAR.csv", sep = ""),
                         sep = '|', dec = ',')
#colnames(SISMAR)
SISMAR <- SISMAR_tmp %>% 
  dplyr::select(c("NIT_EMP_COL_IMPO_EXPO","PERIODO.SISMAR","PUERTO","MUELLE","TONELADAS","TIPO_CARGA","INTERCAMBIO",
                  "REGISTRO_CAPITANIA","CIUDAD_EMP_COL_IMPO_EXPO","PAIS_PUERTO_EXTRANJERO","DPTO_2","NAVIERA" )) %>% 
  dplyr::rename(DEPTO_EMP_COL = DPTO_2,
                CIUDAD_EMP_COL=CIUDAD_EMP_COL_IMPO_EXPO, 
                NIT_EMP_COL = NIT_EMP_COL_IMPO_EXPO )
#SISMAR %>% str()



#
### 2. Transformaciones adicionales
# TONELADAS viene como chr, convertimos a numeric
# SISMAR$TONELADAS <- gsub( ",", ".", SISMAR$TONELADAS)
# SISMAR$TONELADAS <- as.numeric(SISMAR$TONELADAS)



# 
# 3. SISMAR_orig es la base completa, SISMAR sera la base agrupada por las demas vatiables categoricas
SISMAR_orig <- SISMAR

categ <- SISMAR_orig %>% 
  dplyr::select(-c('TONELADAS')) %>% 
  colnames()

# Agrupamos toda la base SISMAR utilizando el nuevo campo REGISTRO_CAPITANIA
SISMAR <- SISMAR_orig %>% 
  dplyr::group_by(across(all_of(categ))) %>% 
  dplyr::summarise(across('TONELADAS', sum)) %>% 
  as.data.frame()
# b <-SISMAR2 %>% filter(NIT_EMP_COL %in%  c('1152205168')) 
b <-SISMAR %>% filter(NIT_EMP_COL %in%  c('901091496',	'830007355',	'830023671',	'830005433',	'860001498',	'860052601',	'860518543',	'900205225',	'900648510',	'900894351',	'901254735',	'901097194',	'901192317',	'900279810',	'900646912',	'890900453',	'900202606',	'901423585',	'860058433',	'901233096'))
unique(b$MUELLE)


# Creamos FECHA
SISMAR$FECHA <- as.Date(paste(SISMAR$PERIODO.SISMAR,'-01', sep = ""))
# Eliminamos campos temporales
SISMAR <- SISMAR %>% dplyr::select(-c('PERIODO.SISMAR'))
# limit_date contendra la informacion del mes presente
limit_date <- as.Date( round_date ( Sys.Date(), 'months')) - years(2)



#obtenemos los clientes que hayan usado los muelles de BAQ
SISMAR <- SISMAR %>% 
  mutate(CLIENTES_HIST = ifelse( MUELLE %in% c("S P R BAQ", "PBQ", "BITCO"),
                                 1, 0)) %>% 
  arrange(NIT_EMP_COL,FECHA)



# Creacion de la base cliente_act que nos dirá los
# clientes actuales y los a recuperar.
cliente_act <- SISMAR %>%
  dplyr::select(NIT_EMP_COL,FECHA,CLIENTES_HIST) %>%
  group_by(NIT_EMP_COL) %>%
  #summarise_all(list(max=max, last=last)) %>%
  summarise_all(list(max)) %>%
  # rename_with(toupper) %>% 
  # dplyr::rename(FECHA=FECHA_MAX) %>% 
  mutate(CLIENTE_ACT = 
           ifelse(FECHA >= limit_date &
                    CLIENTES_HIST == 1, 
                  1, 0),
         CLIENTE_RECUPERAR = 
           ifelse(FECHA < limit_date &
                    CLIENTES_HIST == 1,
                  1, 0) )  %>% 
  dplyr::select(NIT_EMP_COL,
                CLIENTE_ACT,CLIENTE_RECUPERAR ) %>% 
  as.data.frame()
# as.data.frame (cliente_act$CLIENTE_ACT %>% table())
# as.data.frame (cliente_act$CLIENTE_RECUPERAR %>% table())


# Llevamos los dos nuevos campos a SISMAR
SISMAR <- merge(SISMAR,cliente_act,by='NIT_EMP_COL')

# BACK-UP de SISMAR
SISMAR_orig2 <- SISMAR
#  SISMAR <- SISMAR_2



# Para el loop usamos unicamente las variables categoricas
categ <- SISMAR %>% 
  dplyr::select(-c('NIT_EMP_COL','TONELADAS','CLIENTE_ACT',
                   'FECHA','CLIENTE_RECUPERAR','REGISTRO_CAPITANIA',
                   'CLIENTES_HIST')) %>% 
  colnames()

# Este loop 1) transforma las categorias de cada campo de la base de SISMAR, de modo que
# aquellas que no describan al menos el 1% de la informacion o que no este en el top 10
# por campo, haran parte de la la categoria global: 'OTROS'. 2) pivotea SISMAR de manera 
# que las categorias de cada variable seran los nuevos campos y sus valores seran los 
# CONTEOS de la base agrupada de SISMAR a nivel de REGISTRO_CAPITANIA.
SISMAR_Segmentacion_CA <- data.frame()
for (i in categ) {
  # Generamos tablas de frecuencia para cada campo y selecionamos el top 15 de 
  # categorias mas frecuentes
  tmp <- as.data.frame(SISMAR[[all_of(i)]] %>% table()) %>% 
            arrange( desc(Freq))
  tmp <- tmp %>%   
    filter(!(. %in%  c('0',''))) %>%
    mutate(cumSum = cumsum(Freq),
           percent = cumSum / sum(Freq),
           aporte = round(percent- lag(percent),5)) %>% 
    filter(if (all_of(i) != "TIPO_CARGA") 
      aporte >= 0.01 | is.na(aporte) else TRUE) %>% # else TRUE es para que no haga nada si no cumple la condicion
    head(10) 
  colnames(tmp) <- c(all_of(i),'Freq','cumSum','percent','aporte')
  #subset(head(10)) percent <= 0.83 | 
  
  #Crea una tabla con el nombre de la variable i
  #assign(paste0("VAR_", i), tmp)
  
  # Se reemplaza en la tabla SISMAR los valores poco frecuentes por 'Otros'
  SISMAR[[all_of(i)]] <- ifelse(SISMAR[[all_of(i)]] %in% tmp[[all_of(i)]],
                        SISMAR[[all_of(i)]],
                        ifelse(is.na(SISMAR[[all_of(i)]]),NA,'OTROS'))
  
  # Pivoteamos campo a campo cada una de sus categorias y sumamos el numero de registros
  # por cliente y por cada categoria de cada campo
  transf_tmp <- SISMAR %>% 
    dplyr::select(c('NIT_EMP_COL', all_of(i))) %>% 
    #mutate(PUERTO=factor(PUERTO)) %>% 
    #mutate(PUERTO_unclass = unclass(PUERTO)) %>% arrange(NIT_EMP_COL_IMPO_EXPO)
    mutate(valor = ifelse(is.na(all_of(i)),0,1)) %>% 
    pivot_wider(id_cols = c(NIT_EMP_COL), 
                names_from = all_of(i), 
                values_from = valor, 
                values_fn = sum) %>%
    arrange(NIT_EMP_COL)
  
  colnames(transf_tmp) <- paste0(all_of(i), "|", colnames(transf_tmp))
  transf_tmp[is.na(transf_tmp)] <- 0
  
  indicad <- colnames(transf_tmp)[-1]
  grp <- colnames(transf_tmp)[1]
  transf_tmp2 <- transf_tmp %>%
    group_by(.[grp]) %>% 
    mutate(total = rowSums(.[indicad], na.rm = TRUE)) %>% 
    dplyr::summarise(across(indicad, ~ ./total))
  
  # Concatenamos cada columna resultado en una sola tabla, la cual seria la base 1 
  # para K-means
  if(dim(SISMAR_Segmentacion_CA)[1] == 0) {
    SISMAR_Segmentacion_CA <- transf_tmp2
  }
  else {
    SISMAR_Segmentacion_CA <- cbind(SISMAR_Segmentacion_CA, transf_tmp2[-1])
  }
}

# Creamos la maestra de total de toneladas por cliente
toneladas <- aggregate(SISMAR$TONELADAS, 
                       list(SISMAR$NIT_EMP_COL), 
                       FUN=sum) 
colnames(toneladas) <- c('NIT_EMP_COL','TONELADAS')


#Concatenamos el campo de toneladas agrupadas a la base CA
SISMAR_Segmentacion_CA <- cbind(SISMAR_Segmentacion_CA, toneladas[-1])


# CAMBIAMOS TODOS LOS NA DE LA TABLA A 0
SISMAR_Segmentacion_CA[is.na(SISMAR_Segmentacion_CA)] <- 0



# despivotamos para generar una tabla de solo 3 campos
SISMAR_melt <- melt(SISMAR_Segmentacion_CA ,id.vars = c('PUERTO|NIT_EMP_COL'),
                    variable.name = "Categorias",value.name = "Conteos")

#  Separamos el campo Categorias
SISMAR_melt <- SISMAR_melt %>% 
  setNames(gsub("[|]", "_", names(.))) %>% 
  mutate(Variables = gsub("[|].*$", "", Categorias), # Todo adelante de '|°
         Categorias = gsub("^.*[|]", "", Categorias))  # Todo antes de '|°
#   separate(Categorias,
#            into = c('Variables', 'Cat'),
#            sep = "[|]",
#            remove = TRUE)
# b <- SISMAR_melt %>% 
#   #select_at(vars(contains(c('TONELADAS','IPC')))) %>%
#   filter(PUERTO_NIT_EMP_COL=='-6927' ) 

SISMAR_Segmentacion_CA <- SISMAR_Segmentacion_CA %>% 
  setNames(gsub("[|]", "_", names(.)))


# Escalamos la base entera de segmentacion
SISMAR_Segmentacion_CA2 <- SISMAR_Segmentacion_CA %>%    #ws with_scale
  #dplyr::select(-c('Freq','CLIENTE_ACT')) %>%
  mutate_at(vars(-c('PUERTO_NIT_EMP_COL',)), scale ) %>%
  mutate_at(vars(-c('PUERTO_NIT_EMP_COL')), list(~ round(., 3)) ) %>%
  setNames(gsub("[,1]", "", names(.))) %>%
  as.data.frame()

#colnames(SISMAR_Segmentacion_CA2)
#SISMAR_Segmentacion_CA2 <- SISMAR_Segmentacion_CA

###### EXPORTAMOS LA TABLA DE SEGMENTACION SIN PIVOTEAR ####
write.table(SISMAR_Segmentacion_CA2,
            file = paste(outpath, 'SISMAR_pivot_scaled.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')

# Base a importar en Propension_102_Segmentacion.R
write.table(SISMAR_Segmentacion_CA,
            file = paste(outpath, 'SISMAR_pivot.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')


############################################################



print('Propension_101_Pivot DONE')
time  <- proc.time() - ptm
print(time)


# Guardamos proyecto
save.image(file = paste(outpath,'Propension_101_Pivot.RData', sep = ''))
