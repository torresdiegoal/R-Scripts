
#your function here
rm(list = ls())

ptm <- proc.time()
##################
## PAQUETES
# A continuacion se cargan los paquetes, para instalarlos usar install.packages('Paquete')
#devtools::install_github("collectivemedia/tictoc") # instalar paquetes desde github
#suppressWarnings(suppressMessages(library(tictoc)))
#tic('Tiempo ejecucion: ') # para contar el tiempo de ejecucion del script

#Indica la ruta a donde library carga los paquetes
libraryFold <- 'C:/Users/qlikservices/AppData/Local/R/win-library/4.2'
suppressWarnings(suppressMessages(library(tidyr, lib=libraryFold)))
#suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(data.table, lib=libraryFold)))
suppressWarnings(suppressMessages(library(dplyr, lib=libraryFold)))
suppressWarnings(suppressMessages(library(tsibble, lib=libraryFold)))
suppressWarnings(suppressMessages(library(lubridate, lib=libraryFold)))
suppressWarnings(suppressMessages(library(tsibble, lib=libraryFold)))
suppressWarnings(suppressMessages(library(reshape2, lib=libraryFold))) # para pivot tablas
suppressWarnings(suppressMessages(library(zoo, lib=libraryFold)))
suppressWarnings(suppressMessages(library(ggplot2, lib=libraryFold)))# graficos

#tic()

###########################
## SISTEMA CARPETAS
#inpath  <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de PropensiÃ³n/03_ExploraciÃ³n/"
#inpath2 <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de PropensiÃ³n/04_RealizaciÃ³n/Impactos/Indicadores_Macroeconomicos/"
#outpath <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de PropensiÃ³n/04_RealizaciÃ³n/Impactos/"

######### En productivo
inpath  <- "C:/QlikSense_Target/Comercio Exterior/90_Propension/TableFiles/Export/"
#inpath2 <- "C:/QlikSense_Target/Comercio Exterior/30_QVD/Transformado/Indicadores_Macroeconomicos_TRA/"
outpath <- "C:/QlikSense_Target/Comercio Exterior/90_Propension/TableFiles/Import/"

## permite guardar el environment y sus objetos creados
# save.image(file = paste(outpath,'Impactos.RData', sep = ''))
#

##########################################################
######  CARGUE Y TRANFORMACION DE INFORMACION SISMAR
####################
#SISMAR_tmp <- read.csv(paste(inpath,"SISMAR.csv", sep = ""),
SISMAR_tmp <- read.csv(paste(inpath,"SISMAR_CSV.csv", sep = ""),
                       sep = '|', dec = ',')
#SISMAR_tmp<-SISMAR
SISMAR <- SISMAR_tmp %>%
  dplyr::select(c("NIT_EMP_COL_IMPO_EXPO","PERIODO_SISMAR","TONELADAS",'MUELLE','TIPO_CARGA' )) %>%
  dplyr::rename(NIT_EMP_COL = NIT_EMP_COL_IMPO_EXPO )

#
# #GENERAMOS UNA MAESTRA DE NOMBRES DE COMPANIAS
# SISMAR_companies <- SISMAR_tmp %>%
#   dplyr::select(c("NIT_EMP_COL_IMPO_EXPO","EMPRESA_COL_IMPO_EXPO" )) %>%
#   dplyr::rename(NIT_EMP_COL = NIT_EMP_COL_IMPO_EXPO ) %>%
#   distinct() %>%
#   filter(!is.na(EMPRESA_COL_IMPO_EXPO))
#
# # NITS con duplicados
# rep <- as.data.frame(table(SISMAR_companies$NIT_EMP_COL) ) %>%
#   filter(Freq > 1)
# SISMAR_companies %>% filter(NIT_EMP_COL == '-5690')
#
# ############# EXPORTA NOMBRES DE COMPANIAS  ##############
# write.table(SISMAR_companies,
#             file = paste(outpath, 'SISMAR_companies.csv',
#                          sep = ''),
#             fileEncoding = 'UTF-8',
#             row.names = F,
#             quote = F, #omite las comillas de cada elemento del output
#             sep = '|',
#             dec = ',',
#             na = '-')





# TONELADAS viene como chr, convertimos a numeric
# SISMAR$TONELADAS <- gsub( ",", ".", SISMAR$TONELADAS)
# SISMAR$TONELADAS <- as.numeric(SISMAR$TONELADAS)
# FECHA igual
#SISMAR$FECHA <- as.Date(SISMAR$FECHA)
# Creamos FECHA
SISMAR$FECHA <- as.Date(paste(SISMAR$PERIODO_SISMAR,'-01', sep = ""))
# Eliminamos campos temporales
SISMAR <- SISMAR %>% dplyr::select(-c('PERIODO_SISMAR'))
# limit_date contendra la informacion del mes presente
limit_date <- as.Date( round_date ( Sys.Date(), 'months')) - years(2)



#obtenemos los clientes que hayan usado los muelles de BAQ
SISMAR <- SISMAR %>%
  mutate(CLIENTES_HIST = ifelse( MUELLE %in% c("S P R BAQ", "PBQ", "BITCO"),
                                 1, 0)) %>%
  arrange(NIT_EMP_COL,FECHA)



## Creacion de la base cliente_act que nos dirÃ¡ los
## clientes actuales y los a recuperar.
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


############# EXPORTA LAS SERIES DE LOS INDICADORES  ##############
write.table(cliente_act,
            file = paste(outpath, 'cliente_act.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')

# # as.data.frame (cliente_act$CLIENTE_ACT %>% table())
# # as.data.frame (cliente_act$CLIENTE_RECUPERAR %>% table())
#
#
# # Llevamos los dos nuevos campos a SISMAR
# SISMAR <- merge(SISMAR,cliente_act,by='NIT_EMP_COL')
#SISMAR %>% str()

# Agregamos las TONELADAS a nivel de NIT y FECHA para generar las series
#SISMAR_ts_1 <- aggregate(TONELADAS ~ NIT_EMP_COL+FECHA+CLIENTE_ACT,
SISMAR_ts_1 <- aggregate(TONELADAS ~ NIT_EMP_COL+FECHA,
                         data = SISMAR,
                         FUN=sum) %>%
  arrange(NIT_EMP_COL, FECHA)


# DATR 22-10-19: Agregar filtro de TIPO_CARGA.
SISMAR_tc <- aggregate(TONELADAS ~ NIT_EMP_COL+TIPO_CARGA,
                       data = SISMAR,
                       FUN=sum) %>%
  # pivot_wider(id_cols = c(NIT_EMP_COL),
  #             names_from = TIPO_CARGA,
  #             values_from = TONELADAS,
  #             values_fn = sum) %>%
  arrange(NIT_EMP_COL)


# Tomamos solamente las empresas que tengan informacion de 12 o mÃ¡s meses.
series_aptas <- as.data.frame(SISMAR_ts_1$NIT_EMP_COL %>% table()) %>%
  arrange( desc(Freq)) %>%
  dplyr::rename(NIT_EMP_COL = '.') %>%
  filter(Freq >= 12) #%>%
#arrange(NIT_EMP_COL)

# SISMAR_ts_lastmonth <- SISMAR_ts_1 %>%
#   group_by(NIT_EMP_COL) %>%
#   summarise_all(list(last))




#
# #Toneladas por NIT
# SISMAR_ton_fecha <- aggregate(TONELADAS ~ FECHA,
#                             data = SISMAR,
#                             FUN=sum) %>%
#   arrange(FECHA)
#
#
#
# ## Para validar el total de toneladas por NITs que tienen mas de 12 puntos
# SISMAR_ton_1 <- merge(SISMAR_ton_nit, series_aptas, by='NIT_EMP_COL',
# #SISMAR_ton_1 <- merge(SISMAR_ts_lastmonth, series_aptas, by='NIT_EMP_COL',
#                       all.x = TRUE) %>%
#   mutate(masdedoce = ifelse(is.na(Freq), 0, 1)) %>%
#   arrange(NIT_EMP_COL)
#   #filter(masdedoce ==1)
#
# SISMAR_ton_1_1 <- SISMAR_ton_1 %>% filter(masdedoce ==1)
#
# a <- sum(SISMAR_ton_1_1$TONELADAS)
# b <- sum(SISMAR_ton_1$TONELADAS)
# c <- a/b

# # c<- as.data.frame(series_aptas$Freq %>% table)
# write.table(series_aptas,
#             file = paste(outpath, 'NITS_aptos.csv',
#                          sep = ''),
#             fileEncoding = 'UTF-8',
#             row.names = F,
#             quote = F, #omite las comillas de cada elemento del output
#             sep = '|',
#             dec = ',',
#             na = '-')
# b <-SISMAR %>% filter(NIT_EMP_COL %in%  c('-100'))

# INNER JOIN con aptas
SISMAR_ts_1 <- merge(SISMAR_ts_1, series_aptas, by='NIT_EMP_COL',
                     all.x = FALSE) %>%
  arrange(NIT_EMP_COL,FECHA)

# comprueba clientes y no clientes de los que tienen mas de n puntos
# b <- SISMAR_ts_1[,c(1,3)] %>% unique()
# b$CLIENTE_ACT %>% table()


# DATR 22-10-19: Filtramos tambien la base de Tipo_Carga
SISMAR_tc <- merge(SISMAR_tc, series_aptas, by='NIT_EMP_COL',
                   all.x = FALSE) %>%
  arrange(NIT_EMP_COL)


# Quitar la ultima columna del df
SISMAR_tc <- SISMAR_tc[, 1:ncol(SISMAR_tc)-1]


####  EXPORTA TIPO_CARGA EN FORMATO DE FILTRO  ##############
write.table(SISMAR_tc,
            file = paste(outpath, 'Toneladas_Tipo_Carga.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')


# #
# SISMAR_ton_aptos <- aggregate(TONELADAS ~ FECHA,
#                               data = SISMAR_ts_1,
#                               FUN=sum) %>%
#   arrange(FECHA)
#
# SISMAR_ton_prop <- merge(SISMAR_ton_fecha, SISMAR_ton_aptos, by='FECHA',
#                          all.x = TRUE) %>%
#   mutate(proporcion = round(TONELADAS.y/TONELADAS.x, 2)) %>%
#   # rename(TONELADAS_total = TONELADAS.x,
#   #        TONELADAS_aptos = TONELADAS.y) %>%
#   arrange(FECHA)
# colnames(SISMAR_ton_prop) <- c('FECHA','TONELADAS_TOTALES','TONELADAS_APTAS','prop')
#
# write.table(SISMAR_ton_prop,
#             file = paste(outpath, 'SISMAR_ton_prop.csv',
#                          sep = ''),
#             fileEncoding = 'UTF-8',
#             row.names = F,
#             quote = F, #omite las comillas de cada elemento del output
#             sep = '|',
#             dec = ',',
#             na = '-')

##########################################################
####  CARGUE Y TRANFORMACION DE INFORMACION SISMAR
####################
#Indicadores2 <- Indicadores
Indicadores <- read.csv(paste(inpath,"CONSOLIDADO.csv", sep = ""),
                        sep = '|',dec=',')

Indicadores <- Indicadores %>%
  dplyr::rename(ANIOMES = "ï..ANIOMES")

# reemplazo todos los ',' por '.' que hay en la base
# Indicadores <- data.frame(lapply(Indicadores, function(x) {
#   gsub(",", ".", x)
# }))
# cAMPO FECHA
Indicadores$FECHA <- as.Date(paste(Indicadores$ANIOMES,
                                   '-01', sep = ""))
#colnames(Indicadores)
# Convertimos a numericos
Indicadores <- Indicadores %>%
  #dplyr::select(-c(1:3,5,9,13:15)) %>%
  dplyr::select(c(FECHA,IPC:PIB_BR)) %>%
  filter(FECHA >= min(SISMAR_ts_1$FECHA)) #filtra por la primera fecha registrada en SISMAR
#filter(FECHA >= '2019-01-01')
#mutate_at(vars(-c('FECHA')),as.numeric )


# despivotamos para generar una tabla de solo 3 campos
Indicadores_melt <- melt(Indicadores ,id.vars = c('FECHA'),
                         variable.name = "INDICADOR",value.name = "VALOR")

############# EXPORTA LAS SERIES DE LOS INDICADORES  ##############
write.table(Indicadores_melt,
            file = paste(outpath, 'Indicadores macroeconomicos.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')
#Toneladas por NIT
# SISMAR_ton_nit <- aggregate(TONELADAS ~ NIT_EMP_COL,
#                          data = SISMAR,
#                          FUN=sum) %>%
#   arrange(NIT_EMP_COL)

#
SISMAR_ts_ns <- merge(SISMAR_ts_1, Indicadores, by='FECHA', #ns No_Scale
                      all.x = TRUE) %>%
  arrange(NIT_EMP_COL,FECHA)




SISMAR_ST <- SISMAR_ts_ns %>%      #st sign_test
  #dplyr::select(-c('Freq','CLIENTE_ACT')) %>%
  dplyr::select(-c('Freq')) %>%
  group_by(NIT_EMP_COL) %>%
  mutate_at(vars(-c('FECHA','NIT_EMP_COL',)), list(diff = tsibble::difference) ) %>%
  #mutate(across(contains('_diff')), list(sign = sign) )
  mutate_at(vars(contains('diff')), list(sign = sign)) %>%
  #separate(name, into = c("letter", "number"), sep = 1) %>%  # like split, opposite: unite()
  select_at(vars(contains(c('FECHA','NIT_EMP_COL','sign')))) #%>%
#rename_at( vars( contains( "sign") ), list( ~paste("cat", gsub("_cat", "", .), sep = "_") ) ) %>%
#$setNames(gsub("_diff_sign", "", names(.)))
#group_by(NIT_EMP_COL) %>%
#table() #%>%
#as.data.frame()




#Impactos_macroeconomicos <- data.frame()
indicad <- colnames(SISMAR_ST[,4:10])
for (i in indicad) {
  j <- gsub('_diff_sign','',all_of(i))
  #j <- gsub('_diff_sign','',"IPC_diff_sign" )
  SISMAR_ST[[all_of(j)]] <- ifelse(SISMAR_ST$TONELADAS_diff_sign == SISMAR_ST[[all_of(i)]] , 1,0)
  # SISMAR_ST  <- SISMAR_ST %>%
  #   mutate(ifelse(TONELADAS_diff_sign == IPC_diff_sign , 1,0))
}

# a <- SISMAR_ST %>%
#   select_at(vars(contains(c('TONELADAS','IPC')))) %>%
#   filter(NIT_EMP_COL=='5234' )
#
# a$IPC %>% table()
# sum(!is.na(a$PIB_BR))


# Nombres de Indicadores
indicad <- colnames(SISMAR_ST[c(11:17)])


# Tabla de Impactos Macroeconomicos
Impactos_macroeconomicos <- SISMAR_ST %>%
  select_at(vars(!contains(c('FECHA','diff_sign')))) %>%
  group_by(NIT_EMP_COL) %>%
  dplyr::summarise(across(indicad, ~ sum(., na.rm = TRUE) / sum(!is.na(.)))) %>%
  mutate_at(vars(-NIT_EMP_COL), list(~ round(., 3)) )

# b <- Impactos_macroeconomicos %>%
#   #select_at(vars(contains(c('TONELADAS','IPC')))) %>%
#   filter(NIT_EMP_COL=='5234' )

###########################
write.table(Impactos_macroeconomicos,
            file = paste(outpath, 'Impactos macroeconomicos original.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')



# despivotamos para generar una tabla de solo 3 campos, consumida por Qlik
Impactos_macroeconomicos_melt <- melt(Impactos_macroeconomicos ,id.vars = c('NIT_EMP_COL'),
                                      variable.name = "INDICADOR",value.name = "VALOR")



###########################
write.table(Impactos_macroeconomicos_melt,
            file = paste(outpath, 'Impactos macroeconomicos.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')


indicad <- colnames(Impactos_macroeconomicos[-1])
topXIndicador <- data.frame()
for (i in indicad) {
  transf_tmp <- Impactos_macroeconomicos %>%
    dplyr::select(c('NIT_EMP_COL', all_of(i))) %>%
    arrange_at(2, desc) %>%
    head(20)
  
  colnames(transf_tmp) <- c(all_of(i), 'NIT_EMP_COL')
  
  if(dim(topXIndicador)[1] == 0) {
    topXIndicador <- transf_tmp
  }
  else {
    topXIndicador <- cbind(topXIndicador, transf_tmp[1])
  }
  
}

topXIndicador <- topXIndicador[-2]
#topXIndicador

# despivotamos para generar una tabla de solo 3 campos
topXIndicador_melt <- melt(topXIndicador,
                           variable.name = "INDICADOR",
                           value.name = "VALOR")

# Creamos campo de orden del pareto por Indicador
topXIndicador_melt$ORDEN <- ave(topXIndicador_melt$VALOR,
                                topXIndicador_melt$INDICADOR,
                                FUN = seq_along)

###########################
# write.table(topXIndicador_melt,
#             file = paste(outpath, 'Top_X_Indicador.csv',
#                          sep = ''),
#             fileEncoding = 'UTF-8',
#             row.names = F,
#             quote = F, #omite las comillas de cada elemento del output
#             sep = '|',
#             dec = ',',
#             na = '-')


# SISMAR_ts_ns[is.na(SISMAR_ts_ns)] <- 0

# estandariza la tabla ns
# SISMAR_ts_ws <- SISMAR_ts_ns %>%    #ws with_scale
#   dplyr::select(-c('Freq','CLIENTE_ACT')) %>%
#   mutate_at(vars(-c('FECHA','NIT_EMP_COL',)), scale ) %>%
#   setNames(gsub("[,1]", "", names(.))) %>%
#   #rename_at( vars( contains( "_cat") ), list( ~paste("cat", gsub("_cat", "", .), sep = "_") ) )
#   mutate_at(vars(-c('FECHA','NIT_EMP_COL')), list(~ round(., 3)) ) %>%
#   as.data.frame()
# SISMAR_ts_ws[is.na(SISMAR_ts_ws)] <- 0
#
# # Unpivot SISMAR_ts_ws
# SISMAR_ts_melt <- melt(SISMAR_ts_ws ,  id.vars = c('NIT_EMP_COL','FECHA'))
#
# a <- SISMAR_ST %>% filter(NIT_EMP_COL=='5234' ) #%>%
# a$TONELADAS_diff_sign %>%  table()
#dplyr::select(-c(2,8:10))

#
# par(mar=c(1,1,1,1)) # permite aumentar el tamaÃ±o del plot panel
# ggplot(a, aes(FECHA,value)) + geom_line(aes(colour = variable)) #%>% dev.off()

# Guardamos proyecto .RData
save.image(file = paste(outpath,'Propension_20_Impactos.RData', sep = ''))


print('Propension_Impactos DONE')
time  <- proc.time() - ptm
print(time)