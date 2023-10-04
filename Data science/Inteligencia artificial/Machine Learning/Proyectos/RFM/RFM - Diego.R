rm(list = ls())


library(readxl)
library(dplyr)


##########################################################
######  FUNCIONES
####################


## Interquartile Range (IQR)
#Se calculan los limites superiores e inferiores para identificación de posibles datos atipicos
# IQR = Q3 - Q1 
# LI = Q1–1.5 * IQR; LS = Q3+1.5 * IQR.
IQR <- function(variable){
  LI=quantile(variable,0.25)-(1.5*(quantile(variable,0.75)-quantile(variable,0.25)))
  LS=quantile(variable,0.75)+(1.5*(quantile(variable,0.75)-quantile(variable,0.25)))
  return(c(LI,LS))
}


#Creamos variables temporales donde excluimos los datos atipicos de cada indicador
categorizacion <- function(variable,outliers,reciencia){
  LI=outliers[1]
  LS=outliers[2]
  VarTemp=variable[variable>=LI & variable<=LS]
  
  # break_1: quantiles sin outliers
  breaks_1=c(min(variable)-0.001,quantile(VarTemp,probs = seq(0,1,0.2))[2:5],max(variable)+0.001)
  
  # break_2: quantiles con outliers
  breaks_2=quantile(variable,probs = seq(0,1,0.2))
  breaks_2[1]=breaks_2[1]-0.001
  breaks_2[6]=breaks_2[6]+0.001
  
  # break_3: rangos fijos con outliers
  breaks_3=seq(min(variable)-0.001,max(variable)+0.001,length.out= 6)
  
  # Tres casos para asignar puntajes
  if(reciencia==FALSE){
    # Usar el break sin outliers sin que quantiles repitan valores
    if(sum(duplicated(breaks_1))==0){
      categorias=cut(variable, breaks=breaks_1,labels = 1:5) %>% as.numeric()
    }
    # Usar el break con outliers sin que quantiles repitan valores
    if(sum(duplicated(breaks_1))!=0 & sum(duplicated(breaks_2))==0){
      categorias=cut(variable, breaks=breaks_2,labels = 1:5) %>% as.numeric()
    }
    # Usar el break con outliers sin usar quantiles sino rangos fijos
    if(sum(duplicated(breaks_1))!=0 & sum(duplicated(breaks_2))!=0){
      categorias=cut(variable, breaks=breaks_3,labels = 1:5) %>% as.numeric()
    }
  }
  # restar 6 y multiplicar -1 invierte el puntaje para Recencia, es decir, si el puntaje mayor es 5 para los valores mas grandes 
  # en los otros casos, en Recencia los menores son los de mayor puntaje
  if(reciencia==TRUE){
    if(sum(duplicated(breaks_1))==0){
      categorias=((cut(variable, breaks=breaks_1,labels = 1:5) %>% as.numeric())-6)*(-1)
    }
    if(sum(duplicated(breaks_1))!=0 & sum(duplicated(breaks_2))==0){
      categorias=((cut(variable, breaks=breaks_2,labels = 1:5) %>% as.numeric())-6)*(-1)
    }
    if(sum(duplicated(breaks_1))!=0 & sum(duplicated(breaks_2))!=0){
      categorias=((cut(variable, breaks=breaks_3,labels = 1:5) %>% as.numeric())-6)*(-1)
    }
  }
  
  return(categorias)
}



RFMManar <- function(BD){
  rfm_df=BD
  LRecencia=IQR(rfm_df$Recencia)
  LMonto=IQR(rfm_df$Monto)
  LFrecuencia=IQR(rfm_df$Frecuencia)
  
  rfm_df$ScoreRecencia=categorizacion(rfm_df$Recencia,LRecencia,reciencia=TRUE)
  rfm_df$ScoreFrecuencia=categorizacion(rfm_df$Frecuencia,LFrecuencia,reciencia=FALSE)
  rfm_df$ScoreMonto=categorizacion(rfm_df$Monto,LMonto,reciencia=FALSE)
  
  rfm_df$Score=NULL
  rfm_df$Score=as.numeric(paste(rfm_df$ScoreRecencia,rfm_df$ScoreFrecuencia,rfm_df$ScoreMonto,sep=""))
  
  return(rfm_df)
}




SegmentosRFMManar = function(df_rfm,customer){
  # df_rfm=rfm_df
  
  if(customer==TRUE){
    rfm_score_table <- df_rfm %>% 
      mutate(segment = dplyr::case_when(Score %in% list(555, 554, 544, 545, 454, 455, 445) ~ "Campeones",
                                        Score %in% list(543, 444, 435, 355, 354, 345, 344, 335) ~ "Leales",
                                        Score %in% list(553, 551,552, 541, 542, 533, 532, 531, 452, 451, 442, 441, 
                                                        431, 453, 433, 432, 423, 353, 352, 351, 342, 341, 333, 323) ~ "Leales en potencia",
                                        Score %in% list(512,511, 422, 421, 412, 411, 311) ~ "Clientes recientes",
                                        Score %in% list(525, 524, 523, 522, 521, 515, 514, 513, 425, 424, 413,414, 415, 
                                                        315, 314, 313) ~ "Prometedores",
                                        Score %in% list(535, 534, 443, 434, 343, 334, 325, 324) ~ "Necesitan atención",
                                        Score %in% list(331, 321, 312, 221, 213) ~ "A punto de dormir",
                                        Score %in% list(255, 254, 245, 244, 253, 252, 243, 242, 235, 234, 225, 224, 
                                                        153, 152, 145, 143, 142, 135, 134, 133, 125, 124) ~ "En riesgo",
                                        Score %in% list(155, 154, 144, 214,215,115, 114, 113) ~ "No puedes perderlos",
                                        Score %in% list(332, 322, 231, 241, 251, 233, 232, 223, 222, 132, 123, 122, 212, 211) ~ "Hibernando",
                                        Score %in% list(111, 112, 121, 131,141,151) ~ "Dormidos",
                                        TRUE ~ "K"))
  }
  
  # customer == FALSE para analisis de productos
  if(customer==FALSE){
    rfm_score_table <- df_rfm %>% 
      mutate(segment = dplyr::case_when(Score %in% list(555, 554, 545, 455, 544, 454, 445, 444) ~ "A",
                                        Score %in% list(553, 535, 355, 543, 534, 453, 435, 354, 345, 443, 434, 344, 533, 353, 335, 433, 343, 334, 333) ~ "B",
                                        Score %in% list(552, 525, 255, 542, 524, 452, 425, 254, 245, 442, 424, 244, 532, 523, 352, 325, 253, 235, 551,
                                                        515, 155, 432, 423, 342, 324, 243, 234, 541, 522, 514, 451, 415, 252, 225, 154, 145, 332, 323,
                                                        233, 441, 422, 414, 242, 224, 144, 531, 513, 351, 315, 153, 135, 431, 413, 341, 322, 314, 232,
                                                        223, 143, 134, 521, 512, 251, 215, 152, 125, 331, 313, 133, 421, 412, 241, 222, 214, 142, 124,
                                                        321, 312, 231, 213, 132, 123, 511, 151, 115, 411, 221, 212, 141, 122, 114, 311, 131, 113, 211,
                                                        121, 112, 111) ~ "C",
                                        TRUE ~ "K"))
  }
  
  return(rfm_score_table)
}





##########################################################
######  CARGUE Y TRANFORMACION DE INFORMACION
###########################

BD_Inicial <- read_excel("C:/Users/Diego Torres/OneDrive/Datasets/Manar/BD_Carvajal.xlsx", 
                         sheet = "Hoja2", col_types = c("numeric", 
                                                        "text", "numeric", "text", "date", 
                                                        "numeric", "numeric", "text"))

#K=BD_Inicial$`nombre producto` %>% table() %>% as.data.frame()
#BD_Inicial=BD_Inicial%>% 
#  filter(BD_Inicial$`nombre producto` %in% K$.[12:31] %>% as.vector())

ultima_fecha = as.Date(max(BD_Inicial$`fecha venta`))
FechaPeriodo1 = ultima_fecha-90
FechaPeriodo2 = ultima_fecha-180

fechas = NULL
fechas$ultima_fecha =ultima_fecha
fechas$FechaPeriodo1 = FechaPeriodo1
fechas$FechaPeriodo2 = FechaPeriodo2
fechas=fechas %>% as.data.frame()


# Se separa la base en dos para que la suma de transacciones por cliente se divida por los periodos
periodo1=BD_Inicial %>% 
  filter(as.Date(`fecha venta`) >= as.Date(FechaPeriodo1))

periodo2=BD_Inicial %>% 
  filter(as.Date(`fecha venta`) < as.Date(FechaPeriodo1) & as.Date(`fecha venta`) >= as.Date(FechaPeriodo2))

periodo1$Rec = as.Date(ultima_fecha)+1-as.Date(periodo1$`fecha venta`)
periodo2$Rec = as.Date(ultima_fecha)+1-as.Date(periodo2$`fecha venta`)


####   RFM CLIENTES
BD_RFM_Periodo1_cliente = periodo1 %>%
  group_by(`codigo cliente`) %>% 
  summarise(Recencia=min(Rec %>% as.numeric()),
            Frecuencia=sum(`cantidad facturado`),
            Monto=sum(`valor facturado`),
            nombre_cliente=unique(`nombre cliente`)) %>%  
  #distinct(`codigo cliente`, .keep_all = TRUE) %>% 
  mutate(Periodo='P1') %>% 
  arrange(`codigo cliente`)


BD_RFM_Periodo2_cliente=periodo2 %>% 
  group_by(`codigo cliente`) %>% 
  summarise(Recencia=min(Rec %>% as.numeric()),
            Frecuencia=sum(`cantidad facturado`),
            Monto=sum(`valor facturado`),
            nombre_cliente=unique(`nombre cliente`)) %>% 
  #distinct(`codigo cliente`, .keep_all = TRUE)%>% 
  mutate(Periodo='P2') %>% 
  arrange(`codigo cliente`)

RFM_SEM1_cliente=SegmentosRFMManar(RFMManar(BD_RFM_Periodo1_cliente),TRUE) 
RFM_SEM2_cliente=SegmentosRFMManar(RFMManar(BD_RFM_Periodo2_cliente),TRUE) 



####    RFM PRODUCTOS
BD_RFM_Periodo1_producto= periodo1 %>%
  group_by(`codigo producto`) %>% 
  summarise(Recencia=min(Rec %>% as.numeric()),
            Frecuencia=sum(`cantidad facturado`),
            Monto=sum(`valor facturado`),
            nombre_producto=unique(`nombre producto`)) %>% 
  distinct(`codigo producto`, .keep_all = TRUE)%>% 
  mutate(Periodo='P1')

BD_RFM_Periodo2_producto=periodo2 %>% 
  group_by(`codigo producto`) %>% 
  summarise(Recencia=min(Rec %>% as.numeric()),
            Frecuencia=sum(`cantidad facturado`),
            Monto=sum(`valor facturado`),
            nombre_producto=unique(`nombre producto`)) %>% 
  distinct(`codigo producto`, .keep_all = TRUE)%>% 
  mutate(Periodo='P2')

RFM_SEM1_producto=SegmentosRFMManar(RFMManar(BD_RFM_Periodo1_producto),FALSE) 
RFM_SEM2_producto=SegmentosRFMManar(RFMManar(BD_RFM_Periodo2_producto),FALSE)


RFM_SEM1_cliente %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/RFM_Tanterior_cliente.csv",sep = ";")
RFM_SEM2_cliente %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/RFM_Tactual_cliente.csv",sep = ";")

RFM_SEM1_producto %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/RFM_Tanterior_producto.csv",sep = ";")
RFM_SEM2_producto %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/RFM_Tactual_producto.csv",sep = ";")

fechas %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/fechas.csv",sep = ";")


