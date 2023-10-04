library(readxl)
library(dplyr)

RFMManar = function(BD){
  
  rfm_df=BD
  LRecencia=Limites(rfm_df$Recencia)
  LMonto=Limites(rfm_df$Monto)
  LFrecuencia=Limites(rfm_df$Frecuencia)
  
  rfm_df$ScoreRecencia=categorizacion(rfm_df$Recencia,LRecencia,reciencia=TRUE)
  rfm_df$ScoreFrecuencia=categorizacion(rfm_df$Frecuencia,LFrecuencia,reciencia=FALSE)
  rfm_df$ScoreMonto=categorizacion(rfm_df$Monto,LMonto,reciencia=FALSE)
  
  rfm_df$Score=NULL
  rfm_df$Score=as.numeric(paste(rfm_df$ScoreRecencia,rfm_df$ScoreFrecuencia,rfm_df$ScoreMonto,sep=""))
  
  return(rfm_df)
}

#Se calculan los limites para identificación de posibles datos atipicos
#Q1–1.5·RIC o superiores a Q3+1.5·RIC.
Limites=function(variable){
  LI=quantile(variable,0.25)-(1.5*(quantile(variable,0.75)-quantile(variable,0.25)))
  LS=quantile(variable,0.75)+(1.5*(quantile(variable,0.75)-quantile(variable,0.25)))
  return(c(LI,LS))
}

#Creamos variables temporales donde excluimos los registros que están por fuera de los limites
categorizacion=function(variable,limites,reciencia){
  LI=limites[1]
  LS=limites[2]
  VarTemp=variable[variable>LI & variable<LS]
  breaks_1=c(min(variable)-1,quantile(VarTemp,probs = seq(0,1,0.2))[2:5],max(variable)+1)
  breaks_2=quantile(variable,probs = seq(0,1,0.2))
  breaks_3=seq(min(variable),max(variable),length.out= 5)
  
  if(reciencia==FALSE){
    if(sum(duplicated(breaks_1))==0){
      categorias=cut(variable, breaks=breaks_1,labels = 1:5) %>% as.numeric()
    }
    if(sum(duplicated(breaks_1))!=0 & sum(duplicated(breaks_2))==0){
      categorias=cut(variable, breaks=breaks_2,labels = 1:5) %>% as.numeric()
    }
    if(sum(duplicated(breaks_1))!=0 & sum(duplicated(breaks_2))!=0){
      categorias=cut(variable, breaks=breaks_3,labels = 1:5) %>% as.numeric()
    }
  }
  if(reciencia==TRUE){
    if(sum(duplicated(breaks_1))==0){
      categorias=((cut(variable, breaks=breaks_1,labels = 1:5) %>% as.numeric())-6)*(-1)
    }
    if(sum(duplicated(breaks_1))!=0 & sum(duplicated(breaks_2))==0){
      categorias=((cut(variable, breaks=breaks_2,labels = 1:5) %>% as.numeric())-6)*(-1)
    }
    if(sum(duplicated(breaks_1))!=0 & sum(duplicated(breaks_2))!=0){
      categorias=((cut(variable, breaks=breaks_3,labels = 1:4) %>% as.numeric())-6)*(-1)
    }
  }
  
  return(categorias)
}

SegmentosRFMManar = function(df_rfm,customer){
  # df_rfm=rfm_df
  
  if(customer==TRUE){
    rfm_score_table <- df_rfm %>% 
      mutate(segment = case_when(df_rfm$Score %in% list(555, 554, 544, 545, 454, 455, 445) ~ "Campeones",
                                 df_rfm$Score %in% list(543, 444, 435, 355, 354, 345, 344, 335) ~ "Leales",
                                 df_rfm$Score %in% list(553, 551,552, 541, 542, 533, 532, 531, 452, 451, 442, 441, 
                                                        431, 453, 433, 432, 423, 353, 352, 351, 342, 341, 333, 323) ~ "Leales en potencia",
                                 df_rfm$Score %in% list(512,511, 422, 421, 412, 411, 311) ~ "Clientes recientes",
                                 df_rfm$Score %in% list(525, 524, 523, 522, 521, 515, 514, 513, 425, 424, 413,414, 415, 
                                                        315, 314, 313) ~ "Prometedores",
                                 df_rfm$Score %in% list(535, 534, 443, 434, 343, 334, 325, 324) ~ "Necesitan atención",
                                 df_rfm$Score %in% list(331, 321, 312, 221, 213) ~ "A punto de dormir",
                                 df_rfm$Score %in% list(255, 254, 245, 244, 253, 252, 243, 242, 235, 234, 225, 224, 
                                                        153, 152, 145, 143, 142, 135, 134, 133, 125, 124) ~ "En riesgo",
                                 df_rfm$Score %in% list(155, 154, 144, 214,215,115, 114, 113) ~ "No puedes perderlos",
                                 df_rfm$Score %in% list(332, 322, 231, 241, 251, 233, 232, 223, 222, 132, 123, 122, 212, 211) ~ "Hibernando",
                                 df_rfm$Score %in% list(111, 112, 121, 131,141,151) ~ "Dormidos"))
  }
  
  if(customer==FALSE){
    rfm_score_table <- df_rfm %>% 
      mutate(segment = case_when(df_rfm$Score %in% list(555, 554, 545, 455, 544, 454, 445, 444) ~ "A",
                                 df_rfm$Score %in% list(553, 535, 355, 543, 534, 453, 435, 354, 345, 443, 434, 344, 533, 353, 335, 433, 343, 334, 333) ~ "B",
                                 df_rfm$Score %in% list(552, 525, 255, 542, 524, 452, 425, 254, 245, 442, 424, 244, 532, 523, 352, 325, 253, 235, 551,
                                                        515, 155, 432, 423, 342, 324, 243, 234, 541, 522, 514, 451, 415, 252, 225, 154, 145, 332, 323,
                                                        233, 441, 422, 414, 242, 224, 144, 531, 513, 351, 315, 153, 135, 431, 413, 341, 322, 314, 232,
                                                        223, 143, 134, 521, 512, 251, 215, 152, 125, 331, 313, 133, 421, 412, 241, 222, 214, 142, 124,
                                                        321, 312, 231, 213, 132, 123, 511, 151, 115, 411, 221, 212, 141, 122, 114, 311, 131, 113, 211,
                                                        121, 112, 111) ~ "C"))
  }
  
  return(rfm_score_table)
}

# BD_Inicial <- read_excel("C:/Users/ZailethSaekaToncelVa/Downloads/BD_Carvajal1.xlsx", 
#                          sheet = "Hoja3", col_types = c("numeric", 
#                                                         "text", "text", "text", "date", "numeric", 
#                                                         "numeric","text"))

BD_Inicial <- read_excel("C:/Users/Diego Torres/Downloads/BD_Carvajal1.xlsx", 
                         sheet = "Hoja3", col_types = c("numeric", 
                                                        "text", "text", "text", "date", "numeric", 
                                                        "numeric","text"))
BD_Inicial_orig <- BD_Inicial

K=BD_Inicial$`nombre producto` %>% table() %>% as.data.frame() 
  #%>% arrange(desc(Freq))
K
BD_Inicial=BD_Inicial%>% 
  filter(BD_Inicial$`nombre producto` %in% K$.[12:31] %>% as.vector())

ultima_fecha=as.Date(max(BD_Inicial$`fecha venta`))
seis_meses_atras=ultima_fecha-180
doce_meses_atras=ultima_fecha-365

fechas = NULL
fechas$ultima_fecha =ultima_fecha
fechas$seis_meses_atras =seis_meses_atras
fechas$doce_meses_atras =doce_meses_atras
fechas=fechas %>% as.data.frame()


Semestre1=BD_Inicial %>% 
  filter(as.Date(`fecha venta`)>=as.Date(seis_meses_atras)) 

Semestre2=BD_Inicial %>% 
  filter(as.Date(`fecha venta`)< as.Date(seis_meses_atras) & as.Date(`fecha venta`) >= as.Date(doce_meses_atras))

Semestre1$Rec=as.Date(ultima_fecha)+1-as.Date(Semestre1$`fecha venta`)
Semestre2$Rec=as.Date(ultima_fecha)+1-as.Date(Semestre2$`fecha venta`)

#RFM CLIENTES
BD_RFM_semestre1_cliente= Semestre1 %>%
  group_by(`codigo cliente`) %>% 
  summarise(Recencia=min(Rec %>% as.numeric()),
            Frecuencia=sum(`cantidad facturado`),
            Monto=sum(`valor facturado`),
            nombre_cliente=unique(`nombre cliente`)) %>% 
  distinct(`codigo cliente`, .keep_all = TRUE)

BD_RFM_semestre2_cliente=Semestre2 %>% 
  group_by(`codigo cliente`) %>% 
  summarise(Recencia=min(Rec %>% as.numeric()),
            Frecuencia=sum(`cantidad facturado`),
            Monto=sum(`valor facturado`),
            nombre_cliente=unique(`nombre cliente`)) %>% 
  distinct(`codigo cliente`, .keep_all = TRUE)

RFM_SEM1_cliente=SegmentosRFMManar(RFMManar(BD_RFM_semestre1_cliente),TRUE) 
RFM_SEM2_cliente=SegmentosRFMManar(RFMManar(BD_RFM_semestre2_cliente),TRUE) 

#RFM PRODUCTOSD
BD_RFM_semestre1_producto= Semestre1 %>%
  group_by(`codigo producto`) %>% 
  summarise(Recencia=min(Rec %>% as.numeric()),
            Frecuencia=sum(`cantidad facturado`),
            Monto=sum(`valor facturado`),
            nombre_producto=unique(`nombre producto`)) %>% 
  distinct(`codigo producto`, .keep_all = TRUE)

BD_RFM_semestre2_producto=Semestre2 %>% 
  group_by(`codigo producto`) %>% 
  summarise(Recencia=min(Rec %>% as.numeric()),
            Frecuencia=sum(`cantidad facturado`),
            Monto=sum(`valor facturado`),
            nombre_producto=unique(`nombre producto`)) %>% 
  distinct(`codigo producto`, .keep_all = TRUE)

RFM_SEM1_producto=SegmentosRFMManar(RFMManar(BD_RFM_semestre1_producto),FALSE) 
RFM_SEM2_producto=SegmentosRFMManar(RFMManar(BD_RFM_semestre2_producto),FALSE)


RFM_SEM1 %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/RFM_Tanterior_cliente.csv",sep = ";")
RFM_SEM2 %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/RFM_Tactual_cliente.csv",sep = ";")

RFM_SEM1 %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/RFM_Tanterior_producto.csv",sep = ";")
RFM_SEM2 %>% write.csv2("C:/Users/ZailethSaekaToncelVa/Downloads/RFM_Tactual_producto.csv",sep = ";")

