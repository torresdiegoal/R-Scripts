rm(list=ls())

library(readxl)
library(dplyr)
library(scales)

inpath <- 'C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Chatbot/Proyectos/Elena/DatosExtraccionQlik/DatosCapaG/2022/Enero/xlsx/'
outpath <- 'C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Chatbot/Proyectos/Elena/DatosExtraccionQlik/DatosCapaG/2022/Enero/Files/'
month <- 'Enero'
year<- '2022'
trim <- 'primer'

files <- dir(inpath)

for(i in files){
  data <- read_excel(paste(inpath,i, sep = ''), na = '-')
  file_name <- substr(i, 1,nchar(i)-5)
  for(j in colnames(data)){
    if(class(data[[j]]) == 'numeric'){
      if(grepl('variacion|porcentaje', names(data[j]))){
        data[[j]] <- label_percent(accuracy = 1, big.mark = '.')(data[[j]])
      }
      else if(grepl('margen', names(data[j]))){
        data[[j]] <- label_percent(accuracy = 0.1, big.mark = '.')(data[[j]])
      }
      else if(grepl('contribucion_volkg', names(data[j]))){
        data[[j]] <- label_comma(accuracy = 0.01, big.mark = '.', decimal.mark = ',')(data[[j]])
      }
      else{
        data[[j]] <- label_comma(accuracy = 1, big.mark = '.', decimal.mark = ',')(data[[j]])
      }
    }
  }
  # Unicamente write.table permite personalizar sep y dec.
  write.table(data, file = paste(outpath, file_name, '.csv', sep = ''), 
              fileEncoding = 'UTF-8',
              row.names = F,
              quote = F, #omite las comillas de cada elemento del output
              sep = ';',
              dec = ',',
              na = '-')
}

datos <- c('datos', NA, month, year)
mes <- c('mes', month, NA, NA)
trimestre <- c('trimestre', trim, NA, NA)

Informacion <- as.data.frame(rbind(datos, mes, trimestre))
colnames(Informacion)<- c('info', 'valor', 'mes', 'anio')
Informacion
write.table(Informacion, file = paste(outpath, 'Informacion.csv', sep = ''), 
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = ';',
            dec = ',',
            na = '-')