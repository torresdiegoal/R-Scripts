

library(openxlsx)

inpath  <- "C:/Users/Diego Torres/OneDrive/Datasets/" # ruta de carga
outpath  <- "C:/Users/Diego Torres/OneDrive/Datasets/CursoR/" # ruta de salida


datos <- read_excel(paste(inpath,'BD sintomatología emocional.xlsx', sep = "")) 

write.xlsx(datos, paste(outpath,'BD sintomatología emocional.xlsx', sep = ""))