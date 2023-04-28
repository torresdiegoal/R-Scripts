rm(list = ls())

library(openair)
library(readxl)

# R no exporta tan chevere los datos como Python, sobre todo string y NAS

inpath <- "D:/OneDrive - Universidad de Los Andes/R scripts/R Basico/Datos/"
dir_vien <- read_excel(paste(inpath,"amazon_dir_vien.xlsx", sep = ""))
vel_vien <- read_excel(paste(inpath,"amazon_vel_vien.xlsx", sep = ""))


dir_vien1 <- subset(dir_vien, select = c(CodigoEstacion, Fecha, Dir))
vel_vien1 <- subset(vel_vien, select = c(CodigoEstacion, Fecha, Vel))

datos <- merge(dir_vien1, vel_vien1, by = c("Fecha", "CodigoEstacion"))

write.csv(datos, file = paste(inpath, "vel_dir_amazon.csv", sep=""))
