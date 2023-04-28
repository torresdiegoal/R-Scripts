rm(list = ls())


setwd("D:/Disco D/R/R Scripts/Trabajos/netCDF Meteocolombia/netCDF_&_csv/")
###################
##### A) PDF

pdf("plot_pdf.pdf") # Abrimos el dispositivo grafico
plot(rnorm(20)) # Creamos un grafico
dev.off() # Cerramos el dispositivo grafico


###################
##### B) PNG

png("plot_PNG.png")
plot(rnorm(20))
dev.off()


###################
##### C) SVG

svg("plot_SVG.svg")
plot(rnorm(20))
dev.off()


###################
##### D) Postscript

postscript("plot_PS.ps")
plot(rnorm(20))
dev.off()


###################
##### E) JPEG

jpeg("plot_JPEG.jpeg", quality = 75)
plot(rnorm(20))
dev.off()


###################
##### F) TIFF

tiff("plot_tiff.tiff", compression = "zip")
plot(rnorm(20))
dev.off()


###################
##### G) BMP

bmp("plot_BMP")
plot(rnorm(20))
dev.off()