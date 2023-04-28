rm(list = ls())

# Importa datos csv SIN una extension .csv, es un Archivo #
datos <- read.csv("D:/OneDrive - Universidad de Los Andes/R scripts/Libertadores/Regresiones/Datos/suicidios_all")
attach(datos)                                                                                            

csv <- datos
datos$suicides_prom_anual

library(lmtest)
library(tseries)
setwd("C:/Users/Diego/Desktop/proyecto regresiones/Modelos_t3")

nombres<- c("poblacion_prom_anual", "Happ_PromScore", "Eco_GDPromppcapital", "prom_family", "Health_LifeExpectancy", "Generosity", "Corruption", "DystopiaResidual")

write.table(t(combn(nombres,1)),"1.txt",sep="+",quote=F,col.names = F, row.names = F)
write.table(t(combn(nombres,2)),"2.txt",sep="+",quote=F,col.names = F, row.names = F)
write.table(t(combn(nombres,3)),"3.txt",sep="+",quote=F,col.names = F, row.names = F)
write.table(t(combn(nombres,4)),"4.txt",sep="+",quote=F,col.names = F, row.names = F)
write.table(t(combn(nombres,5)),"5.txt",sep="+",quote=F,col.names = F, row.names = F)
write.table(t(combn(nombres,6)),"6.txt",sep="+",quote=F,col.names = F, row.names = F)
write.table(t(combn(nombres,7)),"7.txt",sep="+",quote=F,col.names = F, row.names = F)
write.table(t(combn(nombres,8)),"8.txt",sep="+",quote=F,col.names = F, row.names = F)


modelos<-read.table("modelos.txt")

ajustar<-function(modelo){
  model<-eval(parse(text=paste("suicides_prom_anual~",modelo)))
  tryCatch({
    ajuste<-lm(model,data=datos)
  }, error=function(e){})
  if(max(coeftest(ajuste)[,4])<0.05 &
     resettest(ajuste,power=2,type="regressor")$p.value>0.1 &
     jarque.bera.test(resid(ajuste))$p.value>0.05 &
     bptest(ajuste)$p.value>0.05 &
     dwtest(ajuste)$p.value>0.05 &
     summary(ajuste)$r.squared >0.6)
  {
    model<-list()
    model$numero<-paste(Sys.time())
    model$Estructura_modelo<-summary(ajuste)
    model$modelo<-eval(parse(text=paste("suicides_prom_anual~",modelo)))
    capture.output(model,file=paste0("modelo",i,".txt"))
    print("Encontre uno :D")
  } 
}

for(i in 1:nrow(modelos)){
  ajustar(modelos[i,])
}
apply(modelos,1, ajustar)


attach(datost3)

bstmodelo=lm(y ~ x2 + x5 + x8 + x9)
summary(bstmodelo)

## Validación del modelo
par(mfrow=c(2,2))
plot(bstmodelo)


residstand=rstandard(bstmodelo)
residstud=rstudent(bstmodelo)
valores_ajustados <- fitted(bstmodelo)
par(mfrow=c(1,1))
plot(valores_ajustados,residstand)


library(ggplot2)
ggplot(data = datost3, aes(x = valores_ajustados, y = residstand)) +
  geom_point() +
  geom_smooth(color = "firebrick") +
  geom_hline(yintercept = 0) +
  ggtitle(" Residuos Estandarizados ")+
  theme_bw()



ggplot(data = datost3, aes(x = valores_ajustados, y = residstud)) +
  geom_point() +
  geom_smooth(color = "firebrick") +
  geom_hline(yintercept = 0) +
  ggtitle(" Residuos Studentizados ")+
  theme_bw()

summary(influence.measures(bstmodelo))
