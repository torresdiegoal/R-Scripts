
#your function here
rm(list = ls())

ptm <- proc.time()


##################
## PAQUETES
#
libraryFold <- .libPaths()[1]

#install.packages("vctrs")
#update.packages("vctrs")
suppressWarnings(suppressMessages(library(vctrs, lib=libraryFold)))
suppressWarnings(suppressMessages(library(tidyr, lib=libraryFold)))
#suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(pROC, lib=libraryFold)))
suppressWarnings(suppressMessages(library(dplyr, lib=libraryFold)))
suppressWarnings(suppressMessages(library(plyr, lib=libraryFold)))
suppressWarnings(suppressMessages(library(caret, lib=libraryFold))) # para validacion cruzada createDataPartition
library(caret) # para validacion cruzada createDataPartition
suppressWarnings(suppressMessages(library(FactoMineR, lib=libraryFold))) # PCA
suppressWarnings(suppressMessages(library(factoextra, lib=libraryFold)))# visualizacion PCA
#suppressWarnings(suppressMessages(library(corrplot, lib=libraryFold)))

# save.image(file = paste(outpath,'Propension.RData', sep = ''))


# Functions
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1)) # COnvierte cada primera letra en mayuscula
  x
}



#################################################
######      I) CARGA y TRANSFORMACION     #####
#################################################

# Sistema de carpetas
# inpath_seg  <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de PropensiÃ³n/04_RealizaciÃ³n/Segmentacion/"
# inpath_imp  <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de PropensiÃ³n/04_RealizaciÃ³n/Impactos/"
# outpath  <- "C:/Users/Diego Torres/OneDrive - Manar Technologies SAS/Modelo de PropensiÃ³n/04_RealizaciÃ³n/Propension/"

inpath  <- "C:/QlikSense_Target/Comercio Exterior/90_Propension/TableFiles/Import/"
outpath  <- "C:/QlikSense_Target/Comercio Exterior/90_Propension/TableFiles/Import/"

# Tablas del Sprint_1
Segmentacion <- read.csv(paste(inpath,"SISMAR_pivot_scaled.csv", sep = ""),
                         #Segmentacion <- read.csv(paste(inpath_seg,"Segmentacion_pivot.csv", sep = ""),
                         sep = '|', dec = ',')
names(Segmentacion)[1] <- 'NIT_EMP_COL'
#dplyr::rename(Segmentacion, NIT_EMP_COL = PUERTO_NIT_EMP_COL)

# Tablas del Sprint_2
Impactos <- read.csv(paste(inpath,"Impactos macroeconomicos original.csv", sep = ""),
                     sep = '|', dec = ',')
Cliente_Act <- read.csv(paste(inpath,"cliente_act.csv", sep = ""),
                        sep = '|', dec = ',')

# Quitar la ultima columna del df
Cliente_Act <- Cliente_Act[, 1:ncol(Cliente_Act)-1]

## Nota importante:
## Los impactos no es necesario estandarizar debido a que ya vienen en un formato de 0 a 1

#Unimos las tres tablas, Impactos filtra los clientes de las otras
base_propension <- merge(Segmentacion, Impactos, by='NIT_EMP_COL', all.x = FALSE)
base_propension <- merge(base_propension, Cliente_Act, by='NIT_EMP_COL', all.x = TRUE)
#names(base_propension)[ncol(base_propension)] <- 'CLIENTE'

rownames(base_propension) <- base_propension$NIT_EMP_COL

base_propension <- base_propension %>%
  dplyr::rename(CLIENTE = CLIENTE_ACT) %>% 
  dplyr::select(-c(NIT_EMP_COL)) #%>% 
#mutate(CLIENTE = as.factor(CLIENTE))


base_propension_names <- data.frame(number = 1:ncol(base_propension),
                                    colname = colnames(base_propension))

#str(base_propension)



# Grafico de correlaciones
# is.corr = FALSE,
# base_propension2 <- base_propension # Tabla que tendra como nombres su posicion
# colnames(base_propension2) <- base_propension_names[,1]
# 
# testRes = cor.mtest(base_propension2, conf.level = 0.95)
# #par(mar=c(0, 0, 0, 0))
# pdf(paste(outpath,"corrplot.pdf", sep = ''), width = 12, height = 12)
# #corrplot(cor(base_propension2), method = 'square', order = 'FPC', type = 'lower', diag = FALSE)
# corrplot(cor(base_propension2), method = 'square', diag = FALSE, order = 'hclust',
#          addrect = 3, rect.col = 'blue', rect.lwd = 3, tl.pos = 'd',    
#          ## specialized the insignificant value according to the significant level
#          p.mat = testRes$p, sig.level = 0.10
#          ) 
# #corrplot(cor(base_propension2), method = 'square')
# dev.off()

##########################################################################################
## Conclusion: Se evidencia alta correlacion entre los campos, hay MULTICOLINEALIDAD
##########################################################################################


# variable de simplificacion
last_col <- ncol(base_propension)

#####################################################
## II) CREACION REGRESION LOGISTICA BASE DIVIDIDA
#

###################################   PCA   ################################################
# Aplicamos el metodo de Analisis de Componentes Principales (CA)

res.PCA = PCA(base_propension[,c(1:last_col-1)], 
              scale.unit = FALSE,
              ncp = 10,
              graph = FALSE)

#base_propension_PCA$var


# Creamos la nueva base con las componentes resultantes del PCA y le anexamos el campo a predecir> CLIENTE
base_propension_PCA <-data.frame(res.PCA$ind$coord, 
                                 CLIENTE = base_propension$CLIENTE) #%>% 
#dplyr::rename(CLIENTE = train.CLIENTE)

# U <- res.PCA$svd$U
# V <- res.PCA$svd$V


# Split the data into training and test set
set.seed(315)
base_propension_PCA$CLIENTE <- as.factor(base_propension_PCA$CLIENTE)
training.samples <- base_propension_PCA$CLIENTE %>% 
  createDataPartition(p = 0.8, list = FALSE)
train  <- base_propension_PCA[training.samples, ]
test <- base_propension_PCA[-training.samples, -last_col]
# Otra forma
# index<-sample(1:nrow(data), round(0.75*nrow(data)))
# train<-data[index,]
# test<-data[-index,]


# Descriptivos
#fviz_eig(res.PCA, addlabels = TRUE)
PCA_variability <- as.data.frame(get_eig(res.PCA)) %>% 
  filter(cumulative.variance.percent <= 85 )
#fviz_eig(res.PCA)


#str(training.samples)
#############################################################################################



# data(Sonar)
###################################
##       REGRESION LOGISTICA 
#

logis <- glm(CLIENTE ~ ., data=train, family="binomial")
summary(logis)

#####    ROC threshold     ####
roc_logis <- roc(response = train$CLIENTE,
                 predictor = logis$fitted.values, # Grafica de la Regresion logistica
                 percent = TRUE)
SensEspec <- roc_logis$sensitivities  + roc_logis$specificities
maximo    <- max(SensEspec)
numOrdenCutoff <- which(SensEspec == maximo)
# tendra el punto de corte donde se maximice la suma de la especificidad y la sensibilidad
logis_threshold <- round(roc_logis$thresholds[numOrdenCutoff],3)


pred_log <- predict(object = logis, 
                    #newdata = test, 
                    newdata = base_propension_PCA,
                    type = 'response' # unico valor posible para regression
)

#caret::R2(pred_rf,test$CLIENTE)

# MSE.lm <- sum((pred_log-test$medv)^2)/nrow(test)
# 
# ## Now calculate the overall "Pseudo R-squared" and its p-value
# ll.null <- logis$null.deviance/-2
# ll.proposed <- logis$deviance/-2
# 
# ## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
# (ll.null - ll.proposed) / ll.null
# 
# ## The low p-value for the R^2: confirma que los resultados no son producto del azar
# 1 - pchisq(2*(ll.proposed - ll.null), df=(length(logis$coefficients)-1))


#pred_log_prob <- predict(logis, test, type = 'response')
# Usamos unicamente la base de testeos
#base_propension_pred <- test %>%
base_propension_logis <- base_propension_PCA %>%
  mutate(CLIENTE = ifelse(CLIENTE == '1', 1, 0),
         probabilidad = round(pred_log,3),
         pred_log = ifelse(probabilidad > logis_threshold, 1, 0), 
         Precision = CLIENTE == pred_log)


###    MATRIZ DE CONFUSION    ###
# conf_table_log <- table(test$CLIENTE, 
#                         pred_log, 
#                         dnn = c('Actual','Pred'))
conf_table_log <- table(base_propension_logis$CLIENTE, 
                        base_propension_logis$pred_log,
                        dnn = c('Actual','Pred'))
conf_table_log

# Valores de la matriz
TN <- conf_table_log[1,1]
FP <- conf_table_log[1,2]
FN <- conf_table_log[2,1]
TP <- conf_table_log[2,2]


###############################################
####     MEDIDAS DE PRECISION
####
#######################################################
# A) ACCURACY/EXACTITUD del modelo
#accuracy <- sum(base_propension_logis$Precision) / nrow(base_propension_logis) # way 1
accuracy <- (TP+TN) / (TP+TN+FN+FP) # way 2
# B) PRECISION del modelo
precition <- (TP) / (TP+FP) 
# C) RECALL/SENSIBILIDAD del modelo: True Positive Rate (TPR), probabilidad de que un + se clasifique como +
recall <- (TP) / (TP+FN)
# D) F1_SCORE del modelo
f1_score <- 2 * (precition*recall) / (precition+recall)
# E) VPN del modelo
vpn <- (TN) / (TN+FN)
# F) ESPECIFICIDAD del modelo: 
specificity <- (TN) / (TN+FP)
# G) False Positive Rate (FPR) del modelo: (1 - ESPECIFICIDAD), probabilidad de que un - se clasifique como +
FPR <- (FP) / (FP+TN)


#Guardamos esta informacion para generar un cuadro comparativo al final
error_measure <- firstup(c('accuracy','precition','recall','f1_score','vpn','specificity','FPR')) # funcion creada
err_logis <- round(c(accuracy,precition,recall,f1_score,vpn,specificity,FPR),3)
err_table <- cbind(error_measure,err_logis)


str(base_propension_logis)

# Obtenemos la base final de clientes a los que se les deberia hacer una gestion comercial mayor
# Primero la base con los FP, se genera el campo Grado_probabilidad: alto, medio, bajo
FP_logis <- base_propension_logis %>% 
  #filter(pred_log != CLIENTE)
  filter(pred_log > CLIENTE) %>% 
  arrange(desc(probabilidad) ) %>% 
  mutate(Grado_probabilidad = ifelse(probabilidad > median(probabilidad),'Alta', 'Media'))

# luego la base con los VN, asi obtenemos a todos los que en la realidad no son clientes
VN_logis <- base_propension_logis %>% 
  #filter(pred_log != CLIENTE)
  filter(pred_log == 0 & CLIENTE == 0) %>% 
  arrange(desc(probabilidad) ) %>% 
  mutate(Grado_probabilidad = 'Bajo')

# Concatenamos las tablas
clientes_propension_logis <- rbind(FP_logis,VN_logis)
clientes_propension_logis <- clientes_propension_logis %>% 
  dplyr::select(probabilidad,Grado_probabilidad) %>% 
  mutate(NIT_EMP_COL = rownames(.)) %>% 
  dplyr::select(NIT_EMP_COL,probabilidad,Grado_probabilidad)  %>% #reorganizamos
  arrange(desc(probabilidad) )


write.table(clientes_propension_logis,
            file = paste(outpath, 'Clientes_probabilidad.csv',
                         sep = ''),
            fileEncoding = 'UTF-8',
            row.names = F,
            quote = F, #omite las comillas de cada elemento del output
            sep = '|',
            dec = ',',
            na = '-')

# # Traemos los nombres del SPRINT 2
# SISMAR_companies <- read.csv(paste(inpath_imp,"SISMAR_companies.csv", sep = ""),
#                      sep = '|', dec = ',')
# 
# 
# 
# Sprint3 <- merge(FP_logis,SISMAR_companies,by='NIT_EMP_COL') %>% 
#   arrange(desc(probabilidad) )


## Grafico de los resultados
# predicted.data <- data.frame(
#   probability.of.client=logis$fitted.values,
#   Cliente=train$CLIENTE)
# 
# predicted.data <- predicted.data[
#   order(predicted.data$probability.of.client, decreasing=FALSE),]
# predicted.data$rank <- 1:nrow(predicted.data)
# 
# ## Lastly, we can plot the predicted probabilities for each sample having
# ## heart disease and color by whether or not they actually had heart disease
# ggplot(data=predicted.data, aes(x=rank, y=probability.of.client)) +
#   geom_point(aes(color=Cliente), alpha=1, shape=1, stroke=1) +
#   geom_hline(yintercept = logis_threshold, color="salmon", size = 0.8, lty=2) + 
#   geom_text(aes(0,logis_threshold,label = logis_threshold, vjust = -1), color="salmon") +
#   xlab("Index") +
#   ylab("Probabilidad de ser cliente del puerto")
# 
# ggsave(paste(outpath,"probabilidad_ser_cliente_PuertoBQlla_scaled.pdf", sep=''))


# Guardamos proyecto
save.image(file = paste(outpath,'Propension_30_Propension.RData', sep = ''))


#conf_table_log



print('Propension_30_Propension DONE')
time  <- proc.time() - ptm
print(time)