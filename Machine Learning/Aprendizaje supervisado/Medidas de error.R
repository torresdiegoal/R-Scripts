
rm(list = ls())



library(ggplot2)
library(cowplot)
library(randomForest)
library(ROCR)
library(caret)


base_rf <- base_propension # Ejemplo de base de terpel 
base_rf$CLIENTE <- as.factor(base_rf$CLIENTE)

str(train)
ncol(base_rf)


# Split the data into training and test set
set.seed(123)
training.samples <- base_rf$CLIENTE %>% 
  createDataPartition(p = 0.7, list = FALSE)
train  <- base_rf[training.samples, ]
test <- base_rf[-training.samples, ]


# Para que el algoritmo tome 'classificacion' y no 'Regression', el campo dependiente DEBE ser FACTOR
randomFor <- randomForest(CLIENTE ~ ., data=train, 
                          ntree=100,
                          keep.forest=TRUE,
                          proximity=TRUE)

###   Valores posibles de predict
# A) Valor de la prediccion
pred_rf_cl <- predict(object = randomFor, 
                      newdata = test, 
                      type = 'class')   # class y response dan resultados binarios e iguales
pred_rf <- predict(object = randomFor, 
                   newdata = test, 
                   type = 'response')


# B) Si queremos aplicar un punto de corte distinto a partir de la probabilidad pronosticada, 
# # usamos:
# pred_rf <- predict(randomFor, test, type = 'prob')
# # Usamos unicamente la base de testeos
# base_propension_pred <- test %>% 
#   mutate(probabilidad = round(pred_rf,3),
#          pred_rf = ifelse(probabilidad > 0.3, 1, 0),
#          Precision = CLIENTE == pred_rf)



############    MATRIZ DE CONFUSION    #########
conf_table_log <- table(test$CLIENTE, 
                        pred_rf, 
                        dnn = c('Actual','Pred'))

# Usar si se usa un punto de corte distinto
# conf_table_log <- table(base_propension_pred$CLIENTE, 
#                         base_propension_pred$pred_log,
#                         dnn = c('Actual','Pred'))

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
accuracy <- sum(base_propension_pred$Precision) / nrow(base_propension_pred) # way 1
accuracy2 <- (TP+TN) / (TP+TN+FN+FP) # way 2
# B) PRECISION del modelo
precition <- (TP) / (TP+FP) 
# C) RECALL/SENSIBILIDAD del modelo: True Positive Rate (TPR)
recall <- (TP) / (TP+FN)
# D) F1_SCORE del modelo
f1_score <- 2 * (precition*recall) / (precition+recall)
# E) VPN del modelo
vpn <- (TN) / (TN+FN)
# F) ESPECIFICIDAD del modelo: probabilidad de que un sujeto sano tenga un resultado negativo en la prueba.
specificity <- (TN) / (TN+FP)
# G) False Positive Rate (FPR) del modelo: 1 - ESPECIFICIDAD
FPR <- (FP) / (FP+TN)


#Guardamos esta informacion para generar un cuadro comparativo al final
#error_measure <- firstup(c('accuracy','precition','recall','f1_score','vpn','specificity','FPR'))
err_randfor <- round(c(accuracy,precition,recall,f1_score,vpn,specificity,FPR),3)
err_table <- cbind(err_table,err_randfor)