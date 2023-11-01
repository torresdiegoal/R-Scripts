rm(list = ls())

librery(dplyr)
library(caret)


inpath  <- 'C:/Users/Diego Torres/OneDrive/Datasets/'
outpath  <- 'C:/Users/Diego Torres/OneDrive/Datasets/'


base_propension_PCA <- read.csv(paste(inpath,"Base_ROC.csv", sep = ""),
                         sep = ',', dec = '.')


# Split the data into training and test set
set.seed(315)
base_propension_PCA$CLIENTE <- as.factor(base_propension_PCA$CLIENTE)
training.samples <- base_propension_PCA$CLIENTE %>% 
  createDataPartition(p = 0.8, list = FALSE)
train  <- base_propension_PCA[training.samples, ]
test <- base_propension_PCA[-training.samples, -last_col]

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
