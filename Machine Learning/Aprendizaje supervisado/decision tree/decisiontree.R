#https://www.youtube.com/watch?v=tU3Adlru1Ng&list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1&index=2

rm(list = ls())

#Decision tree with party
library(party)

inpath  <- "D:/Disco D/R/Datos/Machine learning/decision tree/"
outpath <- "D:/Disco D/R/Datos/Machine learning/decision tree/"

knitr::include_graphics(paste(inpath,"Info_datos.png", sep = ""), 
                        auto_pdf = getOption("knitr.graphics.auto_pdf", TRUE))

mydata <- read.csv(paste(inpath,"Cardiotocographic.csv", sep = ""))
mydata$NSPF <- as.factor(mydata$NSP)

#partition data into Training and validation datasets
set.seed(1234)
pd <- sample(2, nrow(mydata), replace = T, prob = c(0.8,0.2))
train <- mydata[pd ==1,]
validate <- mydata[pd==2,]

mytree <- ctree(NSPF~LB+AC+FM, mydata, 
                controls = ctree_control(mincriterion=0.9, minsplit=50))
print(mytree)
plot(mytree, type="simple")

predict(mytree, validate, type = "prob") #nos arroja la prob que tiene cada individ de presentar cada una de los factores elegidos
predict(mytree, validate) # nos da unicamente a qué factor pertenece cada individuo dependiendo de quién tenga mayor prob asociada.

#Misclassification error
tab<-table(predict(mytree), mydata$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab) # error de clasificación

#########################
###### rpart package ####
########################

library(rpart)
library(rpart.plot)
tree1 <- rpart(NSPF~LB+AC+FM, train)
rpart.plot(tree1, extra = 4) #extra = 4 presenta la probabilidad de pertenecer a cada factor segun el nodo

predict(tree1, validate)