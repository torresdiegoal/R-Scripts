#library(tigerstats) #si se tiene instalado
rm(list = ls())

####################################################
###### TEST PARA MEDIAS VARIANZA POB CONOCIDA ######
####################################################

#prueba
TestMuPoblZ(2400, 240, 2500, 30, 0.05, "below")

TestMuPoblZ <- function(xbar, sigma, mu0, n, alpha, region){
  z <- (xbar-mu0)/(sigma/sqrt(n)) #estadistico
  
  #si se quiere comprobar que el parametro estimado es mayor a alpha
  if(region == "above"){
    pval <- pnorm(z, lower.tail = F)
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
      }
  }
  
  #si se quiere comprobar que el parametro estimado es menor a H0
  if(region == "below"){
    pval <- pnorm(z)
    
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
    }
  }
  
  #si se quiere comprobar que el parametro estimado es diferente de H0
  if(region == "between"){
    pval <- 2*pnorm(abs(z),lower.tail = F)
    
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
    }
  }
}



####################################################
###### TEST PARA MEDIAS VARIANZA POB CONOCIDA ######
####################################################

#prueba
TestMuPoblT(2400, 240, 2500, 30, 0.05, "below")

TestMuPoblT <- function(xbar, S_samp, mu0, n, alpha, region){
  t <- (xbar-mu0)/(S_samp/sqrt(n)) #estadistico
  
  #si se quiere comprobar que el parametro estimado es mayor a alpha
  if(region == "above"){
    pval <- pt(t, df= n-1, lower.tail = F)
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
    }
  }
  
  #si se quiere comprobar que el parametro estimado es menor a H0
  if(region == "below"){
    pval <- pt(t, df= n-1)
    
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
    }
  }
  
  #si se quiere comprobar que el parametro estimado es diferente de H0
  if(region == "between"){
    pval <- 2*pt(abs(t), df= n-1, lower.tail = F)
    
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
    }
  }
}





##############################
##### TEST PARA VARIANZAS ####
##############################

TestVarianza <- function(S, n, alpha, region){
  X <- ((n-1)*S^2)/sigma^2 #estadastico
  
  #si se quiere comprobar que el parametro estimado es mayor a H0
  if(region == "above"){
    pval <- pchisq(X, lower.tail = F)
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
    }
  }
  
  #si se quiere comprobar que el parametro estimado es menor a H0
  if(region == "below"){
    pval <- pchisq(X)
    
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
    }
  }
  
  #si se quiere comprobar que el parametro estimado es diferente de H0
  if(region == "between"){
    pvalI <- pchisq(z, n-1)
    pvalS <- pchisq(z, n-1, lower.tail = F)
    pval <- 2*min(pvalS, pvalI)
    
    if(pval > alpha){
      print(paste("El valor del p-valor es de: ", round(pval,5), " y es mayor a alpha: ", alpha, ". Por lo tanto, no se rechaza H0"))
    }
    else{
      print(paste("El valor del p-valor es de: ", round(pval,5), " y no es mayor a alpha: ", alpha, ". Por lo tanto, se rechaza H0"))
    }
  }
}


#prueba
TestVarianza(56, 200, 0.01, "below")





d <- c(1:15)
d <- c(1:7, 9, 10, 12:15)
c <- c(1065, NA, 1267, 1200, NA, 1370, 1780, NA, 2223, 2473,NA, 2400, 2900, NA, 2500)
data <- cbind(d,c)

model <- lm(c2~d)
model

c2 <- c(1065, 1161, 1267, NA, 1485, NA, 1780, 2400, 2223, 2473, NA, NA, 2900, 3000, NA)

model <- lm(d~c2)
model
