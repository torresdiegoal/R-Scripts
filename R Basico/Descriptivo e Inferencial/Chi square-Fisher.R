rm(list = ls())

library(tigerstats)

###################################################################### ###########
## FUNCIONES PARA OBTENER INTERVALOS DE CONFIANZA DE DISTINTAS DISTRIBUCIONES ###
#################################################################################


# A) 
###################################################################### 
###### Intervalos de confianza para estimar u en distr. normal #######

xbar <- 3.9  # media muestral
sigma <- 1.03 # sd poblacional
n <- 102
alpha <- 0.1
porcen <- (1-alpha)*100

LIN <- round(xbar - (qnorm(alpha/2, lower.tail = F) * (sigma/sqrt(n))), digits = 2)
LSN <- round(xbar + (qnorm(alpha/2, lower.tail = F) * (sigma/sqrt(n))),digits = 2)
paste("El intervalo de confianza para u del ", porcen, "% está entre: ",LI," y ", LS, sep = "")


# B) 
###################################################################
###### Intervalos de confianza para estimar proporciones #########

n <- 420
p <- 123/n
alpha <- 0.9
ICI = round((p - (qnorm(alpha)*(sqrt((p*(1-p)/n))))), 2)
ICS = round((p + (qnorm(alpha)*(sqrt((p*(1-p)/n))))), 2)

print(paste("El intervalo de confianza para alpha:", alpha," se encuentra entre", ICI, "y", ICS))


# C) 
##########################################################################
##### Intervalos de confianza para estimar u en distr. T Student #######

weight <- c(80, 65, 80,60, 63, 95, 56, 49, 62, 53, 78, 66)
stdw <- sd(weight)
meanw <- mean(weight)
alphat <- 0.1
porcent <- (1-alphat)*100
m <- 12

LIT <- round(meanw - (qt(alphat/2, m-1, lower.tail = F) * (stdw/sqrt(m))), digits = 2) #lim inferior
LST <- round(meanw + (qt(alphat/2, m-1, lower.tail = F) * (stdw/sqrt(m))),digits = 2) #lim superior
paste("El intervalo de confianza para u del ", porcent, "% está entre: ",LIT," y ", LST, sep = "")


# D)
#############################################################################
###### Intervalos de confianza para estimar u en distr. Chi Cuadrado #######

seven <- c(35,44,38,55,33,56,67,45,48,40)
X <- mean(seven)
std <- sd(seven)
alpha <- .1
porcent <- (1-alpha)*100
m <- 10

LIX <- round(sqrt(((m-1)*(std)^2)/qchisq(alpha/2, m-1, lower.tail = F)), digits = 2) #lim inferior
LSX <- round(sqrt(((m-1)*(std)^2)/qchisq(alpha/2, m-1, lower.tail = T)), digits = 2) #lim superior
paste("El intervalo de confianza para ",expression(sigma)," a ", porcent, "% de confianza está entre: ",LIX," y ", LSX, sep = "")


# E) 
#####################################
## Esperanza y varianza en Fisher ##

E_var <- function(df1, df2){
  if (df2  > 2 ){
    
    E <- df2/(df2-2)   #Esperanza en Fisher
    print(paste("u: ", round(E, 3)))
    
    if(df2 > 4){
      sigmasq <- ((2*(df2)^2)*(df1+df2-2))/(df1*(df2-2)^2*(df2-4)) # Var en Fisher
      print(paste("sigma^2: ", round(sigmasq, 3)))
    }
    else{
      print("El valor de n no es mayor a 4, no es posible calcular la varianza")
    }
    
  }
  else {
    print("El valor de n no es mayor a 2, no es posible calcular ni la esperanza ni la varianza")
  }
}


tExplore() #plotea y juega con grados delibertad de la t student

curve(dchisq(x,df=3), col= "red", xlim= c(0,60), xlab = expression(chi^2))
curve(dchisq(x,df=7), col= "blue", xlim= c(0,60), add = T)
curve(dchisq(x,df=15), col= "green", xlim= c(0,60), add = T)
curve(dchisq(x,df=30), col= "orange", xlim= c(0,60), add = T)


qf(.95, 9, 7, lower.tail = T)


curve(df(x,df1=9, 7), col= "red", xlim= c(0,20), xlab = expression(chi^2))


?df  # QUE ES LA DISTRIBUCION F? ADEMAS, funcion de densidad F
qf(.9, df1 = 5, df2= 7) #Quartil asociado a distribucion F

t.test(weight, conf.level = 0.9) #da intervalo de confianza, media y t de hipotesis

curve(df(x, df1 = 5, df2= 7), col= "red", xlim= c(0,15), xlab = "F")

pbinom(49, 2000, 0.02)
