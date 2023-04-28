rm(list = ls())

# Punto 2. Esperanza y varianza en Fisher
E_var <- function(df1, df2){
  if (df2  > 2 ){
    
    E <- df2/(df2-2)   #Esperanza en Fisher
    print(paste("u: ", round(E, 3)))
    
    if(df2 > 4){
      sigmasq <- ((2*(df2)^2)*(df1+df2-2))/(df1*(df2-2)^2*(df2-4))
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


# Punto 7.a. Intervalos de confianza para estimar u en distr. Chi Cuadrado 
seven <- c(35,44,38,55,33,56,67,45,48,40)
X <- mean(seven)
std <- sd(seven)
alpha <- .1
porcent <- (1-alpha)*100
m <- 10

LIX <- round(sqrt(((m-1)*(std)^2)/qchisq(alpha/2, m-1, lower.tail = F)), digits = 2) #lim inferior
LSX <- round(sqrt(((m-1)*(std)^2)/qchisq(alpha/2, m-1, lower.tail = T)), digits = 2) #lim superior
paste("El intervalo de confianza para ",expression(sigma)," a ", porcent, "% de confianza está entre: ",LIX," y ", LSX, sep = "")


# Punto 7.b.a. Intervalos de confianza para estimar u en distr. normal 
xbar <- 3.9
sigma <- 1.03
n <- 102
alpha <- 0.1
porcen <- (1-alpha)*100

LIN <- round(xbar - (qnorm(alpha/2, lower.tail = F) * (sigma/sqrt(n))), digits = 2)
LSN <- round(xbar + (qnorm(alpha/2, lower.tail = F) * (sigma/sqrt(n))),digits = 2)
paste("El intervalo de confianza para u del ", porcen, "% está entre: ",LIN," y ", LSN, sep = "")

#Punto 7.b.b.
pnorm(3.5, 3.9,1.03)
qnorm(0.95, 3.9,1.03)

# Punto 8.a. Intervalos de confianza para estimar u en distr. normal 
xbar <- 1100000
sigma <- 250000
n <- 26
alpha <- 0.01
porcen <- (1-alpha)*100

LIN <- round(xbar - (qnorm(alpha/2, lower.tail = F) * (sigma/sqrt(n))), digits = 2)
LSN <- round(xbar + (qnorm(alpha/2, lower.tail = F) * (sigma/sqrt(n))),digits = 2)
paste("El intervalo de confianza para u del ", porcen, "% está entre: ",LIN," y ", LSN, sep = "")

#Punto 11
xbar <- 5
sigma <- 0.7
n <- 37
alpha <- 0.05
porcen <- (1-alpha)*100

LIN <- round(xbar - (qnorm(alpha/2, lower.tail = F) * (sigma/sqrt(n))), digits = 2)
LSN <- round(xbar + (qnorm(alpha/2, lower.tail = F) * (sigma/sqrt(n))),digits = 2)
paste("El intervalo de confianza para u del ", porcen, "% está entre: ",LIN," y ", LSN, sep = "")

# Punto 12.a.
twelve <- c(35,44,38,55,33,56,67,45,48,40,50,35,56,41,47)
X <- mean(twelve)
std <- sd(twelve)
alpha <- .1
porcent <- (1-alpha)*100
m <- 15
LIN <- round(X - (qnorm(alpha/2, lower.tail = F) * (std/sqrt(m))), digits = 2)
LSN <- round(X + (qnorm(alpha/2, lower.tail = F) * (std/sqrt(m))),digits = 2)
paste("El intervalo de confianza para u del ", porcen, "% está entre: ",LIN," y ", LSN, sep = "")

# Punto 12.b.

LIX <- round(sqrt(((m-1)*(std)^2)/qchisq(alpha/2, m-1, lower.tail = F)), digits = 2) #lim inferior
LSX <- round(sqrt(((m-1)*(std)^2)/qchisq(alpha/2, m-1, lower.tail = T)), digits = 2) #lim superior
paste("El intervalo de confianza para ",expression(sigma)," a ", porcent, "% de confianza está entre: ",LIX," y ", LSX, sep = "")

