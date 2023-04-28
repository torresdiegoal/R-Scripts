rm(list = ls())

################################################
##### Hacer plots con símbolos matemáticos #####
###############################################

set.seed(2059)
x <- rnorm(25)
y <- rnorm(25)

## CON LA FUNCION EXPRESSION( )

plot(x,y, xlab = expression(hat(x)[t]),
     ylab = expression(phi^{rho + a}),
     main = "Pure Expressions")

#with spaces
plot(x,y, xlab = expression(hat(x)[t] ~ z * w), # ~ GIVE A SPACE
     ylab = expression(phi^{rho + a} * z ~ w), # * JOINED
     main = "Pure Expressions with Spacing")

#with more text
plot(x,y, 
     xlab = expression(paste("Text here ", hat(x), " here ", z^rho, " and here")), 
     ylab = expression(paste("Here is some text of ", phi^{rho})), 
     main = "Expressions with Text")

#SUBSTITUTING EXPRESSIONS
#We may also want to substitute actual values in for a lable.For instance, if we were automating the
#creation of plots with different values of some variable, we could use the following strategy to 
#update the labels as needed to reflect each of the changing values.

#Below, we first create two values, py and ee, from which we want to display the actual numerical 
#value. Using the substitute() command (and paste() once again), we add the call to these values, 
#p being the call to py and e for ee (this is specified in the list() option in substitute()).

py = 3.14
ee = 2.71

plot(x,y, 
     xlab = substitute(paste("Here is ", pi, " = ", p), list(p = py)), 
     ylab = substitute(paste("e is = ", e ), list(e = ee)), 
     main = "Substituted Expressions")
