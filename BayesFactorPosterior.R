##Estimating cut off posterior for Bayes Factor (5 = substantial and 10 = strong) 
library(rootSolve)

# Bayes Factor (K) is based on  Kass and Raftery (1995)

#K 	        Strength of evidence
#1 to 3.2 	Not worth more than a bare mention
#3.2 to 10 	Substantial
#10 to 100 	Strong
#> 100 	    Decisive 

#BF = (Post*(1-Prior))/((1-Post)*Prior) 
#BF <- (x*(1-(0.5/n)))/((1-x)*(0.5/n)) #lambda = 0.5, n = No. of predictors

#Cut off for migration predictors
#BF <- (x*(1-(0.5/12)))/((1-x)*(0.5/12)); n_migration = 12

fun <- function(x) {(x*(1-0.04166667))/((1-x)*0.04166667) - 3.2} #0.5/12 = 0.04166667

uniroot.all(fun, c(-10,10)) #BF(3.2) = 0.1221385, BF(10) = 0.3029551, BF(100) = 0.8130177

x <- seq(-5,5,0.1)
plot(x, fun(x))

#Cut off for Ne predictors
#BF <- (x*(1-(0.5/5)))/((1-x)*(0.5/5)); n_Ne = 5

fun <- function(x) {(x*(1-0.1))/((1-x)*0.1) - 3.2} #0.5/5 = 0.1

uniroot.all(fun, c(-10,10)) # BF(3.2) = 0.2624784, BF(10) = 0.5263355, BF(100) = 0.9174086