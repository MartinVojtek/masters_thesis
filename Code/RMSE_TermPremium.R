library(readxl)
library(optimx)
library(pracma)
library(tseries)
library(Metrics)

# parameters
h <- 0.9701556 # obtained from data

# function looking for optimal sigma and beta parameters
TP_findpar <- function(betasigma) {
  beta <- betasigma[1]
  sigma <- betasigma[2]
  # pre-allocate  
  b <- matrix(NA,4,1) 
  calculator <- 0
  # first part of equation  
  for (n in c(12,60,120,240)) {
    calculator <- calculator+1
    temp <- 0 
    for (k in 2:n) {
      temp1 <- 0
      for (j in 2:(k-1)){
        temp1 <- temp1 + beta*h^(k-j-1)
      }
      temp <- temp + beta * h^(k-2) + temp1
    }
    
    first_eq <- temp
    
    # second part of equation    
    temp2 <- 0
    for(k in 2:n){
      for(j in 2:(k-1)){
        for(i in 1:(j-1)){
          temp2 <- temp2 + h^(j-i-1)*h^(k-i-1)
        }
      }
    }
    
    second_eq <- temp2
    b[calculator] <- -1/n * sigma^2 * (first_eq + second_eq)
    
  }
  # transform output
  b <- b*100*12 # *100 because of percentage, *12 because of annual transformation
  # fit these values as well as possible, see TermPremiumCalibration.xlsx for details
  b_real <- matrix(NA,4,1) 
  b_real[1] <- -0.0685978
  b_real[2] <- 0.33780927
  b_real[3] <- 0.9332286
  b_real[4] <- 1.4565876
  b_dif <- rmse(b_real,b)
  # uncomment following rows to see how the optim function seeks best values, warning: it slows the algorithm significantly
  #cat("\nbeta", beta)
  #cat("\nsigma", sigma)
  #cat("\nbdiff", sum(b_dif))
  return(b_dif)
}
# starting values based on Kozicki and Tinsley
optimized <- optim(c(-383,0.69/1200), TP_findpar,method = c("Nelder-Mead"), control = list(maxit = 10000))
optimized$par
sprintf("%.10f",optimized$par)





