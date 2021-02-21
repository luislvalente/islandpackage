
#LogLik- vector of logliks e.g. LogLik = c(-100.1,-102,-105.2)
# k - number of parameters e.g. k =c(4,5,3)
# n - sample size, should be the same as M e.g. n = 1000
# IC_type - for the weights function only - 'AIC', 'BIC' or 'AICc'

# AIC Akaike Information Criterion
myAIC <- function(LogLik,k){
  aic <- (2*k)-(2*LogLik)
  return(aic)
}

## AIC for small sample sizes
myAICc <- function(LogLik,k,n){
  aicc <- myAIC(LogLik,k)+((2*k*(k+1))/(n-k-1))
  return(aicc)
}

# BIC Bayesian Information Criterion
myBIC <- function(LogLik,k,n){
  bic <- -2*LogLik+k*(log(n)+log(2*pi))
  return(bic)
}


# IC weights
ICweights <- function(LogLik,k,n,IC_type){
  if(IC_type == 'AIC'){
    IC <- myAIC(LogLik,k)
  }
  if(IC_type == 'AICc'){
    IC <- myAICc(LogLik,k,n)
  }
  if(IC_type == 'BIC'){
    IC <- myBIC(LogLik,k,n)
  }
  bestmodelIC <- min(IC)
  weights <- exp(-0.5*(IC-bestmodelIC))
  weights <- weights/sum(weights)
  return(weights)
}
