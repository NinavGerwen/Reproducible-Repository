## To calculate the fit indices the Tucker-Lewis Index and Comparative Fit Index,
## we require a baseline model and saturated model to test a model against

## The loglikelihood of the base model can be calculated with the following 
## function, which only takes the data as input
base.model <- function(data) {
  
  k <- ncol(data)
  n <- nrow(data)
  
  ## Create an empty vector of length equal to test length
  pi <- rep(NA, k)
  
  ## Then, for every column/item:
  for(i in 1:k) {
    
    ## Determine the number of people who scored the item correctly
    n_i <- sum(data[, i] == 1)
    
    ## Determine the proportion of people who scored the item correctly
    pi_i <- mean(data[, i])
    
    ## Determine the likelihood that this many items people
    ## scored the item correctly
    pi[i] <- (pi_i)^n_i * (1 - (pi_i))^(n - n_i)
    
  }
  
  ## The total likelihood is then the product of all these likelihoods
  loglik <- log(prod(pi))
  
  print(loglik)
  
  ## Return the log of the likelihood
  return(loglik)
  
}

## The loglikelihood of the saturated model can be calculated through
## the following function, which takes as input the 
## score-pattern frequency aggregrated data and the total number of observations
sat.model <- function(agg_data, n_r) {
  
  ## Determine the number of observed score patterns
  n <- nrow(agg_data)
  
  ## Create an empty vector of length n
  pi <- rep(NA, n)
  
  ## Then, for every observed scorepattern
  for(i in 1:n) {
    ## get the number of times it has been observed
    n_x <- agg_data$fr[i]
    
    ## Get the relative frequency of the score pattern
    pi[i] <- log((n_x / n_r)^n_x)
    
  }
  
  ## Then, the log likelihood is calculated by taking the log of the
  ## product of all relative frequencies
  loglik <- sum(pi)
  
  ## And this value should be returned  
  return(loglik)
  
}



## Then, to calculate the TLI and CFI, we have a function which takes as input:
## the loglikelihood of the tested model, the dataset,
## the aggregated dataset, number of observations and test length
TLI <- function(testedlog, dataset, agg_data, n, k){
  
  ## The numerator is calculated by a chi-sq distribution
  numerator <- (2 * ((sat.model(agg_data, n_r = n) - testedlog)))/((2^k - 1) - 2*k)
  
  denominator <- (2 * (testedlog - base.model(dataset)))/(k)
  
  fit_value <- 1 - (numerator/denominator)
  
  return(fit_value)
  
  
}

## Then, to calculate the CFI:
CFI <- function(testedlog, dataset, agg_data, n, k){
  
  numerator <- (2 * (sat.model(agg_data, n_r = n) - testedlog)) - ((2^k - 1) - 2*k)
    
  denominator <- (2 * (testedlog - base.model(dataset))) - (k)
    
  fit_value <- 1 - (numerator/denominator)
  
  return(fit_value)
  
}


## Below are different variations of TLI that I tested
TLI2 <- function(testedlog, dataset, agg_data, n, k){
  
  ## The numerator is calculated by a chi-sq distribution
  numerator <- (2 * ((sat.model(agg_data, n_r = n) - testedlog)))/((2^k - 1) - 2*k)
  
  denominator <- (2 * ((sat.model(agg_data, n_r = n) - base.model(dataset))))/((2^k - 1) - k)
  
  fit_value <- 1 - (numerator/denominator)
  
  return(fit_value)
  
  
}

TLI3 <- function(testedlog, dataset, agg_data, n, k){
  
  ## The numerator is calculated by a chi-sq distribution
  numerator <- (2 * ((sat.model(agg_data, n_r = n) - testedlog)))/(2*k)
  
  denominator <- (2 * ((sat.model(agg_data, n_r = n) - base.model(dataset))))/(k)
  
  fit_value <- 1 - (numerator/denominator)
  
  return(fit_value)
  
  
}

TLI4 <- function(testedlog, dataset, agg_data, n, k){
  
  chi_base <- (2 * ((sat.model(agg_data, n_r = n) - base.model(dataset))))
  
  df_base <- ((2^k - 1) - k)
  
  chi_tested <- (2 * ((sat.model(agg_data, n_r = n) - testedlog)))
  
  df_tested <- ((2^k - 1) - 2*k)
  

  numerator <- (chi_base / df_base) - (chi_tested / df_tested)
  
  denominator <- (chi_base / df_base) - 1
  
  fit_value <- (numerator/denominator)
  
  return(fit_value)
  
  
}

