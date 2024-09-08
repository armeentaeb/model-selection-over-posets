## this function computes the regularization in the regularized maximum likelihood estmator solved via GES for causal learning
## input: observational data and desired false discovery level
## output: regularization penalty lambda that would attain desired false discovery level via our stability based procedure

choose_regularization_FD_bound_causal <- function(data,FD_max){
  
  lambda <- 3
  FD <- 0
  while (FD <= FD_max){ 
    
     estimate_bags <- subsampling_estimates_causal(data,lambda) 
     FD<- FD_bound_computation(estimate_bags)
     lambda<- lambda*8/10
  }
  
  return(lambda)
}