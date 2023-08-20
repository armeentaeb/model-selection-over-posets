### This function computes the amount of thresholding in ranking of means to have guaranteed false discovery level
### input:# X -- data e.g. test scores of OECD countries ......
          # FD_max -- desired false discovery bound  
### output: thresh -- the threshold based on the desired false discovery bound

choose_threshold_FD_bound_mean <- function(X,FD_max){
  
  pi_estimate <- list()
  
  # a vector computing q_k for different values of k
  q <- matrix(0,as.integer(p-1),1)

  FD_bound <- 0
  
  # initializing threshold to be large and slowly decreasing it until achieving desired false discovery bound
  thresh <- 20
  
  while(FD_bound <= FD_max){
    
    out<- subsampling_estimates_mean(data,thresh)
    q <- out[[2]]
    
    # compute the false discovery bound based on theorem result
    FD_bound <- 0
    for (i in 1:(p-1)){
      FD_bound <- FD_bound + q[i]^2/((p-i)*(1-2*alpha))
    }
    
    # update threshold
    thresh <- thresh-0.5
  }
  
  return(thresh)
}