### This function computes the amount of thresholding in ranking of means to have guaranteed false discovery level
### input:# data -- pairwise comparison data ......
# FD_max -- desired false discovery bound  
### output: thresh -- the threshold based on the desired false discovery bound

choose_threshold_FD_bound <- function(data,FD_max){

# initializing threshold to be large and slowly decreasing it until achieving desired false discovery bound
FD_bound <- 0
thresh <- 0.2

while(FD_bound <= FD_max){
  pi_estimate <- list()
  out <- subsampling_estimates(data,thresh)
  q<- out[[2]]
  
  # compute the false discovery bound based on theorem result
  FD_bound <- 0
  for (i in 1:(p-1)){
    FD_bound <- FD_bound + q[i]^2/((p-i)*(1-2*alpha))
  }
  # update threshold
  thresh <- thresh-0.005
}
return(thresh)
}