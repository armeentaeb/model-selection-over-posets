## this function determines the amount of thresholding in difference of weights in BTL model so that the resulting set of relations has a desired control on false discoveries
## input: data -- pairwise comparison data
      ##: FD_max -- desired false discovery level
## output: the threshold value to achieve desired false discovery level
choose_threshold_FD_bound_partial_ranking <- function(data,FD_max){
 
  # adaptively changing the threshold until we achieve desired false discovery level
  thresh <- 1
  while(FD_bound <= FD_max){
    
    # compute average of ordered paritions where Adj_{ij} represents frequency of subsamples where i>j
    avg_adj <- subsampling_estimates_partial_ranking(data,thresh)
    
    # calculating the false discovery level
    FD_bound <- sum(sum(avg_adj))^2/((p*(p+1))*(1-2*alpha))
    
    # decreasing the threshold level until achieving desired false discovery level 
    thresh <- thresh-0.025
  }
  
  
return(thresh)
  
  
}
