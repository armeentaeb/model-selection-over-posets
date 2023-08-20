## this function computes the number of clusters in k-means clustering to use to attained desired false discovery level
## input: data matrix and desired false discovery level
## output: number of clusters in k-means clustering

choose_threshold_FD_bound <- function(data,FD_max){
  num_clusters <- p-1
  FD <- 0
  while (FD <= FD_max){ 
    partition_bags <- subsampling_estimates(data,num_clusters)
    FD <- FD_computation(partition_bags)
    num_clusters  <- num_clusters -1
  }
  return(num_clusters)
}
