## this function computes the partitions obtained from running k-means in different subsamples of data
## input: data matrix and number of clusters to use in k-means clustering algorithm
## output: partitions from subsampling

subsampling_estimates <- function(data,num_clusters){
    
  partition_bags <-  list()
    for (i in 1:num_bags){
      
      # generate first complementary subsample dataset
      temp1 <- sample(nrow(data[[1]]), floor(nrow(data[[1]]))/2)
      data_sub <- c()
      for (i in 1:p){
        data_sub <- rbind(data_sub,colMeans(data[[i]][temp1,]))
      }
      
      # perform k-means clustering and obtain first partition
      (cl <- kmeans(data_sub, num_clusters))
      
      
      vector <- cl$cluster
      partition_bag <- list()
      for (i in 1:num_clusters){
        ind <- which(vector == i) 
        partition_bag <- append(partition_bag,list(ind))
      }
      partition_bags <- append(partition_bags,list(partition_bag))
      
      # generate second complementary subsample dataset
      temp2 <- setdiff(1:nrow(data[[1]]),temp1)
      data_sub <- c()
      for (i in 1:p){
        data_sub <- rbind(data_sub,colMeans(data[[i]][temp2,]))
      }
      
      # perform k-means clustering and obtain second partition
      (cl <- kmeans(data_sub, num_clusters))
      vector <- cl$cluster
      partition_bag <- list()
      for (i in 1:num_clusters){
        ind <- which(vector == i) 
        partition_bag <- append(partition_bag,list(ind))
      }
      partition_bags <- append(partition_bags,list(partition_bag))
    }
  return(partition_bags)
}
