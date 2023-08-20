## this function computes partitions obtained from the subsamples
## input: data -- pairwise comparison data
#: thresh -- the threshold value that is applied to total ranking obtained from BTL model to obtain ordered partition
## output: Adj -- aggregated ordered partition matrix where Adj_{ij} represents frequency that variable i > variable j across subsamples

subsampling_estimates_partial_ranking <- function(data,thresh){
  
  # this is a matrix that aggregates ordered partitions obtained from each subsample
  avg_Adj <- matrix(0,p,p)
  for (bag_iter in 1:num_bags){
    connected = FALSE
    
    # obtain complementary subsamples where the variables in each complementary subsample are connected
    while(!connected){
      
      data_subsample_1 = list()
      data_subsample_2 = list()
      for (i in 2:p){
        data_subsample_1[[i]] <- list() 
        data_subsample_2[[i]] <- list() 
        data_subsample_1[[1]] <- list() 
        data_subsample_2[[1]] <- list()
        for (j in 1:as.integer(i-1)){
          if (length(data[[i]][[j]])>=2){
            size = length(data[[i]][[j]])
            v<-sample(1:size, size, replace = FALSE, prob = NULL)
            temp <- data[[i]][[j]]
            temp_1 <- temp[v[1:as.integer(size/2)]]
            temp_2 <- temp[v[1+as.integer(size/2):as.integer(size-1)]]
            data_subsample_1[[i]][[j]] <- temp_1
            data_subsample_2[[i]][[j]] <- temp_2
            
          } else{
            data_subsample_1[[i]][[j]] <- list()
            data_subsample_2[[i]][[j]] <- list()
          }
        }
      }
      
      
      for (j in 1:as.integer(p-1)){
        for (i in as.integer(j+1):p){
          if(length(data_subsample_1[[i]][[j]]) > 0){data_subsample_1[[j]][[i]] <- 1-data_subsample_1[[i]][[j]]}
          else{data_subsample_1[[j]][[i]] <- list()}
          if(length(data_subsample_2[[i]][[j]]) > 0){data_subsample_2[[j]][[i]] <- 1-data_subsample_2[[i]][[j]]}
          else{data_subsample_2[[j]][[i]] <- list()}
        }
      }
      L_mat_1 <- matrix(0,p,p)
      for (i in 1:p){
        for (j in 1:p){
          if (i !=j){
            if (length(data_subsample_1[[i]][[j]])==0){L_mat_1[i,j]}else{
              L_mat_1[i,j] <- sum(data_subsample_1[[i]][[j]])}
          }
        }
      }
      
      L_mat_2 <- matrix(0,p,p)
      for (i in 1:p){
        for (j in 1:p){
          if (i !=j){
            if (length(data_subsample_2[[i]][[j]])==0){L_mat_2[i,j]}else{
              L_mat_2[i,j] <- sum(data_subsample_2[[i]][[j]])}
          }
        }
      }
      diag(L_mat_1)<-0;diag(L_mat_2)<-0
      connected <- (is.connected(graph_from_adjacency_matrix(L_mat_1))) &&(is.connected(graph_from_adjacency_matrix(L_mat_2)))
      
    }
    
    ## aggregating ordered partitions obtained from each subsample
    avg_Adj <- avg_Adj + rank_data_partial_ranking(data_subsample_1,thresh)
    avg_Adj <- avg_Adj + rank_data_partial_ranking(data_subsample_2,thresh)
    
  }
  avg_Adj<- avg_Adj/as.integer(2*num_bags)
  
  return(avg_Adj)
  
  
}
