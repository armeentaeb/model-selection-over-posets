### This function computes the amount of thresholding in ranking of means to have guaranteed false discovery level
### input:# data -- pairwise comparison data ......
# thresh -- -- the threshold based on the desired false discovery bound
### output: pi_estimate -- total ranking estimates across different subsamples

subsampling_estimates <- function(data,thresh){
  pi_estimate <- list()
  q <- matrix(0,as.integer(p-1),1)
  for (bag_iter in 1:num_bags){
    connected = FALSE
    ## create complementary datasets where every item is ``connected" to another so that the BTL maximum likelihood exists 
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
    
    # obtain total ranking with first complementary dataset and update the vector q
    pi_1 <- rank_data(data_subsample_1,thresh)
    for (i in 2:p){
      for (j in 1:as.integer(i-1)){
        if (which(pi_1 == i) < which(pi_1 == j)){q[as.integer(i-j)]<-q[as.integer(i-j)]+1}
      }
    }
    # obtain total ranking with second complementary dataset and update the vector q
    
    pi_2 <- rank_data(data_subsample_2,thresh)
    for (i in 2:p){
      for (j in 1:as.integer(i-1)){
        if (which(pi_2 == i) < which(pi_2 == j)){q[as.integer(i-j)]<-q[as.integer(i-j)]+1}
      }
    }
    
    # store the total rankings from complementary subsamples
    pi_estimate <- append(pi_estimate,list(pi_1))
    pi_estimate <- append(pi_estimate,list(pi_2))
    
  }
  q <- q/(2*num_bags)
  return(list(pi_estimate,q))
}

