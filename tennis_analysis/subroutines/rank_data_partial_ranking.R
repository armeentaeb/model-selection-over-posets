## this function identifies a set of irreflexive, assymetic, and transitive relations among the items using BTL maximum likelihood and thresholding of estimated weights
## input: data -- pairwise comparison data
##: thresh -- the threshold value used on the difference of weights estimated from BTL model
## output: adjacency matrix encoding the relations

rank_data_partial_ranking <- function(data,thresh){
  
  p_temp <-  matrix(rpois(p*1,20),p)
  p_temp <- sort(p_temp,decreasing = TRUE)
  p_init <- matrix(0,as.integer(p),1)
  for (i in 1:as.integer(p)){p_init[i] <- p_temp[i]/sum(p_temp)}
  
  # maximum-likelihood estimate of BTL model
  convergence <- FALSE
  p_old <- p_init
  p_new <- matrix(0,as.integer(p),1)
  while(!convergence){
    
    for (j in 1:as.integer(p)){
      a <- 0
      b<-0
      for (i in 1:as.integer(p)){
        if (i != j){
          a <- a + length(unlist(data[[j]][[i]]))/(p_old[j] + p_old[i])
          b<- b + sum(unlist(data[[j]][[i]]))
        }
      }
      if (a <= 0 || is.nan(a)){
        p_new[j] <- p_old[j]
      }
      else{p_new[j]<- b*a^(-1)}
    }
    
    if (norm(p_new - p_old,type = "F")/norm(p_old,type = "F") <= 0.001){convergence = TRUE}
    p_old <- p_new
    
  }
  
  # normalize estimate
  p_old <- p_old/sum(p_old)
  
  # storing the relations in an nxn matrix: adj[i,j] = 1 if i > j; 
  #since we are using difference of weights, the set of relations necessarily satisfies irreflexitivity, transitivity, and asymmetry  
  adj <- matrix(0,length(p_old),length(p_old))
  for (i in 1:length(p_old)){
    for (j in 1:length(p_old)){
      if (p_old[i]-p_old[j] >= thresh){
        adj[i,j] <- 1
      }
    }
  }
  
return(adj)
  
}
