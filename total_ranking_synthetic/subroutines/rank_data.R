### this function performs total ranking using BTL maximum likelihood with thresholding
### input: data -- pairwise comparison data
          # threshold_min: value to threshold the difference in weights of BTL model
### output: pi_current -- estimated total ranking

rank_data <- function(data,threshold_min){
  
  # perform maximum likelihood estimate of BTL model
  p_temp <-  matrix(rpois(p*1,20),p)
  p_temp <- sort(p_temp,decreasing = TRUE)
  p_init <- matrix(0,as.integer(p),1)
  for (i in 1:as.integer(p)){p_init[i] <- p_temp[i]/sum(p_temp)}
  
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
  p_new <- p_new/max(p_new)
  
  ## starting from the least element, greedily move up to poset for which the difference in weights is above the minimum threshold 
  threshold_true <- TRUE
  pi_current <- 1:p
  while (threshold_true){
    
    
    nElement_temp <- neighbors(pi_current)
    threshold_val <- c()
    for (i in 1:length(nElement_temp)){
      temp<- nElement_temp[[i]]
      for (j in 1:p){
          if (temp[j]> pi_current[j]){
            threshold_val <- append(threshold_val,p_new[temp[j]]-p_new[pi_current[j]])
          }
      }
    }
    # choose a neighbor where the difference in the associated weights corresponding to items that are swapped is above minimum threshold
    ind <- which.max(threshold_val)
    if (threshold_val[ind] >= threshold_min){
      pi_current <- nElement_temp[[ind]]
    }else{
      threshold_true <- FALSE
    }
      
  }

  
  return(pi_current)
}

