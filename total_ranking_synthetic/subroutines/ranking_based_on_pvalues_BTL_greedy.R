### this function identifies all the poset elements that can be returned from taking the greedy path in the poset
### satisfying the testing criterion
### input: X -- data e.g. test scores of OECD countries
### output: pi_current -- join of all poset elements that satisfy the testing criterion

ranking_based_on_pvalues_BTL_greedy <- function(X){
  
  pi_current <- pi_0
  p_value_threshold <- TRUE
  
  # proceed greedily until we cannot find satisfy the p-value criterion
  while(p_value_threshold)
  {
    list_neighbs <- neighbors(pi_current)
    p_val_neighbs <- c()
    
    for (l in 1:length(list_neighbs)){
      pi_temp <- list_neighbs[[l]]
      for (i in 1:(p)){
        if (pi_temp[i]!=pi_current[i]){
          temp <- X[[pi_temp[i]]][[pi_current[i]]]
          p_val_neighbs <- append(p_val_neighbs,compute_p_vals(length(which(temp==1)),length(temp)))
          break
        }
      }
    }
    

    if (min(p_val_neighbs) <= alpha/(p*(p-1)/2)){
      ind <- which.min(p_val_neighbs)
      pi_current <- list_neighbs[[ind]]
    }else{
      p_value_threshold <- FALSE
    }
    
  }
  return(pi_current)
}
