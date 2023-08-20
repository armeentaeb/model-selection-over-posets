### this function performs the stability based approach for model selection
### input: pi_estimate -- ranking estimates from subsampling
### output: pi_current -- stable ranking element obtained from algorithm
find_stable_total_ranking <- function(pi_estimate){
  
  #initialize
  stableNeighbor = TRUE
  pi_current <- pi_0
  
  # proceed greedily until stability criterion cannot be satisfied
  while (stableNeighbor){

    # compute all possible neighbors
    nElement <- neighbors(pi_current)
    
    # for every every neighbor, compute how stable
    stab_elements <- matrix(0,1,length(nElement))
    for (i in 1:length(nElement)){
      temp_pi <- unlist(nElement[i])
      sval <- 0
      for (j in 1:as.integer(2*num_bags)){
        pi_bag <- unlist(pi_estimate[j])
        sval <- sval + rho_compute(temp_pi,pi_bag)-rho_compute(pi_current,pi_bag);
      }
      stab_elements[i] <- sval/(as.integer(2*num_bags))
    }
    #if the stability criterion is not satisfied, stop; else, move to the most stable element 
    if (max(stab_elements) < 1-alpha){ stableNeighbor = FALSE}
    else{
      ind <- which.max(stab_elements)
      pi_current <- unlist(nElement[ind])
    }
  }
  return(pi_current)
}
