## this function computes the false discovery bound 
## input: partition_bags -- partitions obtained from subsampling
## output: overall_FD -- false discovery bound

FD_computation <- function(partition_bags){
  
  overall_FD<- 0
  for (k in 1:(p-1)){
    s <- 0
    for (l in 1:(k)){
      s<- s + choose(k+1,l)#*min(k+1-l,l)
    }
    
    # computing q_k based on the analysis in the paper
    qk<- 0
    for (l in 1:(2*num_bags)){
      partition <- partition_bags[[l]]
      for (h in 1:length(partition)){
        if (length(partition[[h]]) > k+1){
          qk <- qk + s*choose(length(partition[[h]]),k+1)
        }
      } 
    }
    qk <- qk/(2*num_bags)
    Sk<- s*choose(p,k+1)
    
    overall_FD <- overall_FD + qk^2/((1-2*alpha)*Sk)
  }
  
  return(overall_FD)
}