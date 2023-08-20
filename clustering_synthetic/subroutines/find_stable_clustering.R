## this function performs stable selection in clustering 
## input: partitions obtained from subsampling
## output: stable partition

find_stable_clustering<- function(partition_bags){

current_partition <- list()
for (i in 1:p){
  current_partition <- append(current_partition,list(i))
}

isStable <- TRUE
old_partition <- current_partition

# greedily move up to poset as long as stability criterion is satisfied
while(isStable){
  neighb <- neighbor_compute(old_partition)
  if (length(neighb) == 1){
    isStable <- FALSE
  }
  
  stable_vals <- c()
  for (i in 1:length(neighb)){
    temp <- 0
    for (l in 1:(2*num_bags)){
      temp <- temp+rho_compute(neighb[[i]],partition_bags[[l]])-rho_compute(old_partition,partition_bags[[l]])
    }
    
    # compute constant c 
    c <- compute_c(neighb[[i]],old_partition)
    
    
    stable_vals <- append(stable_vals,temp/(2*num_bags*(1-alpha)*c))
  }
  # if stable criterion is satisfied, choose the most stable
  if (max(stable_vals) >= 1){
    ind <- which.max(stable_vals)
    old_partition <- neighb[[ind]]
  }else{
    isStable <- FALSE
  }
}
return(old_partition)
}