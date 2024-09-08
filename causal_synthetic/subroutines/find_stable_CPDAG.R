## this function computes stable CPDAG using stability-based algorithm
## input: CPDAGs estimated using GES with data obtained from subsampling
## output: stable CPDAG

find_stable_CPDAG <- function(estimate_bags){
  
  # enumerate all DAGs in the estimated cpdags across bags
  all_dags_estimates <- list()
  for (l in 1:as.integer(2*num_bags)){
    Z<-equivalentDAGs(pcalg2dagitty(t(estimate_bags[l][[1]]), 1:p,type = "dag"))
    temp <- list()
    for (i in 1:length(Z)){
      temp <- append(temp,list(dagitty_to_adjmatrix(Z[[i]])))
    }
    all_dags_estimates <- append(all_dags_estimates,list(temp)) 
  }
  
  
  # ensure stability
  is_stable <- TRUE
  # current cpdag is the empty one
  x_current <- matrix(0,p,p)
  
  current_rho_bag <- matrix(0,1,2*num_bags)
  
  # list of current meets
  current_meets <- list()
  for (i in 1:as.integer(2*num_bags)){
    current_meets <- append(current_meets,list(matrix(0,p,p)))
  }
  
  # find stable edges
  stable_edges <- matrix(0,p,p)
  for (l in 1:as.integer(2*num_bags)){
    M <-estimate_bags[[l]]
    M <- M + t(M); M[which(M == 2)]<-1
    stable_edges <- stable_edges + M
  }
  ind <- which(stable_edges/(2*num_bags) >= 1-alpha)
  stable_edges[ind] <-1; stable_edges[setdiff(1:p^2,ind)] <-0
  
  # all dags across all bags
  all_dags_bags <- list()
  for (l in 1:as.integer(2*num_bags)){
    all_dags_bags <- append(all_dags_bags,list(cpdag_to_all_dags(estimate_bags[[l]])))
  }
  
  while(is_stable){
    # find all neighbors that do not have cycles
    all_neighb_temp <- all_possible_neighbors(x_current,stable_edges)
    all_neighb <- list()
    for (i in 1:length(all_neighb_temp)){
      M<- all_neighb_temp[[i]];M <- M+t(M); M[which(M==2)]<-1
      if (diameter(graph_from_adjacency_matrix(M),directed = FALSE)<=2){
        if (sum(sum(hadamard.prod(M,stable_edges))) == sum(sum(M)) && is_polytree(all_neighb_temp[[i]])){
          all_neighb <- append(all_neighb,list(all_neighb_temp[[i]]))
        }
      }
    }
    
    neighbor_meet <- list()
    stable_neighbors_exists = c()
    
    
    
    
    # compute current value of rho function for every bag
    rho_bag <- c()
    
    for (l in 1:as.integer(2*num_bags)){
      rho_bag <- append(rho_bag,compute_rho(x_current,estimate_bags[[l]]))
    }
    
    
    if (length(all_neighb)==0){ is_stable <- FALSE}
    else{
      stab <- matrix(0,1,length(all_neighb))
      
      for (i in 1:length(all_neighb)){
        s <- 0
        for (l in 1:as.integer(2*num_bags)){
          temp<- compute_rho(all_neighb[[i]],estimate_bags[[l]])
          s<- s+ temp-rho_bag[l]
          if (temp-rho_bag[l] < 0 || temp-rho_bag[l]>=2 ){"ERROR"}
        }
        stab[i] <- s/(2*num_bags)
        
      }
      if (max(stab) < 1-alpha){ is_stable <- FALSE}else{
        x_current <- all_neighb[[which.max(stab)]]
      }
    }
  }
  
  x_stable <- x_current
  return(x_stable)
  
}