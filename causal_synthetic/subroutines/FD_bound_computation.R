## this is a function that computes our false discovery bound in the causal structure learning context
## input: partition_bags -- CPDAG estimatesf from subsampling
## output: false discovery bound from our threorem results in the context of causal structure learning

FD_bound_computation <- function(partition_bags){
  
  FD_out <- 0
  # compute q_k and S_k for every k
  for (k in 1:(p-1)){
    
    # compute Sk
    if (k == 1){
      Sk <- choose(p,2)
    }
    else{
      t <- 0
      for (j in 1:k){
        t <- t + choose(k,j)
      }
      t <- t - k
      Sk <- p*choose(p-1,k)*t*k
    }
    
    # compute empirical estimate for qk
    if (k == 1){
      qk<-0;for (i in 1:(2*num_bags)){temp<-partition_bags[[i]]; temp <- temp+temp; temp[which(temp==2)]<-1; qk <- qk+sum(sum(temp))/2 }
    }
    
    else{
      q_k <- 0
      for (l in 1:length(partition_bags)){
        
        Z<-equivalentDAGs(pcalg2dagitty(t(partition_bags[[l]]), 1:p,type = "dag"))
        sym<-partition_bags[[l]]; sym<-sym+t(sym); sym[which(sym==2)]<-1
        
        for (i in 1:p){
          ind <- which(sym[i,] == 1)  
          if (length(ind) >= k){
            A <- combn(ind,k)
            CPDAG_cont <- list()
            for (k in 1:ncol(A)){
              for (j in 1:length(Z)){
                temp <- t(dagitty_to_adjmatrix(Z[[j]]))
                temp[i,setdiff(1:p,A[,k])] <- 0
                temp_new <- matrix(0,p,p)
                temp_new[i,] <- temp[i,]
                CPDAG_cont <- append(CPDAG_cont,list((1*dag2cpdag(t(temp_new)))))
              }
            }
            
            CPDAG_cont <- unique(CPDAG_cont)
            
            for (i in 1:length(CPDAG_cont)){
              if (sum(sum(CPDAG_cont[[i]]))>0){
                smaller_neighbs <- neighbors_delete(CPDAG_cont[[i]])
                for (j in 1:length(smaller_neighbs)){
                  
                  q_k <- q_k + compute_rho(CPDAG_cont[[i]],partition_bags[[l]]) -compute_rho(smaller_neighbs[[j]],partition_bags[[l]])
                }
              }
            }
          }
          
        }
      }
    }
    qk <- qk/(2*num_bags)
    
    # false discovery bound
    FD_out <- FD_out + qk^2/((1-2*alpha)*Sk)
  }
  
  
  
  return(FD_out)
}

