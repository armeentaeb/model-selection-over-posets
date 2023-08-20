### This function computes the estimates from subsampling with ranking means
### input:# X -- X e.g. test scores of OECD countries ......
          # thresh -- the threshold based on the desired false discovery bound
### output: pi_estimate -- estimates from subsampling

subsampling_estimates_mean <- function(X,thresh){
  
  pi_estimate <- list()
  q <- matrix(0,as.integer(p-1),1)
  
  
  for (l in 1:num_bags){
    
    # obtaining complementary datasets
    sub_1 <- c()
    sub_2 <- c()
    for (j in 1:length(ordered_names)){
      temp<- X[[j]]; ind <- which(!is.na(temp))
      temp <- temp[which(as.matrix(school[ind,10])=="private")]
      
      v<-sample(1:length(temp), length(temp), replace = FALSE, prob = NULL)
      temp <- temp[v];
      sub_1 <- append(sub_1,mean(temp[1:as.integer(length(temp)/2)]))
      sub_2 <- append(sub_2,mean(temp[1+as.integer(length(temp)/2):length(temp)-1]))
    }
    
    # computing total ranking estimates for complementary datasets 
    pi_1 <- rank_data_mean(sub_1,thresh)
    
    # update the vector q
    for (i in 2:p){
      for (j in 1:as.integer(i-1)){
        if (which(pi_1 == i) < which(pi_1 == j)){q[as.integer(i-j)]<-q[as.integer(i-j)]+1}
      }
    }
    
    pi_2 <- rank_data_mean(sub_2,thresh)
    
    for (i in 2:p){
      for (j in 1:as.integer(i-1)){
        if (which(pi_2 == i) < which(pi_2 == j)){q[as.integer(i-j)]<-q[as.integer(i-j)]+1}
      }
    }
    
    pi_estimate <- append(pi_estimate,list(pi_1))
    pi_estimate <- append(pi_estimate,list(pi_2))
    
    
  }
  q<- q/as.integer(2*num_bags)
  
  return(list(pi_estimate,q))
  
}
  