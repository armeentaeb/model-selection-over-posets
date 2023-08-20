### this function computes all elements that form a covering pair with input element
### input: pi1 -- total ranking
### output: neighb_list -- list, containing all elements that form covering pairs with pi1

neighbors <- function(pi1){
  p <- length(pi1)
  neighb_list <- list()
  i <- 1
  while (i <= p-1){
    if (pi1[i] < pi1[i+1]){
      if (i == 1){
        t2 <- pi1[as.integer(i+1):p]; t2 <- t2[which(t2!=pi1[i+1])]; t2 <- Reshape((t2),1,length(t2))
        pi_new <- cbind(c(pi1[i+1]),c(pi1[i]),t2)
        neighb_list <- append(neighb_list,list(pi_new))
      }else{
        t1 <- pi1[1:i-1]; t1 <- Reshape(t1,1,length(t1))
        t2 <- pi1[as.integer(i+1):p]; t2 <- t2[which(t2!=pi1[i+1])]; t2 <- Reshape(t2,1,length(t2))
        pi_new <- cbind(t1,c(pi1[i+1]),c(pi1[i]),t2)
        neighb_list <- append(neighb_list,list(pi_new))
      }
    }
    i = i+1
  }
  
  
  return(neighb_list)
}