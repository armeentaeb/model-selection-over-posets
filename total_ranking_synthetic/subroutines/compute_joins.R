### this function computes join of two total rankings
### input: pi_1 and pi_2 -- two total ranking estimates
### output: pi_current -- stable ranking element obtained from algorithm

compute_joins <- function(pi1,pi2){

  p <- length(pi1)
  pi_0 <- c(1)  
  
  for (i in 2:p){
    placed <- FALSE
    j <- 1
    while(!placed){
      if (which(pi1 == i) < which(pi1 == pi_0[j]) || which(pi2 == i) < which(pi2 == pi_0[j])){
        t1 <-  Reshape(pi_0[1:as.integer(j)-1],1,j-1)
        t2 <- Reshape(pi_0[j:length(pi_0)],1,length(pi_0)-j+1)
        pi_0<- cbind(t1,c(i),t2)
        placed <- TRUE
      }else{
        j <- j+1
      }
      if (j > length(pi_0)){
        pi_0 <- cbind(pi_0,c(i))
        placed <- TRUE
      }
    }
  }
  
  return(pi_0)
  
}
  
  