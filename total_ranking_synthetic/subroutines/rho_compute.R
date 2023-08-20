rho_compute <- function(pi1,pi2){
  n <- length(pi1)
  A1 <- matrix(0,p,p)
  A2 <- matrix(0,p,p)
  for (i in 2:p){
    for (j in 1:as.integer(i-1)){
      if (which(pi1 == i) < which(pi1 == j)){A1[i,j]<-1}
      if (which(pi2 == i) < which(pi2 == j)){A2[i,j]<-1}
    }
  }
  
  return(sum(hadamard.prod(A1,A2)))
}