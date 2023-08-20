compute_p_vals <- function(y_sum,L){

      s<- 0
      for (k in as.integer(y_sum):L){
        if (k <= L/2){
          s <- s+ choose(L,k)*(k/L)^k*(1-k/L)^(L-k)
        }else{
          s <- s+ choose(L,k)*(1/2)^L
        }
      }
      
      
  return(s)
}