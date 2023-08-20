## this function computes the similarity valuation of two CPDAGs
## input: two CPDAGs
## output: their similarity valuation

compute_rho <- function(x,y){
  
  out<- list()
  desc_x<- append(unique(all_smaller_elements(list(x),out)),list(x))
  s <- c()
  for (j in 1:length(desc_x)){
    s <- append(s,is_less_than(desc_x[[j]],y)*rank(desc_x[[j]]))
  }
  return(max(s))
}