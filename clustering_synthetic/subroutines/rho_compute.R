## this function computes the similarity valuation of two elements 
## input: element1 and element2 are two partitions
## output: similarity valuation of the two input elements

rho_compute <- function(element1,element2){
   
   # approach is based on computing rank of the meet (since poset is a lattice and thus has well-defined meet)
   s <- 0
   n <- 0
   meet <- list()
   for (i in 1:length(element1)){
     for (j in 1:length(element2)){
       set_intersect <- intersect(element1[[i]],element2[[j]])
       if (length(set_intersect)>0){
         meet <- append(meet,list(set_intersect))
       }
     }
     n <- n+length(element1[[i]])
   }

  return(n-length(meet))
}