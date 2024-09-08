## this function computes all the CPDAGs that are smaller (in a partial order sense) than the input CPDAG
## input: list x containing CPDAGs and a list out
## output: all CPDAGs smaller than every CPDAG contained in x added to the list out
all_smaller_elements <- function(x,out){
  
   for (i in 1:length(x)){
     if (sum(sum(x[[i]])) > 0){
        neighbors_less <- neighbors_delete(x[[i]]) 
        out <- append(out,neighbors_less)
        return(append(out,all_smaller_elements(neighbors_less,out)))
        
     }
   }
}