## this function computes the neighbors in the clustering of a particular element that form covering pairs with the element
## input: the element
## output: list of all neighbors

compute_c <- function(new_partition,old_partition){
  
  for (i in 1:length(new_partition)){
    for (j in 1:length(old_partition)){
        if (length(intersect(new_partition[[i]],old_partition[[j]])) < length(new_partition[[i]]) && length(intersect(new_partition[[i]],old_partition[[j]])) > 0) {
            c <- min(length(intersect(new_partition[[i]],old_partition[[j]])),length(setdiff(new_partition[[i]],intersect(new_partition[[i]],old_partition[[j]]))))
            break; 
        }
    }
  }
  return(c)
}