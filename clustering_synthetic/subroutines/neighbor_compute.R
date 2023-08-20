## this function computes the neighbors in the clustering of a particular element that form covering pairs with the element
## input: the element
## output: list of all neighbors
neighbor_compute <- function(element){
  output <- list()
  if (length(element) == 1){
    return(element)
  }else{
  for (i in 1:length(element)){
    for (j in 1:length(element)){
      if (j > i){
        remain_ind <- setdiff(1:length(element),c(i,j))
        temp_list <- element[remain_ind]
        temp_list <- append(temp_list,list(c(element[[i]],element[[j]])))
        output <- append(output,list(temp_list))
      }
      
    }
  }
   return(output)   
  }
}