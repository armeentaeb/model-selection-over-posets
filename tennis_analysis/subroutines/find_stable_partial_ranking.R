## this function perform stability based procedure in partial ranking
## input: Adj -- aggregation of partial ranking estimates from subsamples
## output: a stable ordered partition

find_stable_partial_ranking <- function(Adj){

# find pairwise ordering that appear with high frequency across the ordered partitions from subsampling
ind <- which(Adj >= 1-alpha)

# starting from the least element, greedily move up the poset by adding pairwise orderings while preserving transitivity
if (length(ind) == 0){print("nothing is selected")}else{
  order_ind <- order(Adj[ind],decreasing = TRUE)
  x <- matrix(0,p,p); x[ind[order_ind[1]]] <- 1
  remain <- setdiff(ind,ind[order_ind[1]])
  is_transitive<- FALSE
  while(length(remain) >0 && !is_transitive){
    temp <- FALSE
    for (i in 1:length(remain)){
      xtemp <-x
      xtemp[remain[i]]<-1
      ind3<- which(xtemp%*%xtemp==1);
      
      if (length(which(xtemp[ind3]==0))==0){
        x <- xtemp
        remain <- setdiff(remain,remain[i]);
        temp <- TRUE
        break
      }
      
    }
    if ((i == length(remain)) && !temp){
      is_transitive<- TRUE
    }
  }
  
}
return(x)
}
