## this function comptues the rank of a given CPDAG (i.e. number of directed and undirected edges)
## input: CPDAG
## output: rank of the CPDAG
rank <- function(x){
  dags<- cpdag_to_all_dags(x)
  return(sum(sum(dags[[1]])))
}