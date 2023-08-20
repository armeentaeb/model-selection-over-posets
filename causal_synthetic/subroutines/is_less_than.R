## this function returns whether a CPDAG is smaller (in a partial order sense) than another
## input: CPDAG x1 and CPDAG x2
## output: 1 if x1 is smaller than x2 and 0 otherwise
is_less_than <- function(x1,x2){

dags_1<- cpdag_to_all_dags(x1)
dags_2 <- cpdag_to_all_dags(x2)
s<- 0
for (i in 1:length(dags_1)){
  for (j in 1:length(dags_2)){
    if (sum(sum(hadamard.prod(dags_1[[i]],dags_2[[j]]))) == sum(sum(dags_1[[i]]))){
      s <- s+1
    }
  }
}

if (s > 0){ return(1)}else{return(0)}
}
