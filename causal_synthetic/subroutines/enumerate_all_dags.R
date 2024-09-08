## this function enumerates all the dags in a partially oriented DAG
## input: pdag
## output: all dags entailed by this pdag

enumerate_all_dags <- function(pdag){
  
  undirected_edges = list()
  temp_pdag1 <- pdag
  for (i in 1:p){
    for (j in 1:p){
      if (temp_pdag1[i,j] == 1 & temp_pdag1[j,i] == 1){
        undirected_edges = append(undirected_edges,list(c(i,j)))
        temp_pdag1[i,j]<- 0;temp_pdag1[j,i] <- 0
      }
    }
  }
  M <- expand.grid(replicate(length(undirected_edges), 0:1, simplify = FALSE))
  all_dags = list()
  if (length(undirected_edges) == 0){
    all_dags = list(pdag)
  }else{
  
  for (i in 1:nrow(M)){
    dag <- pdag
    v <- M[i,]
    for (j in 1:length(v)){
      if (v[j] == 1){
        edge_pair <- unlist(undirected_edges[j])
        dag[edge_pair[2],edge_pair[1]] <- 0
      }else{
        edge_pair <- unlist(undirected_edges[j])
        dag[edge_pair[1],edge_pair[2]] <- 0
      }
      if (isValidGraph(dag,type = "dag")){
        all_dags <- append(all_dags,list(dag)) 
      }  
    }
  }
  }
  return(all_dags)
}