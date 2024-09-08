is_polytree <- function(M){
  output <- FALSE
  N <- M+t(M); N[which(N==2)]<-1
  s <- 0
  for (i in 1:p){
   for (j in 1:p){
     for (k in 1:p){
       if (i!= j && j!= k && i!= k && N[i,j]*N[j,k]*N[k,i] == 1){
          s <- s+1         
       }
     }
   } 
  }
  g <- as((M),"graphNEL")
  adj<- (as(pdag2dag(g, keepVstruct=TRUE)$graph,"matrix"))
  if (s == 0 & is_chordal(graph_from_adjacency_matrix(adj))$chordal){
    output <- TRUE
  }
  return(output)
}