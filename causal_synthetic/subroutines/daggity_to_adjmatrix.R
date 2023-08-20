## this function takes as input a dagitty object and converts it to an adjacency matrix
## obtained from "https://rdrr.io/github/IyarLin/orientDAG/man/dagitty_to_adjmatrix.html" -- see its documentation for more details

dagitty_to_adjmatrix <- function(daggity_obj) {
  edg <- dagitty:::edges(daggity_obj)
  node_names <- dagitty:::names.dagitty(daggity_obj)
  ans_mat <- matrix(
    data = 0, nrow = length(node_names),
    ncol = length(node_names),
    dimnames = list(node_names, node_names)
  )
  if (length(edg)!=0){
  M <- edg[c("v","w","e")]
  for (i in 1:nrow(M)){
    if (M[i,3] == "--"){
      ans_mat[as.numeric(M[i,1]),as.numeric(M[i,2])]<-1;ans_mat[as.numeric(M[i,2]),as.numeric(M[i,1])]<-1
    }
    else{
      ans_mat[as.numeric(M[i,1]),as.numeric(M[i,2])]<-1;
    }
  }
  }
  else{ans_mat <- matrix(0,p,p)}

  
  
  
  return(ans_mat)
}