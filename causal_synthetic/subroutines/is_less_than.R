## this function returns whether a CPDAG is smaller (in a partial order sense) than another
## input: CPDAG x1 and CPDAG x2
## output: 1 if x1 is smaller than x2 and 0 otherwise
# using the result of Lemma 5.2 of https://arxiv.org/pdf/1301.2282. 
is_less_than <- function(x1,x2){

  # check skeleton of x1 contained in x2
  dags_1<- cpdag_to_all_dags(x1)
  dags_2 <- cpdag_to_all_dags(x2)
  dag_1 <- dags_1[[1]]
  dag_2 <- dags_2[[1]]
  A <- x1; B <- x2; A <- 1*((A+t(A))>0); B <- 1*((B+t(B))>0)
  s1 <- length(which(B-A <0))
  if (s1 > 0){
    return(0)
  }else{
    s2 <- 0
    for (i in 1:(nrow(x1)-1)){
      for (j in (i+1):nrow(x1)){
        pa_set <- rbind(setdiff(which(dag_2[,i] > 0),j),setdiff(which(dag_2[,j] > 0),i))
        if (B[i,j] == 0 &&  !dsep(as.character(i),as.character(j),as.character(pa_set),as(dag_1,"graphNEL"))){
          s2 <- s2+1
        }
        
      }
    }
    
    if (s2 > 0){
      return(0)}else{
        return(1)
      }
    
  }
}


  
  
  
  
  
  
  # 
  # 
  # 
  # 
  # # check for v-structures
  # s2 <- 0
  # for (i in 1:nrow(x1)){
  #   for (j in 1:nrow(x1)){
  #     for (k in 1:nrow(x1)){
  #     
  #       # v-structure in x1, then there must be a v-structure in x2 or is covered
  #     if (x1[i,k] == 1 && x1[j,k] == 1 && A[i,j] == 0){
  #       if (B[i,j] == 0 && !(x2[i,k] == 1 && x2[j,k]==1)){
  #         s2 <- s2+1
  #       }
  #       
  #     }
  #     
  #       # v-structure in x2, either v-structure must be removed by not having one of the edges
  #       
  #       if (x2[i,k] == 1 && x2[j,k] == 1 && B[i,j] == 0){
  #         if (A[i,k] == 1 && A[j,k] == 1 && (x1[i,k] == 0 || x1[j,k] == 0)){
  #           s2 <- s2+1
  #         }
  #         
  #       }
  #     
  #       
  #       
  #       
  #     }
  #   }
  #   
  #   
  #   
  # }
  
  
  