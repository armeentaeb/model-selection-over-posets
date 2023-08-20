calculate_false_discoveries <- function(input1,input2){

# output  <- matrix(0,p,p)
# for (i in 1:length(all_dags_1)){
#   for (j in 1:length(all_dags_2)){
#     temp <- sum(sum(hadamard.prod(all_dags_1[[i]],all_dags_2[[j]])))
#       #sum(sum(hadamard.prod(matrix(all_dags_1[i,],p,p, byrow = TRUE),matrix(all_dags_2[j,],p,p, byrow = TRUE))))
#     if (sum(sum(output))<temp){
#       output <- hadamard.prod(all_dags_1[[i]],all_dags_2[[j]])
#     }
#   }
# }

pdag <- hadamard.prod(input1,input2)
g2 <- as((pdag), "graphNEL")
adj<- (as(pdag2dag(g2, keepVstruct=TRUE)$graph,"matrix"))


g2 <- as((adj), "graphNEL") ## convert to graph
cpdag2 <- dag2cpdag(g2)
all_dags_consider <- cpdag_to_all_dags(as(cpdag2,"matrix"))
all_dags_1 <- cpdag_to_all_dags(input1)
all_dags_2 <- cpdag_to_all_dags(input2)
s1<-0
for (i in 1:length(all_dags_consider)){
  for (j in 1:length(all_dags_1)){
    if (hadamard.prod(all_dags_consider[[i]],all_dags_1[[j]]) == all_dags_consider[[i]]){
      s1 <- s1+1
    }   
  }
}

s2<-0
for (i in 1:length(all_dags_consider)){
  for (j in 1:length(all_dags_2)){
    if (hadamard.prod(all_dags_consider[[i]],all_dags_2[[j]]) == all_dags_consider[[i]]){
      s2 <- s2+1
    }   
  }
}

if (s2== 0 || s1 ==0 ){
  return ("Error, rho is not a good function")
}

  
return(sum(sum(all_dags_1[[1]])) - sum(sum(adj)))
  
  
  
   # enumerate DAGS of pdag1
  #all_dags1 = enumerate_all_dags(pdag1)
  #all_dags2 = enumerate_all_dags(pdag2)
  
  #M = matrix(0,length(all_dags1),length(all_dags2))
  #for (i in 1:length(all_dags1)){
  #  for (j in 1:length(all_dags2)){
   #   dag1 <- unlist(all_dags1[i]); dag2 <- unlist(all_dags2[j])
  #    M[i,j]<- sum(hadamard.prod(dag1,dag2))
  #  }
  #}
  
  #min_max
  #s = matrix(0,length(all_dags1),1)
  #for (i in 1:length(all_dags1)){
   # s[i] = max(M[i,])
  #}
  #min_max_1 = min(s)
  #s = matrix(0,length(all_dags2),1)
  #for (i in 1:length(all_dags2)){
   # s[i] = max(M[,i])
  #}
  #min_max_2 = min(s)
  
  # calculate number of false discoveries
  #return(c(sum(sum(unlist(all_dags1[1])))-min(min_max_1,min_max_2),min(min_max_1,min_max_2)))
  
}