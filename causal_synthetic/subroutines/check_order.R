# here x1 is a collection of DAGs computed from enumerating ones in a bag, x2 is a cpdag and z is a starting point cpdag
check_order <- function(x1,x2,neighb_meet_temp){
  
# find all neighbors of z that have no cycles
#neighb_meet_temp <- all_possible_neighbors(z)
neighb_meet <- list()
for (i in 1:length(neighb_meet_temp)){
  temp <- neighb_meet_temp[[i]]
  temp <- temp+t(temp); temp[which(temp==2)]=1
  x1_t <- x1[[1]]+t(x1[[1]]); x1_t[which(x1_t == 2)]=1
  x2_t <- x2+t(x2); x2_t[which(x2_t == 2)]=1

  if (sum(sum(hadamard.prod(temp,x1_t))) == sum(sum(temp)) && sum(sum(hadamard.prod(temp,x2_t))) == sum(sum(temp))){
    neighb_meet <- append(neighb_meet,list(temp))
  } 
}


if (length(neighb_meet) >0){
is_covered <- FALSE
t <- 1
s<-0
new_element <- NULL
while (!is_covered || t <= length(neighb_meet)){
  
  # find a mask function masking edges that are not present.
  mask <- matrix(1,p,p)
  for (i in 1:p){
    for (j in 1:p){
      if (neighb_meet[[t]][i,j] == 0 && neighb_meet[[t]][j,i] == 0){
        mask[i,j]<- 0; mask[j,i]<-0  
      }
    }
  }
  z2 <- pdag2dag(as((x2),"graphNEL"), keepVstruct=TRUE);
  z2 <- hadamard.prod(as(z2$graph,"matrix"),mask)
  z2 <- as(t(z2),"graphNEL")
  
  for (i in 1:length(x1)){
    adjacency <- x1[[1]]
    adjacency <- hadamard.prod(adjacency,mask)
    cp1 <- as(dag2cpdag(as((adjacency),"graphNEL")),"matrix")
    cp2 <- as(dag2cpdag(z2),"matrix")
    if ((cp1 == neighb_meet[[t]]) &&  (cp2 == neighb_meet[[t]])){s <- 1; new_element <- cp1; is_covered <- TRUE; break}
  }
  t <- t+1
}
return(list(s,new_element))
}else{
  list(0,x2)
}

}