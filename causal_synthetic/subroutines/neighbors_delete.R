## this function computes all the CPDAGs that are smaller (in partial order sense) than in input CPDAG and form a covering pair with the input CPDAG
## input: CPDAG x_current
## output: list containing all the CPDAGs that are smaller (in partial order sense) than x_current and form a covering pair with x_current

neighbors_delete <- function(x_current){
  
  all_possible_ind <- which(x_current+t(x_current) != 0  ,arr.ind = TRUE)

  all_possible_neighb <- list()
  
  # the output is obtained by performing valid delete operators as described in the paper "https://www.jmlr.org/papers/volume3/chickering02b/chickering02b.pdf"
  
  for (i in 1:nrow(all_possible_ind)){
    
    z1_ind <- all_possible_ind[i,1] #x
    z2_ind <- all_possible_ind[i,2] #y
    
    # find neighbors of z2
    neighbors_H <- c()
    for (k in 1:p){
      if (k %in% which(x_current[z2_ind,]==1) && k %in% which(x_current[,z2_ind] == 1) && (k%in%which(x_current[z1_ind,] == 1) || k%in%which(x_current[,z1_ind] == 1))){
        neighbors_H <- append(neighbors_H,k)
      }
    }
    
    
    all_H <- powerset(neighbors_H)
    if (length(neighbors_H)==0){
      x_new_pdag <- x_current
      x_new_pdag[z1_ind,z2_ind]<-0
      x_new_pdag[z2_ind,z1_ind]<-0
      
      if (isValidGraph(t(x_new_pdag),type = "pdag")){
        
        g <- as((x_new_pdag),"graphNEL")
        dag <- pdag2dag(g, keepVstruct=TRUE)
        cpdag<- dag2cpdag(dag$graph)
        all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix"))) 
        
        
      }
      
    }else{
      for (j in 1:length(all_H)){
        
        if (length(all_H[[j]])==0){
          x_new_pdag <- x_current
          x_new_pdag[z1_ind,z2_ind]<-0
          x_new_pdag[z2_ind,z1_ind]<-0
          
          if (isValidGraph(t(x_new_pdag),type = "pdag")){
            
            g <- as((x_new_pdag),"graphNEL")
            dag <- pdag2dag(g, keepVstruct=TRUE)
            
            cpdag<- dag2cpdag(dag$graph)
            all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix"))) 
            
          }
        }else{  
          x_new_pdag <- x_current
          x_new_pdag[z1_ind,z2_ind]<-0
          x_new_pdag[z2_ind,z1_ind]<-0
          
          x_new_pdag[z2_ind,all_H[[j]]] <- 1
          x_new_pdag[all_H[[j]],z2_ind] <- 0
          for (l in length(all_H[[j]])){
            if (x_new_pdag[all_H[[j]][l],z1_ind] == 1 && x_new_pdag[z1_ind,all_H[[j]][l]] == 1){
              x_new_pdag[all_H[[j]][l],z1_ind] <- 0
            }
          }
          
          
          if (isValidGraph(t(x_new_pdag),type = "pdag")){
            
            g <- as((x_new_pdag),"graphNEL")
            dag <- pdag2dag(g, keepVstruct=TRUE)
            cpdag<- dag2cpdag(dag$graph)
            all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix"))) 
          }
        }
      }
    }
    
  }
  return(unique(all_possible_neighb))
  
}
  
  
  
