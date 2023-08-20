all_possible_neighbors= function(x_current,stable_edges){
  
  
  #all_possible_ind <- which(x_current+t(x_current)+diag(p) == 0,arr.ind = TRUE)
  all_possible_ind1 <- which(x_current+t(x_current)+diag(p) == 0)
  all_possible_ind2 <- which(stable_edges == 1)
  all_possible_ind_temp <- intersect(all_possible_ind1,all_possible_ind2)
  all_possible_ind<- c()
  for (i in 1:length(all_possible_ind_temp)){
    col<-ceil(all_possible_ind_temp[i]/p)
    row<- all_possible_ind_temp[i]-(col-1)*p
    all_possible_ind<-rbind(all_possible_ind,c(row,col))
  }
  all_possible_neighb <- list()
  
  for (i in 1:nrow(all_possible_ind)){
    z1_ind <- all_possible_ind[i,1]
    z2_ind <- all_possible_ind[i,2]
    # find neighbors of z2
    
    neighbors_T <- c()
    for (k in 1:p){
      if (k %in% which(x_current[z2_ind,]==1) && k %in% which(x_current[,z2_ind] == 1) && k%in%which(x_current[z1_ind,] == 0) && k%in%which(x_current[,z1_ind] == 0)){
        neighbors_T <- append(neighbors_T,k)
      }
    }
    
    if (length(neighbors_T) == 0){
      x_new_pdag <- x_current
      x_new_pdag[z1_ind,z2_ind]<-1
      if (isValidGraph(t(x_new_pdag),type = "pdag")){
        
        g <- as((x_new_pdag),"graphNEL")
        dag <- pdag2dag(g, keepVstruct=TRUE)
        cpdag<- dag2cpdag(dag$graph)
        all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix"))) 
        
      }
      
    }else{
      
      all_T <- powerset(neighbors_T)
      for (j in 1:length(all_T)){
        
        if (length(all_T[[j]])==0){
          x_new_pdag <- x_current
          x_new_pdag[z1_ind,z2_ind]<-1
          if (isValidGraph(t(x_new_pdag),type = "pdag")){
            
            g <- as((x_new_pdag),"graphNEL")
            dag <- pdag2dag(g, keepVstruct=TRUE)
            
            cpdag<- dag2cpdag(dag$graph)
            all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix"))) 
            
            # }
          }
        }else{  
          x_new_pdag <- x_current
          x_new_pdag[z1_ind,z2_ind]<-1
          x_new_pdag[z2_ind,all_T[[j]]] <- 0
          if (isValidGraph(t(x_new_pdag),type = "pdag")){
            
            g <- as((x_new_pdag),"graphNEL")
            dag <- pdag2dag(g, keepVstruct=TRUE)
            cpdag<- dag2cpdag(dag$graph)
            all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix"))) 
            
          }
        }
      }
    }
    
    # z1_ind <- all_possible_ind[i,2]
    # z2_ind <- all_possible_ind[i,1]
    # # find neighbors of z2
    # 
    # neighbors_T <- c()
    # for (i in 1:p){
    #   if (i %in% which(x_current[z2_ind,]==1) && i %in% which(x_current[,z2_ind] == 1) && i%in%which(x_current[z1_ind,] == 0) && i%in%which(x_current[,z1_ind] == 0)){
    #     neighbors_T <- append(neighbors_T,i)
    #   }
    # }
    # all_T <- powerset(neighbors_T)
    # if (length(neighbors_T)==0){
    #   x_new_pdag <- x_current
    #   x_new_pdag[z1_ind,z2_ind]<-1
    #   if (isValidGraph(t(x_new_pdag),type = "pdag")){
    #   
    #   g <- as((x_new_pdag),"graphNEL")
    #   dag <- pdag2dag(g, keepVstruct=TRUE)
    #   cpdag<- dag2cpdag(dag$graph)
    #   all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix"))) 
    #   
    #   }
    #   #}
    # }else{
    #   for (j in 1:length(all_T)){
    #     
    #     if (length(all_T[[j]])==0){
    #       x_new_pdag <- x_current
    #       x_new_pdag[z1_ind,z2_ind]<-1
    #       if (isValidGraph(t(x_new_pdag),type = "pdag")){
    #         
    #       g <- as((x_new_pdag),"graphNEL")
    #       dag <- pdag2dag(g, keepVstruct=TRUE)
    #       cpdag<- dag2cpdag(dag$graph)
    #       all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix")))
    #       }
    #      # }
    #       
    #     }else{  
    #       x_new_pdag <- x_current
    #       x_new_pdag[z1_ind,z2_ind]<-1
    #       x_new_pdag[z2_ind,all_T[[j]]] <- 0
    #       if (isValidGraph(t(x_new_pdag),type = "pdag")){
    #         
    #       g <- as((x_new_pdag),"graphNEL")
    #       dag <- pdag2dag(g, keepVstruct=TRUE)
    #       cpdag<- dag2cpdag(dag$graph)
    #       all_possible_neighb  <- append(  all_possible_neighb ,list(as(cpdag,"matrix"))) 
    #     
    #       }
    #     }
    #   }
    # }
  }
  return(unique(all_possible_neighb))
  
}
