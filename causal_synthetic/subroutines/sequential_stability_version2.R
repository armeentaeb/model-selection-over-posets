sequential_stability_version2 <- function(data,y,lam_vec,lam_iter){
  
  
  subsampling_statistics <- compute_subsampling_statistics(data,y,lam_vec,lam_iter)
  avg_sss <- subsampling_statistics[[2]]
  
  ## breadth-first search approach to obtaining a multiset
  
  # initialize breath-first search algorithm
  set_current <- list()
  set_to_visit <- list()
  set_visited <- list()
  set_final <- list()
  
  
  for (i in 1:p){
    temp <- (Reshape(data[,i],1,n)%*% avg_sss%*%Reshape(data[,i],n,1))/(norm(data[,i],type="2")^2)
    if (temp >= alpha){
      set_to_visit <- append(set_to_visit,list(list(c(),c(i),temp)))
      set_final<- append(set_final,list(c(i)))
      set_visited <- append(set_visited,list(list(c(),c(i),temp)))
      
    }
  }
  
  
  
  while(length(set_to_visit)>0){
    
    temp_list <- list()
    for (i in 1:length(set_to_visit)){
      
      original_vars <- set_to_visit[[i]][[1]]
      new_vars <- set_to_visit[[i]][[2]]
      temp_matrix <-  Reshape(svd(data[,new_vars])$u,n,length(new_vars))%*%t(Reshape(svd(data[,new_vars])$u,n,length(new_vars)))
      current_matrix <- (diag(n)-temp_matrix)%*%avg_sss%*%(diag(n)-temp_matrix)
      
      
      v <- matrix(0,p,1)
      for (i in setdiff(1:p,new_vars)){
        v[i]<- (Reshape(data[,i],1,n)%*%current_matrix%*%Reshape(data[,i],n,1))/(norm(data[,i],type="2")^2)
      }
      
      for (j in 1:p){
        if (v[j] >= alpha){
          temp_list <- append(temp_list,list(list(new_vars,append(new_vars,j),v[j])))
          if (length(which(list(new_vars) %in%set_final))>0){
            set_final <- set_final[-which(list(new_vars) %in%set_final)]
          }
          set_final <- append(set_final,list(append(new_vars,j)))
        }
      }
      
    }
    
    
    
    set_to_visit<- temp_list
    set_visited <- append(set_visited,temp_list)
  }
  
  
  temp <- list()
  if (length(set_final)>0){
    for (i in 1:length(set_final)){
      temp<-append(temp,list(sort(set_final[[i]])))
    }
    set_final <- unique(temp)
  }
  return(list(set_final,set_visited))
  
}