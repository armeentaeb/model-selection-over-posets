compute_rho <- function(cpdag_1,cpdag_2){

  if (rank(cpdag_1) <= rank(cpdag_2)){
    x <- cpdag_1
    y <- cpdag_2
  }else{
    x<- cpdag_2
    y <- cpdag_1
  }

  
  visited <- list(x)
  to_visit <- list(x)
  output<- 0
  meet_found <- FALSE
  
  while(length(to_visit)>0 && (!meet_found)){
   temp_list <- list()
   for (i in 1:length(to_visit)){
     cpdag_temp <- to_visit[[i]]
     if (is_less_than(cpdag_temp,y)){
       to_visit <- list()
       output <- rank(cpdag_temp)
       meet_found <- TRUE
       break
     }
     list_neighbs <- neighbors_delete(cpdag_temp)
     temp_list <- append(temp_list,list_neighbs)
     visited <- append(visited,list_neighbs)
     #for (j in 1:length(list_neighbs)){
    #   if (!(list(list_neighbs[[j]])%in%visited)){
    #     temp_list <- append(temp_list,list(list_neighbs[[j]]))
    #     visited <- append(visited,list(list_neighbs[[j]]))
    #   }
    # }
     
     
   }
   to_visit<-unique(temp_list)
  }

return(output)
}