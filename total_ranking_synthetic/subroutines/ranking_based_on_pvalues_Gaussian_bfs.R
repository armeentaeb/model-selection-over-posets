### this function identifies all the poset elements that can be returned from taking paths in the poset 
### satisfying the testing criterion
### input: X -- data e.g. test scores of OECD countries
### output: pi_current -- join of all poset elements that satisfy the testing criterion

ranking_based_on_pvalues_Gaussian_bfs <- function(X){
 
   # initialize breath first search algorithm  
   pi_current <- c(1:p)
   pi_to_visit <- list()
   pi_visited <- list()
   pi_to_visit <- append(pi_to_visit,list(pi_current))
   pi_visited <- append(pi_visited,list(pi_current))
   
   
   while(length(pi_to_visit)>0){
     
     temp_list <- list()
     for (i in 1:length(pi_to_visit)){
       
       pi_temp <- pi_to_visit[[i]]
       list_neighbs <- neighbors(pi_temp)
       p_val_neighbs <- c()
       for (i in 1:length(list_neighbs)){
         pi_temp2 <- list_neighbs[[i]]
         
         # find the p-value associated to moving from pi_temp to pi_temp_2 in the poset
         for (i in 1:(p)){
           if (pi_temp2[i]!=pi_temp[i]){
             p_val <- compute_p_vals_Gaussian(X[[pi_temp2[i]]],X[[pi_temp[i]]])
             break
           }
         }
         # check if the p-value threshold criterion is satisfied
         if(p_val <= alpha/(p*(p-1)/2) && !(list(pi_temp2) %in% pi_visited)){
           
           temp_list <- append(temp_list,list(pi_temp2))
           pi_visited <- append(pi_visited,list(pi_temp2))
           
         } 
       }
       
     } 
     pi_to_visit<-temp_list 
     
   }
   
   # compute join of all the poset elements that satisfy the testing criterion
   pi_current<- 1:p
   for (i in 1:length(pi_visited)){
     pi_current <- compute_joins(pi_current,pi_visited[[i]])
   }

   return(pi_current)
}  
