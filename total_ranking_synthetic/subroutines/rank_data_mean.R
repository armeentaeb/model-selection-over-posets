### this function performs total ranking using BTL maximum likelihood with thresholding
### input: mean_vec -- vector where each coordinate represents mean of the corresponding variable
# threshold_min: value to threshold the difference in weights of BTL model
### output: pi_current -- estimated total ranking


rank_data_mean <- function(mean_vec,threshold_min){
  
   p_new <- mean_vec
   
   ## starting from the least element, greedily move up to poset for which the difference in means is above the minimum threshold 
   
   threshold_true <- TRUE
   pi_current <- 1:p
   while (threshold_true){
     
     
     nElement_temp <- neighbors(pi_current)
     threshold_val <- c()
     for (i in 1:length(nElement_temp)){
       temp<- nElement_temp[[i]]
       for (j in 1:p){
         if (temp[j]> pi_current[j]){
           threshold_val <- append(threshold_val,p_new[temp[j]]-p_new[pi_current[j]])
         }
       }
     }
     
     # choose a neighbor where the difference in the associated means corresponding to items that are swapped is above minimum threshold
     ind <- which.max(threshold_val)
     if (threshold_val[ind] >= threshold_min){
       pi_current <- nElement_temp[[ind]]
     }else{
       threshold_true <- FALSE
     }
     
   }
   
  return(pi_current)
  
}