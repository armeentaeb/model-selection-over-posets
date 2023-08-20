## set seed
set.seed(42)

## Load packages
library(MASS)
library(pcalg)
library(matrixcalc)
library(pracma)
library(devtools)
library(Rcpp)
library(Matrix)
library(igraph)

# adjust this path
root_path = "/Users/ataeb/Dropbox/Poset code/"


path = paste(root_path,"total_ranking_synthetic/subroutines",sep = "")

## source all relevant files
setwd(path)
files.sources = list.files()
sapply(files.sources, source)


## setup parameters
p <- 30 # number of items
pi_0 <- c(1:p) # least element ranking
num_bags <- 50 # B
num_iter <- 50 # number of iterations to compute error metrics
alpha <- 0.3 # cutoff threshold for stability based approach
k_max <- p*(p+1)/2 
FD_max <- 3 # upper bound on the number of false discoveries


## setup different experimental configurations
n_vec<- c(200,250,300) # number of pariwise comparisons used to obtain ranking
tau_vec <- c(0.97,0.975,0.98) # controls the closeness of the weights in the BTL model



## initialize vectors of true and false discoveries for stability approach and non-subsampling approach
FD_final<- c()
TD_final <- c()
FD_vanilla_final<- c()
TD_vanilla_final<- c()

## loop over configurations of how close the weights are
for (tau_iter in 1:length(tau_vec)){
  
  
  # obtain vector of weights in BTL model and normalize them
  pv<-(tau_vec[tau_iter])^seq(0,as.integer(p-1),by=1)
  pv[c(1,2,3)] <- pv[c(3,2,1)]
  pv[c(8,9,10)] <- pv[c(10,9,8)]
  pv[c(15,16,17)] <- pv[c(17,16,15)]
  pv[c(20,21,22)] <- pv[c(22,21,20)]
  pv[c(25,26,27)] <- pv[c(27,26,25)]
  
  p_norm <- matrix(0,length(pv),1)
  for (i in 1:length(pv)){
    p_norm[i] <- pv[i]/sum(pv)
  }
  
  # true total ordering
  pi_star = order(pv,decreasing = TRUE)
  
  data <- list()
  data[[1]] <- list()
  
  
  
  ## loop over number of pairwise comparisons
 
    for (samp_iter in 1:length(n_vec)){
      
      FD<-0;TD<-0;FD_vanilla<-0;TD_vanilla<-0
      
      for (iter in 1:num_iter){
      
      
        ## create list containing all the pairwise data
        for (i in 2:p){
          data[[i]] <- list()
          for (j in 1:as.integer(i-1)){
            data[[i]][[j]] <- rbinom(n_vec[samp_iter],1,p_norm[i]/(p_norm[i]+p_norm[j]))
          }
        }
        
        for (j in 1:as.integer(p-1)){
          for (i in as.integer(j+1):p){
            if(length(data[[i]][[j]]) > 0){data[[j]][[i]] <- 1-data[[i]][[j]]}
            else{data[[j]][[i]] <- list()}
          }
        }
        
        
        ## find the amount to threshold weight estimates from BTL model based on false discovery bound
        thresh <- choose_threshold_FD_bound(data,FD_max)
        
        ## find all the subsampling estimates
        out <- subsampling_estimates(data,thresh)
        pi_estimate <- out[[1]]
        
        ## stable estimate
        pi_out<- find_stable_total_ranking(pi_estimate)
        
        
        ## update true and false discoveries for stability based approach 
        FD <- FD+rho_compute(pi_out,pi_out) - rho_compute(pi_out,pi_star)
        TD <- TD+rho_compute(pi_out,pi_star)

        # find total ranking without subsampling (standard BTL maximum-liklihood base estimator)
        pi_vanilla <- rank_data(data,0)
        
        ## update true and false discoveries for approach without subsampling
        FD_vanilla <- FD_vanilla+rho_compute(pi_vanilla,pi_vanilla) - rho_compute(pi_vanilla,pi_star)
        TD_vanilla <- TD_vanilla+rho_compute(pi_vanilla,pi_star)

      }
      FD_final <- append(FD_final,FD/num_iter)
      TD_final <- append(TD_final,TD/num_iter)
      FD_vanilla_final<- append(FD_vanilla_final,FD_vanilla/num_iter)
      TD_vanilla_final <- append(TD_vanilla_final,TD_vanilla/num_iter)
     
      
      
    }
  }

## plot the results
save.image(paste(root_path,"synthetic_results_files_and_plotting/ranking_synthetic.RData",sep = ""))



