## set seed
set.seed(42)

## Load packages
library(MASS)
library(matrixcalc)
library(pracma)
library(devtools)
library(Rcpp)
library(Matrix)

# adjust this path
root_path = "/Users/ataeb/Dropbox/Poset code/"

path = paste(root_path,"clustering_synthetic/subroutines",sep = "")
## source all relevant files
setwd(path)
files.sources = list.files()
sapply(files.sources, source)



## setup parameters
p <- 20 # number of items or elements
alpha <- 0.3 # stability threshold
max_iter <- 50 # number of iterations to compute different metrics
num_bags <- 50 # number of bags
FD_max<- 3 # largest number of false discoveries

## setup different experimental configurations
nvec <- c(40,65,90) # number of samples for every element
dvec <- c(3,3.5,4)#controls the distance among the partitions
# true partition
true_partition <- list()
true_partition <- list(c(1,2,3,4,5),6:10)
for (i in 11:p){
  true_partition <- append(true_partition,i)
}

## initialize vectors of true and false discoveries for stability approach and non-subsampling approach
FD_final <- c()
TD_final <- c()
FD_final_vanilla <- c()
TD_final_vanilla <- c()

for (m_iter in 1:length(nvec)){
  n <- nvec[m_iter]
  for (d_iter in 1:length(dvec)){
    print(d_iter)
    d <- dvec[d_iter]
    mu_vec <- linspace(0, d,length(true_partition))
    
    
    FD_vec <- c()
    TD_vec <- c()
    TD_vec_vanilla<-c()
    FD_vec_vanilla<-c()
    
    
    for (iter in 1:max_iter){
      
      # create data
      data <- list()
      for (i in 1:5){
        data <-  append(data,list(mvrnorm(m,rbind(mu_vec[1],0), 1/4*diag(2))))
      }
      for (i in 6:10){
        data <-   append(data,list(mvrnorm(m,rbind(mu_vec[2],0), 1/4*diag(2))))
      }  
      for (i in 11:p){
        data <-   append(data,list(mvrnorm(m,rbind(mu_vec[i-8],0), 1/4*diag(2))))
      }  
      
      ## find the amount to threshold weight estimates from BTL model based on false discovery bound
      k_param <- choose_threshold_FD_bound(data,FD_max)
      
      ## find all the subsampling estimates
      partition_bags <- subsampling_estimates(data,k_param)  
      
      ## stable estimate
      out_partition<- find_stable_clustering(partition_bags)
      
      
      
      TD_vec <- append(TD_vec,rho_compute(out_partition,true_partition))
      FD_vec <- append(FD_vec,rho_compute(out_partition,out_partition)-rho_compute(out_partition,true_partition))
      
      # the vanilla approach without subsampling
      full_data_partition<- vanilla_clustering_cv(data)
      TD_vec_vanilla <- append(TD_vec_vanilla,rho_compute(full_data_partition,true_partition))
      FD_vec_vanilla <- append(FD_vec_vanilla,rho_compute(full_data_partition,full_data_partition)-rho_compute(full_data_partition,true_partition))
      
    }
    
    TD_final <- append(TD_final,mean(TD_vec))
    FD_final <- append(FD_final,mean(FD_vec))
    TD_final_vanilla <- append(TD_final_vanilla,mean(TD_vec_vanilla))
    FD_final_vanilla <- append(FD_final_vanilla,mean(FD_vec_vanilla))
    print(TD_final)
    print(FD_final)
    print(TD_final_vanilla)
    print(FD_final_vanilla)
    
  }
  
}

save.image(paste(root_path,"synthetic_results_files_and_plotting/clustering_synthetic.RData",sep = ""))









