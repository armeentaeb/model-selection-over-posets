
## Load packages
library(pcalg)
library(matrixcalc)
library(pracma)
library(devtools)
library(Rcpp)
library(igraph)
library(dagitty)

# adjust this path
root_path = "/Users/ataeb/Dropbox/Poset code 2/"

path = paste(root_path,"subroutines",sep = "")

## source all relevant files
setwd(path)
files.sources = list.files()
sapply(files.sources, source)


# generate random data 
p <- 10
nvec <- c(1000,1200,1400,1600,1800)
avg_edge_vec <-c(0.8,1.5) #divide this by (p-1) to get the probabilities v in the paper. 
eps_noise_vec <- c(1/4)
max_undir <- 1
max_iter <- 50
num_bags = 50
alpha = 0.3
FD_max <- 2 



FD_final <- c()
TD_final <- c()
TD_vanilla_final <- c()
FD_vanilla_final <-c()


# search over different denseness of graphs
for (prob_iter in 1:length(avg_edge_vec)){
  avg_edge <- avg_edge_vec[prob_iter]
  num_undir <-  max_undir+1
  
  # find the underlying causal model
  diameter_small <- FALSE
  adj = matrix(0,p,p)
  
  ## set seed
  set.seed(42)
  
    obj <- randDAG(p, avg_edge)
    Bstar<-as(obj, "matrix")
    Bstar <- t(Bstar)
    Bstar[which(abs(Bstar)>=10^(-3))]<-runif(length(which(abs(Bstar)>=10^(-3))),0.5,0.7)  
    adj = matrix(0,p,p)
    adj[which(abs(Bstar)>10^(-3))] <- 1
    M <- adj; M <- M+t(M); M[which(M==2)]<-1; 
    diameter_small <- (diameter(graph_from_adjacency_matrix(M),directed = FALSE)<=2)

  Z<-equivalentDAGs(pcalg2dagitty((adj), 1:p,type = "dag"))
  all_dags_star <- list()
  for (i in 1:length(Z)){
    all_dags_star <- append(all_dags_star,list(dagitty_to_adjmatrix(Z[[i]])))
  }
  
  g2 <- as(t(adj), "graphNEL") ## convert to graph
  cpdag2 <- dag2cpdag(g2)
  true_CPDAG <- as(cpdag2,"matrix")
  
  
  
  for (eps_iter in 1:length(eps_noise_vec)){
    
    # search over different sample sizes
    for (niter in 1:length(nvec)){
      print(niter)
      n<- nvec[niter]
      FD_vec <- c();TD_vec <- c();FD_vanilla_vec <- c();TD_vanilla_vec <- c()
      
      
      eps_data <- matrix( rnorm(p*n,mean=0,sd=eps_noise_vec[eps_iter]), p, n) 
      data <- solve(diag(p)-Bstar)%*% eps_data
      
      ## find the regularization lambda in the GES algorithm so that stability based procedure has desired control on false discoveries
      lambda<- choose_regularization_FD_bound_causal(data,FD_max)
      
      
      for (iter in 1:max_iter){
        # create data
        eps_data <- matrix( rnorm(p*n,mean=0,sd=eps_noise_vec[eps_iter]), p, n) 
        data <- solve(diag(p)-Bstar)%*% eps_data
        
        ## find all the subsampling estimates
        CPDAG_bags <- subsampling_estimates_causal(data,lambda)  
        
        ## stable estimate
        stable_CPDAG<- find_stable_CPDAG(CPDAG_bags)
        
        # compute true and false discoveries associated with stable estimate
        TD<- compute_rho(stable_CPDAG,true_CPDAG)
        FD_vec <- append(FD_vec,c(rank(stable_CPDAG)-TD))
        TD_vec <- append(TD_vec,c(TD))
        
        # CPDAG without subsampling using cross validation to choose lambda
        vanilla_CPDAG <- vanilla_CPDAG_estimation(data)
        
        
        # compute true and false discoveries associated with estimate without subsampling
        TD<- compute_rho(vanilla_CPDAG,true_CPDAG)
        FD_vanilla_vec <- append(FD_vanilla_vec,c(rank(vanilla_CPDAG)- TD))
        TD_vanilla_vec <- append(TD_vanilla_vec,c(TD))
        
      }
      
      FD_final <- append(FD_final,mean(FD_vec))
      TD_final <- append(TD_final,mean(TD_vec))
      plot(FD_final)
      plot(TD_final)
      FD_vanilla_final <- append(FD_vanilla_final,mean(FD_vanilla_vec))
      TD_vanilla_final <- append(TD_vanilla_final,mean(TD_vanilla_vec))

    }
  }
}

save.image(paste(root_path,"synthetic_results_files_and_plotting/causal_synthetic.RData",sep = ""))
