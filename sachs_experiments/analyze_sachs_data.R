set.seed(42)

# install appropriate libraries
library(MASS)
library(pcalg)
library(matrixcalc)
library(pracma)
library(devtools)
library(Rcpp)
library(igraph)
library(dagitty)
library("readxl")


# adjust this path
root_path = "/Users/ataeb/Dropbox/Poset code/"

path = paste(root_path,"causal_synthetic/subroutines",sep = "")


## source all relevant files
setwd(path)
files.sources = list.files()
sapply(files.sources, source)


## got the data from here: https://web.stanford.edu/group/wonglab/JASAFlowCytometryData/JASA%20Flow%20Cytometry%20Data.htm



p <- 11
all_files <- list.files(path="/Users/ataeb/Dropbox/Poset code/sachs_experiments/Sachs_data", pattern=".xls", all.files=TRUE,full.names=TRUE)

# take data from the observational environment
data <- matrix(0,1,p)
data <- rbind(data,as.matrix(read_excel(all_files[2])))


n<- nrow(data)
num_bags <- 50
FD_max <- 2
alpha<-0.3
data <- t(data)

# choose the regularization 
lambda<- choose_regularization_FD_bound_causal(data,FD_max)

## find all the subsampling estimates
CPDAG_bags <- subsampling_estimates_causal(data,lambda)  

## stable estimate
stable_CPDAG<- find_stable_CPDAG(CPDAG_bags)


colnames(stable_CPDAG)<- rownames(data)
rownames(stable_CPDAG)<- rownames(data)























# 
# FD <- 0
# lambdav <- 10
# alpha <- 0.7
# while(FD <= FD_max){
#   estimate_bags <- list()
#   for (bag_iter in 1:B){
#     v<-sample(1:n, n, replace = FALSE, prob = NULL)
#     data1 <- (data[as.matrix(v[1:as.integer(n/2)]),])
#     data2 <- (data[as.matrix(v[1+as.integer(n/2):as.integer(n-1)]),])
#     score1 <- new("GaussL0penIntScore", data1,lambda = lambdav*log(nrow(data1)))
#     score2 <- new("GaussL0penIntScore", data2,lambda = lambdav*log(nrow(data2)))
#     
#     ges.fit1 <- ges(score1);  A1 <- ges.fit1$essgraph
#     M = matrix(0,p,p)
#     for (i in 1:p){M[i,A1[['.in.edges']][[names(A1$.in.edges)[i]]]]<-1}
#     estimate_bags <- append(estimate_bags,list(t(M)))
#     
#     
#     ges.fit2 <- ges(score2);  A2 <- ges.fit2$essgraph
#     M = matrix(0,p,p)
#     for (i in 1:p){M[i,A2[['.in.edges']][[names(A2$.in.edges)[i]]]]<-1}
#     estimate_bags <- append(estimate_bags,list(t(M)))
#   }
#   FD <- FD_bound_computation(estimate_bags)
#   print(FD)
#   lambdav <- lambdav*8/10
#   
# }
# 
# # enumerate all DAGs in the estimated cpdags across bags
# all_dags_estimates <- list()
# for (l in 1:as.integer(2*B)){
#   Z<-equivalentDAGs(pcalg2dagitty(t(estimate_bags[l][[1]]), 1:p,type = "dag"))
#   temp <- list()
#   for (i in 1:length(Z)){
#     temp <- append(temp,list(dagitty_to_adjmatrix(Z[[i]])))
#   }
#   all_dags_estimates <- append(all_dags_estimates,list(temp)) 
# }
# 
# 
# 
# # ensure stability
# is_stable <- TRUE
# # current cpdag is the empty one
# x_current <- matrix(0,p,p)
# 
# current_rho_bag <- matrix(0,1,2*B)
# 
# # list of current meets
# current_meets <- list()
# for (i in 1:as.integer(2*B)){
#   current_meets <- append(current_meets,list(matrix(0,p,p)))
# }
# 
# # find stable edges
# stable_edges <- matrix(0,p,p)
# for (l in 1:as.integer(2*B)){
#   M <- estimate_bags[[l]]
#   M <- M + t(M); M[which(M == 2)]<-1
#   stable_edges <- stable_edges + M
# }
# ind <- which(stable_edges/(2*B) >= alpha)
# stable_edges[ind] <-1; stable_edges[setdiff(1:p^2,ind)] <-0
# 
# # all dags across all bags
# all_dags_bags <- list()
# for (l in 1:as.integer(2*B)){
#   all_dags_bags <- append(all_dags_bags,list(cpdag_to_all_dags(estimate_bags[[l]])))
# }
# 
# while(is_stable){
#   print(x_current)
#   # find all neighbors that do not have cycles
#   all_neighb_temp <- all_possible_neighbors(x_current,stable_edges)
#   all_neighb <- list()
#   for (i in 1:length(all_neighb_temp)){
#     M<- all_neighb_temp[[i]];M <- M+t(M); M[which(M==2)]<-1
#     if (diameter(graph_from_adjacency_matrix(M),directed = FALSE)<=2){
#       if (sum(sum(hadamard.prod(M,stable_edges))) == sum(sum(M)) && is_polytree(all_neighb_temp[[i]])){
#         all_neighb <- append(all_neighb,list(all_neighb_temp[[i]]))
#       }
#     }
#   }
#   
#   neighbor_meet <- list()
#   stable_neighbors_exists = c()
#   
#   
#   # compute current value of rho function for every bag
#   rho_bag <- c()
#   #all_dags_x_current <- cpdag_to_all_dags(x_current)
#   
#   for (l in 1:as.integer(2*B)){
#     #rho_bag <- append(rho_bag,sum(sum(all_dags_x_current[[1]]))- calculate_false_discoveries(all_dags_x_current,all_dags_bags[[l]]))
#     rho_bag <- append(rho_bag,compute_rho(x_current,estimate_bags[[l]]))
#   }
#   
#   
#   if (length(all_neighb)==0){ is_stable <- FALSE}
#   else{
#     stab <- matrix(0,1,length(all_neighb))
#     
#     for (i in 1:length(all_neighb)){
#       s <- 0
#       for (l in 1:as.integer(2*B)){
#         #if (stable_neighbors_exists[l]==1){
#         #temp<- sum(sum(all_dags_consider[[1]]))- calculate_false_discoveries(all_dags_consider,all_dags_bags[[l]])
#         temp<- compute_rho(all_neighb[[i]],estimate_bags[[l]])
#         s<- s+ temp-rho_bag[l]
#         if (temp-rho_bag[l] < 0 || temp-rho_bag[l]>=2 ){"ERROR"}
#       }
#       stab[i] <- s/(2*B)
#       
#     }
#     if (max(stab) < alpha){ is_stable <- FALSE}else{
#       x_current <- all_neighb[[which.max(stab)]]
#     }
#   }
# }






