## this function computes CPDAG estimates from subsamplings
## input: observational data and choice of regularization penalty in lambda in GES algorithm
## output: list containing CPDAG estimates from subsampling

subsampling_estimates_causal <- function(data,lambdav){
  
  
  estimate_bags <-  list()
  for (i in 1:num_bags){
    # create complementary subsamples
    v<-sample(1:n, n, replace = FALSE, prob = NULL)
    data1 <- t(data[,as.matrix(v[1:as.integer(n/2)])])
    data2 <- t(data[,as.matrix(v[1+as.integer(n/2):as.integer(n-1)])])
    

    # obtain CPDAG from first complementary data
    score1 <- new("GaussL0penIntScore", data1,lambda = lambdav*log(nrow(data1)))
    ges.fit1 <- ges(score1);  A1 <- ges.fit1$essgraph
    M1 = matrix(0,p,p)
    for (i in 1:p){M1[i,A1[['.in.edges']][[names(A1$.in.edges)[i]]]]<-1}
    estimate_bags <- append(estimate_bags,list(t(M1)))
    
    # obtain CPDAG from second complementary data
    score2 <- new("GaussL0penIntScore", data2,lambda = lambdav*log(nrow(data2)))
    ges.fit1 <- ges(score1);  A1 <- ges.fit1$essgraph
    M1 = matrix(0,p,p)
    for (i in 1:p){M1[i,A1[['.in.edges']][[names(A1$.in.edges)[i]]]]<-1}
    estimate_bags <- append(estimate_bags,list(t(M1)))
    
  }
  
  return(estimate_bags)
  
}