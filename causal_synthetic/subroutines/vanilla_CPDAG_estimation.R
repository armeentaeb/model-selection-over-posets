## this function computes cross-validated CPDAG estimated from GES
## input: data
## output: cross-validated CPDAG


vanilla_CPDAG_estimation <- function(data){
  
  # choice of regularization parameter that we sweep over
  lambda_scale <- linspace(0.1,2,50)
  
  # creating a 70-30 train and validation split
  v<-sample(1:n, n, replace = FALSE, prob = NULL)
  data_train <- t(data[,as.matrix(v[1:as.integer(n*7/10)])])
  data_test <- t(data[,as.matrix(v[1+as.integer(n*3/10):as.integer(n-1)])])
  Sigma_train <- cov(data_train)
  Sigma_test <- cov(data_test)
  validation_log_likelihood <- c()
  
  for (lam_iter in 1:length(lambda_scale)){
    
    # perform GES
    score <- new("GaussL0penIntScore", data_train,lambda = lambda_scale[lam_iter]*log(nrow((data_train))))
    ges.fit <- ges(score);  A1 <- ges.fit$essgraph
    M = matrix(0,p,p)
    for (i in 1:p){M[i,A1[['.in.edges']][[names(A1$.in.edges)[i]]]]<-1}
    
    # find the associated adjacency matrix Omega and noise variance matrix Omega
    B <- matrix(0,p,p)
    Omega <- diag(p)
    for (j in 1:p){
      ind <- which(M[j,]!=0)
      if (length(ind)==0)
      {
        Omega[j,j] <- 1/(n*7/10)*sum((data_train[,j])^2)
      }else{
        b <- solve(t(data_train[,ind])%*%data_train[,ind])%*%(t(data_train[,ind])%*%data_train[,j]); B[j,ind]<-b
        Omega[j,j] <- 1/(n*7/10)*sum((data_train[,j]-data_train%*%Reshape(B[j,],p,1))^2)#Sigma_train[j,j] - Sigma_train[j,ind]%*%solve(Sigma_train[ind,ind])%*%Sigma_train[ind,j]
      }
    }
    
    # compute precision matrix entailed by estimated causal model
    precision_est <- t(diag(p)-B)%*%solve((Omega))%*%(diag(p)-B)
    
    # compute negative log-likelihood of causal model on test data
    validation_log_likelihood <- append(validation_log_likelihood,-log(det(precision_est))+sum(diag(precision_est%*%Sigma_test)))
  }
  
  # ue the regularization parameter that yields smalles negative log-likelihood and refit to entire data with this parameter
  ind <- which.min(validation_log_likelihood)
  score <- new("GaussL0penIntScore", t(data),lambda = lambda_scale[ind]*log(nrow(t(data))))
  ges.fit <- ges(score);  A1 <- ges.fit$essgraph
  M = matrix(0,p,p)
  for (i in 1:p){M[i,A1[['.in.edges']][[names(A1$.in.edges)[i]]]]<-1}
  vanilla_CPDAG <- t(M)
  
  return(vanilla_CPDAG) 
}
  