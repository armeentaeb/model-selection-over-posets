## this function performs vanilla k-means clustering (without subsampling) with optimal number of clusters chosen based on sil score 
## input: data
## output: partition obtained from validated k-means

vanilla_clustering_cv <- function(data){

# form data to be used for clustering by finding mean associated with each feature
data_sub <- c()
for (i in 1:p){
  data_sub <- rbind(data_sub,colMeans(data[[i]]))
}

sil_score_mean <- c()
max_cluster<- p-rho_compute(true_partition,true_partition)
# vary the number of clusters and compute sil score
for (num_clusters in 2:max_cluster){
  sil_score <- c()
  (cl <- kmeans(data_sub, num_clusters))
  for (i in 1:p){
    #compute a_i
    ind <- which(cl$cluster == cl$cluster[i])
    ind <- setdiff(ind,i)
    t <- 0
    for (j in 1:length(ind)){
      t <- t + norm(as.matrix(data_sub[i,]-data_sub[ind[j],]))
    }
    if (length(ind) == 0){
      sil_score <- append(sil_score,1)
      
    }
    else{
      ai <- 1/(length(ind))*t
      
      bi_vec <- c()
      for (k in setdiff(1:num_clusters,cl$cluster[i])){
        ind <- which(cl$cluster == k)
        ind <- setdiff(ind,i)
        t <- 0
        for (j in 1:length(ind)){
          t <- t + norm(as.matrix(data_sub[i,]-data_sub[ind[j],]))
        }
        bi_vec <- append(bi_vec,1/(length(ind))*t)
      }
      bi <- min(bi_vec)
      sil_score <- append(sil_score,(bi-ai)/max(bi,ai))
    }
  }
  
  sil_score_mean <- append(sil_score_mean,mean(sil_score))
}

# choose optimal cluster number 
opt_cluster <- which.max(sil_score_mean)

# rerun k-means with optimal cluster
(cl <- kmeans(data_sub, opt_cluster))

# obtain associated partition
vector <- cl$cluster
full_data_partition<- list()
for (i in 1:opt_cluster){
  ind <- which(vector == i) 
  full_data_partition <- append(full_data_partition,list(ind))
}
return(full_data_partition)
}