set.seed(42)

library(MASS)
library(pcalg)
library(matrixcalc)
library(pracma)
library(devtools)
library(Rcpp)
library(Matrix)
library(igraph)

# adjust this path
# adjust this path
root_path = "/Users/ataeb/Dropbox/Poset code/"

path = paste(root_path,"tennis_analysis/subroutines/",sep = "")
## source all relevant files
setwd(path)
files.sources = list.files()
sapply(files.sources, source)


## creating the tennis data
p<- 6



tennis_data = list()


## taking top 6 rapid chess without nepo
tennis_data = list()

tennis_data[[1]] <- list()
## Djokovic, nadal, Federer, Murray, Wawrinka, Berdych

## create the pairwise comparison data
tennis_data[[2]] <- list()
tennis_data[[2]][[1]] <-  c(rep(1, 29),rep(0,30))
tennis_data[[3]] <- list()
tennis_data[[3]][[2]] <- c(rep(1, 16),rep(0,24))
tennis_data[[3]][[1]] <- c(rep(1, 23),rep(0,27))
tennis_data[[4]] <- list()
tennis_data[[4]][[3]] <- c(rep(1, 11),rep(0,14))
tennis_data[[4]][[1]] <- c(rep(1,11),rep(0,25))
tennis_data[[4]][[2]] <- c(rep(1, 7),rep(0,17))

tennis_data[[5]] <- list()
tennis_data[[5]][[4]] <- c(rep(1, 9),rep(0,13))
tennis_data[[5]][[3]] <-  c(rep(1, 3),rep(0,23))
tennis_data[[5]][[1]] <-  c(rep(1, 6),rep(0,20))
tennis_data[[5]][[2]] <- c(rep(1, 3),rep(0,19))


tennis_data[[6]] <- list()
tennis_data[[6]][[5]] <- c(rep(1, 5),rep(0,11))
tennis_data[[6]][[4]] <-  c(rep(1, 6),rep(0,11))
tennis_data[[6]][[3]] <-  c(rep(1, 6),rep(0,20))
tennis_data[[6]][[2]] <- c(rep(1, 4),rep(0,20))
tennis_data[[6]][[1]] <- c(rep(1, 3),rep(0,25))

# make sure it is symmetric
for (j in 1:as.integer(p-1)){
  for (i in as.integer(j+1):p){
    if(length(tennis_data[[i]][[j]]) > 0){tennis_data[[j]][[i]] <- 1-tennis_data[[i]][[j]]}
    else{tennis_data[[j]][[i]] <- list()}
  }
}

data <- tennis_data

FD_max <- 3
FD_bound <- 0
alpha <- 0.3
num_bags <- 50

## find the amount to threshold weight estimates from BTL model 
thresh <- choose_threshold_FD_bound_partial_ranking(data,FD_max)

## find all the subsampling estimates
avg_Adj <- subsampling_estimates_partial_ranking(data,thresh)

## stable estimate
output<- find_stable_partial_ranking(avg_Adj)









