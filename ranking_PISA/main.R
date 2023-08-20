set.seed(42)


library(MASS)
library(pcalg)
library(matrixcalc)
library(pracma)
library(devtools)
library(Rcpp)
library(Matrix)
library(igraph)


library(learningtower)
data(school)

# adjust this path
root_path = "/Users/ataeb/Dropbox/Poset code/"

path = paste(root_path,"total_ranking_synthetic/subroutines",sep = "")
setwd(path)
files.sources = list.files()
sapply(files.sources, source)

# load all 2018 data
student_data_2018 <- load_student(2018)


# OECD ranking 2015 reading
ordered_names <- c('CAN','FIN','IRL','EST','KOR','JPN','NOR','NZL','DEU','POL','SVN','NLD','AUS','SWE','DNK','FRA','BEL','PRT','GBR','USA','CHE','LVA','CZE','AUT','ITA','ISL','LUX')
ordered_names<-ordered_names[1:15]

# number of items
p<- length(ordered_names)


data<- list()
for (i in 1:length(ordered_names)){
  temp <- as.matrix(student_data_2018[which(student_data_2018[2] == ordered_names[i]),11])
  data <- append(data,list(temp))
}

pi_0 <- 1:p

## approach based on testing
alpha <- 0.05
X<- list()
for (i in 1:length(ordered_names)){
  temp<- data[[i]]
  ind <- which(!is.na(temp))
  temp <- temp[which(as.matrix(school[ind,10])=="private")]
  X <- append(X,list(temp))
}

pi_testing_greedy<- ranking_based_on_pvalues_Gaussian_greedy(X)
testing_ranking <- ordered_names[pi_testing_greedy]






