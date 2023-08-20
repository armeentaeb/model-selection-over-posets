# adjust this path
root_path = "/Users/ataeb/Dropbox/Poset code/"

path = paste(root_path,"synthetic_results_files_and_plotting",sep = "")


setwd(path)
load("ranking_synthetic.RData")
FD_final_ranking <- FD_final
TD_final_ranking <- TD_final
FD_final_vanilla_ranking <- FD_vanilla_final
TD_final_vanilla_ranking <- TD_vanilla_final


load("clustering_synthetic.RData")
FD_final_clustering <- FD_final
TD_final_clustering <- TD_final
FD_final_vanilla_clustering <- FD_final_vanilla
TD_final_vanilla_clustering <- TD_final_vanilla

load("causal_synthetic.RData")
FD_final_causal <- FD_final
TD_final_causal <- TD_final
FD_final_vanilla_causal <- FD_vanilla_final
TD_final_vanilla_causal <- TD_vanilla_final


par(mfrow=c(1,3))
plot(FD_final_ranking, TD_final_ranking,main="Total ranking",col = "red",xlim=c(0,12),ylim = c(0,15),xaxs="i", yaxs="i",xlab = "size of false discoveries", ylab = "size of true discoveries")
par(new = TRUE)
plot(FD_final_vanilla_ranking,TD_final_vanilla_ranking,pch = 0,col = "blue",xlim=c(0,12),ylim = c(0,15),xaxs="i", yaxs="i",xlab = "size of false discoveries", ylab = "size of true discoveries")
for (i in 1:length(FD_final)){
  segments(x0 = FD_final_ranking[i],y0 = TD_final_ranking[i], x1 = FD_final_vanilla_ranking[i], y1 = TD_final_vanilla_ranking[i],col = "black",lty=2)
}
segments(x0 = 3,y0 = 0,x1 = 3, y1 = 15,col = "green",lty=1)

legend(x = "bottomright", legend = c("stability selection","w/o subsampling","desired FD level"),pch = c(1,0,NA),lty('','',3),lwd = c('','',2),col = c(
  "red","blue","green"))


plot(FD_final_clustering, TD_final_clustering,main = "Clustering",col = "red",xlim=c(0,12),ylim = c(0,9),xaxs="i", yaxs="i",xlab = "size of false discoveries", ylab = "size of true discoveries")
par(new = TRUE)
plot(FD_final_vanilla_clustering,TD_final_vanilla_clustering,pch = 0,col = "blue",xlim=c(0,12),ylim = c(0,9),xaxs="i", yaxs="i",xlab = "size of false discoveries", ylab = "size of true discoveries")
for (i in 1:length(FD_final)){
  segments(x0 = FD_final_clustering[i],y0 = TD_final_clustering[i], x1 = FD_final_vanilla_clustering[i], y1 = TD_final_vanilla_clustering[i],col = "black",lty=2)
}
segments(x0 = 3,y0 = 0,x1 = 3, y1 = 9,col = "green",lty=1)

legend(x = "bottomright", legend = c("stability selection","w/o subsampling","desired FD level"),pch = c(1,0,NA),lty('','',3),lwd = c('','',2),col = c(
  "red","blue","green"))




plot(FD_final_causal, TD_final_causal,,main = "Causal structure learning",col = "red",xlim=c(0,14),ylim = c(0,9),xaxs="i", yaxs="i",xlab = "size of false discoveries", ylab = "size of true discoveries")
par(new=TRUE)
plot(FD_final_vanilla_causal,TD_final_vanilla_causal,pch = 0,col = "blue",xlim=c(0,14),ylim = c(0,9),xaxs="i", yaxs="i",xlab = "size of false discoveries", ylab = "size of true discoveries")
par(new=TRUE)
for (i in 1:length(FD_final)){
  segments(x0 = FD_final_causal[i],y0 = TD_final_causal[i], x1 = FD_final_vanilla_causal[i], y1 = TD_final_vanilla_causal[i],col = "black",lty=2)
  par(new=TRUE)
}
segments(x0 = 2,y0 = 0,x1 = 2, y1 = 9,col = "green",lty=1)

legend(x = "bottomright", legend = c("stability selection","w/o subsampling","desired FD level"),pch = c(1,0,NA),lty('','',3),lwd = c('','',2),col = c(
  "red","blue","green"))
