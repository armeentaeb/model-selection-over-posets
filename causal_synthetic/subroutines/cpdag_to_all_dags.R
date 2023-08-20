## this  function takes a CPDAG and computes all the DAGs entailed by it
## input: CPDAG x_current
## output: list all_dags_x_current of all the DAGs entailed by x_current
cpdag_to_all_dags <- function(x_current){
  g2 <- as((x_current), "graphNEL")
  adj<- (as(pdag2dag(g2, keepVstruct=TRUE)$graph,"matrix"))
  Z<-equivalentDAGs(pcalg2dagitty(t(adj), 1:p,type = "dag"))
  all_dags_x_current <- list()
  for (i in 1:length(Z)){
    all_dags_x_current  <- append(all_dags_x_current ,list(dagitty_to_adjmatrix(Z[[i]])))
  }
  return(all_dags_x_current)
}