matequal <- function(x, y){
is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)}