## given a set of elements, this function computes its powerset
## input: set of elements s
## output: powerset of s
powerset = function(s){
  len = length(s)
  l = vector(mode="list",length=2^len) ; l[[1]]=numeric()
  counter = 1
  for(x in 1:length(s)){
    for(subset in 1:counter){
      counter=counter+1
      l[[counter]] = c(l[[subset]],s[x])
    }
  }
  return(l)
}