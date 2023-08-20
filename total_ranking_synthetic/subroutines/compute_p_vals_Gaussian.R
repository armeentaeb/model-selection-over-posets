### this function generates p-value from a two sample test containing Gaussian data
### input: x,y -- two data vectors
### output: p_val -- p-value

compute_p_vals_Gaussian <- function(x,y){
  
  sp <- sqrt(((length(x)-1)*var(x)+(length(y)-1)*var(x))/(length(x)+length(y)-2))
  t<-(mean(x)-mean(y))/(sp*sqrt(1/length(x)+1/length(y)))
  p_val <- pt(t,length(x)+length(y)-2, lower.tail = FALSE)
  return(p_val)

}