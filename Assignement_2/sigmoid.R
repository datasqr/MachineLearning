sigmoid <- function(z){
  g <- 1/(1+exp(-z))
  return(g)
}