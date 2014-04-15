costFunction <- function(X, y, theta){
  
  m=length(y)
  z <- (X %*% theta)
  h <- sigmoid(z)
  
  J = -1/(m) * sum(t(y) %*% log(h) + (1-t(y)) %*% log(1-h));
  grad = 1/m * (t(X) %*% (h - y))
  
  output <- list(J, grad)
  return(J)
}
