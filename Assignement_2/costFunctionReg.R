costFunctionReg <- function(X, y, theta, lambda){
  
  #% Deal with the theta(1) term, set it to '0'
  
  thetaFilt = theta
  thetaFilt[1] <- 0
  
  m=length(y)
  z <- (X %*% theta)
  h <- sigmoid(z)
  
  J = -1/m * sum(t(y) %*% log(h) + (1-t(y)) %*% log(1-h)) + (lambda/(2*m)) * (t(thetaFilt) %*% thetaFilt)
  grad = 1/m * (t(X) %*% (h - y)) + (lambda/(m) * (theta))
  
  output <- list(J, grad)
  return(J)
}
