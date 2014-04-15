GradientDescent <- function(X, y, theta, alpha, iterations){
  m=length(y)
  num_iters = iterations;
  J_history = matrix(rep(0, num_iters),  nrow = num_iters, ncol = 1);
  for (i in 1:num_iters) {
    theta = theta - alpha * (1/m) * t(t((X %*% theta) - y) %*% X)
    J_history[i,1] = Cost_Function(X, y, theta)
  }
  output <- list(theta, J_history)
#  return(theta)
  return(output)
}
  
  