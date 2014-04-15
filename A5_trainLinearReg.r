trainLinearReg <- function(X, y, lambda){
  
  #%TRAINLINEARREG Trains linear regression given a dataset (X, y) and a
  #%regularization parameter lambda
  #%   [theta] = TRAINLINEARREG (X, y, lambda) trains linear regression using
  #%   the dataset (X, y) and regularization parameter lambda. Returns the
  #%   trained parameters theta.
  #%
  
  # http://cran.r-project.org/web/packages/gpr/gpr.pdf
  
  #% Initialize Theta
  initial_theta <- rep(0,ncol(X))
  
  #% Create "short hand" for the cost function to be minimized
# initial_theta <- rep(1,ncol(X))
  wrapper <- function(initial_theta) linearRegCostFunctionJ_t(initial_theta, X=X, y=y, lambda)
  op <- optim(initial_theta, wrapper, method = "L-BFGS-B")
  
  return(op)
}