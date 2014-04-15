featureNormalize <- function(X) {
  n=nrow(X)
  mu1=mean(X[,1])
  mu2=mean(X[,2])
  sigma1=sd(X[,1])
  sigma2=sd(X[,2])
  t = matrix(rep(1,n), nrow = nrow(X), ncol = 1)
   
  X_norm.V1 = (X[,1] - (t * mu1)) / (t * sigma1)
  X_norm.V2 = (X[,2] - (t * mu2)) / (t * sigma2)
  X = cbind(X_norm.V1, X_norm.V2)
  mu = cbind(mu1, mu2)
  sigma = cbind(sigma1, sigma2)
  
#  output <- list(X_norm, mu, sigma)
  return(X)
}