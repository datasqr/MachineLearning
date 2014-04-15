mapFeature <- function(X1, X2){
  #% MAPFEATURE Feature mapping funcion to polynomial features
  #%
  #%   MAPFEATURE(X1, X2) maps the two input features
  #%   to quadratic features used in the regularization exercise.
  #%
  #%   Returns a new feature array with more features, comprising of 
  #%   X1, X2, X1.^2, X2.^2, X1*X2, X1*X2.^2, etc..
  #%
  #%   Inputs X1, X2 must be the same size
  #%
  
  rcF <- as.vector(dim(X1))
  nrF <- rcF[1]
  ncF <- rcF[2]
  
  Xout1 <- matrix(1, nrow = nrF, ncol = ncF)
  
  Xout = list()
  degree = 6;
  for(i in 1:degree){
    for(j in 0:i){
      Xout1 = ((X1 ^ (i-j)) * (X2 ^ j))
      Xout <- cbind(Xout, Xout1)
    }
  }
  return(Xout)
}
