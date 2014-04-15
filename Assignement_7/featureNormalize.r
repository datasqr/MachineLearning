featureNormalize <- function(X){
# %FEATURENORMALIZE Normalizes the features in X 
#%   FEATURENORMALIZE(X) returns a normalized version of X where
#%   the mean value of each feature is 0 and the standard deviation
#%   is 1. This is often a good preprocessing step to do when
#%   working with learning algorithms.

#  mu = mean(X);
#  X_norm = bsxfun(@minus, X, mu);
  
#  sigma = std(X_norm);
#  X_norm = bsxfun(@rdivide, X_norm, sigma);
  
#  X_norm = apply(X, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)));
  
#  muColumn <- apply(X, 2, mean)
  
#  X_mean <- ((X) - muColumn)
  
#  sdColumn <- apply(X, 2, sd)
  
#  X_norm <- sweep(X_mean,MARGIN=1,sdColumn,`/`)

  X_norm <- scale(X, center = T, scale = T)
  
# check column means and variances of centered and scaled matrix
# apply(scale(X),2,mean)

# apply(scale(X),2,var)
  
  return(X_norm)
}

  
