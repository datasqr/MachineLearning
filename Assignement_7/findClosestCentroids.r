findClosestCentroids <- function(X, centroids){
  
  #%FINDCLOSESTCENTROIDS computes the centroid memberships for every example
  #%   idx = FINDCLOSESTCENTROIDS (X, centroids) returns the closest centroids
  #%   in idx for a dataset X where each row is a single example. idx = m x 1 
  #%   vector of centroid assignments (i.e. each entry in range [1..K])
  #%
  
  #% Set K
  K = nrow(centroids)
  
  #% You need to return the following variables correctly.
  idx = matrix(0, nrow = nrow(X), ncol = 1)
  
  #% ====================== YOUR CODE HERE ======================
  #  % Instructions: Go over every example, find its closest centroid, and store
  #%               the index inside idx at the appropriate location.
  #%               Concretely, idx(i) should contain the index of the centroid
  #%               closest to example i. Hence, it should be a value in the 
  #%               range 1..K
  #%
  #% Note: You can use a for-loop over the examples to compute this.
  #%
  #% centroids = [3, 2]
  #% X = [300, 2]
  
  for(i in 1:nrow(X)){
    deltas = matrix(0, nrow = K, ncol = 1)
    x = (X[i,])
    for(j in 1:K){
      k = (centroids[j,])
      delta = x - k
      delta = as.matrix(delta, byrow = F)
      deltas[j] = t(delta) %*% delta
    }  
    idx[i] = which.min(deltas[,1])
 }
return(idx)
}

