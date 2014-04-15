Cost_Function <- function(X, y, theta){
  predictions = X %*% theta;
  sqrErrors = (predictions-y)^2;
  J = 1/(2*m) * sum(sqrErrors);
  return(J)
}
