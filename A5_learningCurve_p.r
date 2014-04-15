learningCurve_p <- function(X, y, Xval, yval, lambda) {

#%LEARNINGCURVE Generates the train and cross validation set errors needed 
#%to plot a learning curve
#%   [error_train, error_val] = ...
#%       LEARNINGCURVE(X, y, Xval, yval, lambda) returns the train and
#%       cross validation set errors for a learning curve. In particular, 
#%       it returns two vectors of the same length - error_train and 
#%       error_val. Then, error_train(i) contains the training error for
#%       i examples (and similarly for error_val(i)).
#%
#%   In this function, you will compute the train and test errors for
#%   dataset sizes from 1 up to m. In practice, when working with larger
#%   datasets, you might want to do this in larger intervals.
#%

#% Number of training examples

n_iter = nrow(X.ini);

#% You need to return these values correctly
#error_train = zeros(m, 1);
#error_val   = zeros(m, 1);

#% ====================== YOUR CODE HERE ======================
#  % Instructions: Fill in this function to return training errors in 
#%               error_train and the cross validation errors in error_val. 
#%               i.e., error_train(i) and 
#%               error_val(i) should give you the errors
#%               obtained after training on i examples.
#%
#% Note: You should evaluate the training error on the first i training
#%       examples (i.e., X(1:i, :) and y(1:i)).
#%
#%       For the cross-validation error, you should instead evaluate on
#%       the _entire_ cross validation set (Xval and yval).
#%
#% Note: If you are using your cost function (linearRegCostFunction)
#  %       to compute the training and cross validation error, you should 
#%       call the function with the lambda argument set to 0. 
#%       Do note that you will still need to use lambda when running
#%       the training to obtain the theta parameters.
#%
#% Hint: You can loop over the examples with the following:
#  %
#%       for i = 1:m
#%           % Compute train/cross validation errors using training examples 
#%           % X(1:i, :) and y(1:i), storing the result in 
#%           % error_train(i) and error_val(i)
#%           ....
#%           
#%       end
#%

m <- nrow(X.ini)

error_train <- matrix(rep(0, m),  nrow = m, ncol = 1)
error_val <- matrix(rep(0, m),  nrow = m, ncol = 1)

X1 <- X
y1 <- y

  for(i in 1:n_iter){  
  # careful with function trainLinearRegJ_t_p or trainLinearRegJ_t
    theta <- trainLinearRegJ_t_p(X1[1:i,], as.matrix(y1[1:i,]), 0);
    theta <- as.matrix(theta$par)
    error_train[i,] <- linearRegCostFunctionJ_t(X1[1:i,], as.matrix(y1[1:i,]), theta, 0);
    error_val[i,] <- linearRegCostFunctionJ_t(Xval, yval, theta, 0);
  
  }

# t(X[1:i,]) transponse function should be implemented because statement X[1:i,] returns 
# numeric value with one columne. In this situation there is an error and function 
# linearRegCostFunctionJ can not be executed

output <- list(error_train, error_val)
return(output)

} 

