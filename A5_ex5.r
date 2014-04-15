
#install.packages("gpr")
library(gpr)

#%% =========== Part 1: Loading and Visualizing Data =============
#%  We start the exercise by first loading and visualizing the dataset. 
#%  The following code will load the dataset into your environment and plot
#%  the data.
#%

trial <- read.csv("trial.csv", head = T, sep =";")
test <- read.csv("test.csv", head = T, sep =";")
val <- read.csv("val.csv", head = T, sep =";")

trial.m <- as.matrix(trial)
test.m <- as.matrix(test)
val.m <- as.matrix(val)

X.ini <- as.matrix(trial.m[,1])
y <- as.matrix(trial.m[,2])

Xval <- as.matrix(val.m[,1])
yval <- as.matrix(val.m[,2])

Xtest <- as.matrix(test.m[,1])
ytest <- as.matrix(test.m[,2])

m <- nrow(trial.m)

plot(trial.m[,1], trial.m[,2], pch=19, col ="blue")

#theta <- matrix(1, nrow = 2, ncol = 1)

rc <- as.vector(dim(X.ini))
nr <- rc[1]
nc <- rc[2]

ones <- rep(1, nr)
ones_val <- rep(1, nrow(Xval))

X <- cbind(ones, X.ini)

#%% =========== Part 2: Regularized Linear Regression Cost =============
#%  You should now implement the cost function for regularized linear 
#%  regression. 
#%

theta <- matrix(1, nrow = nc+1, ncol = 1)

lambda <- 1.0

linearRegCostFunctionJ(X, y, theta, lambda)

#%% =========== Part 3: Regularized Linear Regression Gradient =============
#%  You should now implement the gradient for regularized linear 
#%  regression.
#%

thetaG <- linearRegCostFunctionG(X, y, theta, lambda)
thetaG

#%% =========== Part 4: Train Linear Regression =============
#%  Once you have implemented the cost and gradient correctly, the
#%  trainLinearReg function will use your cost function to train 
#%  regularized linear regression.
#% 
#%  Write Up Note: The data is non-linear, so this will not give a great 
#%                 fit.
#%

lambda <- 0
X <- cbind(ones, X.ini)

theta <- trainLinearRegJ_t(X, y, lambda);
theta <- as.matrix(theta$par)

plot(trial.m[,1], trial.m[,2], xlim=c(-60, 40), ylim=c(-5, 40), pch=19, col ="blue")
abline(theta[1,], theta[2,], xlim=c(-60, 40), ylim=c(-5, 40), type = "l")

#plot(X.ini, X %*% theta, xlim=c(-60, 40), ylim=c(-5, 40), type = "l")


#%% =========== Part 5: Learning Curve for Linear Regression =============
#  %  Next, you should implement the learningCurve function. 
#%
#%  Write Up Note: Since the model is underfitting the data, we expect to
#%                 see a graph with "high bias" -- slide 8 in ML-advice.pdf 
#%


lambda <- 0;
plot(Xval, yval, pch=19)
X <- cbind(ones, X.ini)
#y <- as.matrix(trial.m[,2])
Xval_ones <- cbind(ones_val, Xval)

error <- learningCurve(X, y, Xval_ones, yval, lambda)

errorTrain <- error[[1]]
errorVal <- error[[2]]

plot(1:m, errorTrain, typ = "l", ylim = c(0,140))
lines(1:m, errorVal, typ = "l")

# =========== Part 6: Feature Mapping for Polynomial Regression =============
#  One solution to this is to use polynomial regression. You should now
#  complete polyFeatures to map each example into its powers
#

p <- 8

X <- X.ini
# X should be a single column matrix
X_poly1 <- polyFeatures(X,p)

X_poly2 <- featureNormalize(X_poly1)

ones_poly <- rep(1, nrow(X_poly2))
X_poly3 <- cbind(ones, X_poly2)

# Map X_poly_test and normalize (using mu and sigma)

X_poly_test1 <- polyFeatures(Xtest,p)
X_poly_test2 <- featureNormalize(X_poly_test1)
ones_poly_test <- rep(1, nrow(X_poly_test2))
X_poly_test3 <- cbind(ones_poly_test, X_poly_test2)

# Map X_poly_val and normalize (using mu and sigma)

X_poly_val1 <- polyFeatures(Xval,p)
X_poly_val2 <- featureNormalize(X_poly_val1)
ones_poly_val <- rep(1, nrow(X_poly_val2))
X_poly_val3 <- cbind(ones_poly_val, X_poly_val2)


#%% =========== Part 7: Learning Curve for Polynomial Regression =============
#  %  Now, you will get to experiment with polynomial regression with multiple
#%  values of lambda. The code below runs polynomial regression with 
#%  lambda = 0. You should try running the code with different values of
#%  lambda to see how the fit and learning curve change.

lambda <- 0
theta_poly <- trainLinearRegJ_t_p(X_poly3, y, lambda)
theta_poly <- as.matrix(theta_poly$par)

plot(trial.m[,1], trial.m[,2], xlim=c(-60, 40), ylim=c(-5, 40), pch=19, col ="blue")
lines(theta_poly, xlim=c(-60, 40), ylim=c(-5, 40), type = "l")

error_poly <- learningCurve_p(X_poly3, y, X_poly_val3, yval, lambda)

errorTrain_poly <- error_poly[[1]]
errorVal_poly <- error_poly[[2]]

plot(1:m, errorTrain_poly, typ = "l", ylim = c(0,140))
lines(1:m, errorVal_poly, typ = "l")


#%% =========== Part 8: Validation for Selecting Lambda =============
#  %  You will now implement validationCurve to test various values of 
#%  lambda on a validation set. You will then use this to select the
#%  "best" lambda value.
#%

valid_curve <- validationCurve(X_poly3, y, X_poly_val3, yval)
lambda_v <- valid_curve[[1]]
errorTrain_fin <- valid_curve[[2]]
errorVal_fin <- valid_curve[[3]]

plot(lambda_v, errorTrain_fin, typ = "l", ylim = c(0,20))
lines(lambda_v, errorVal_fin, typ = "l")


