install.packages("expm")
library(expm)
#%% =========== Part 1: Regularized Logistic Regression ============
#  %  In this part, you are given a dataset with data points that are not
#%  linearly separable. However, you would still like to use logistic 
#%  regression to classify the data points. 
#%
#%  To do so, you introduce more features to use -- in particular, you add
#%  polynomial features to our data matrix (similar to polynomial
#                                           %  regression).


data2 <- read.table("ex2data2.txt", sep =",")

plot(data2$V1, data2$V2, pch=19, 
     col=cut(data2$V3, breaks=c(2)))

data2.m <- as.matrix(data2)
dim(data2.m)

X1 <- as.matrix(data2.m[, c(1)])
X2 <- as.matrix(data2.m[, c(2)])
y <- (data2.m[, 3])


X <- mapFeature(X1,X2)

rc <- as.vector(dim(X))
nr <- rc[1]
nc <- rc[2]

#% Initialize fitting parameters
initial_theta <- matrix(0, nrow = nc, ncol = 1)

#% Set regularization parameter lambda to 1
lambda = 1;

costFunctionReg(X, y, initial_theta, lambda)

#% Initialize fitting parameters
initial_theta <- matrix(0, nrow = nc, ncol = 1)

#% Optimize

initial_theta <- rep(0,ncol(X))
wrapper <- function(initial_theta) costFunctionReg(initial_theta, X=X, y=y, lambda)
op <- optim(initial_theta, wrapper, method = "L-BFGS-B")
# method = "L-BFGS-B" it did not work for default method

theta <- as.matrix(op[[1]])

#% Compute accuracy on our training set
p = predict(theta, X)

mean(p)

# PLOT
plot_x <- c(min(X[,2])-2, max(X[,2])-2)
plot_y <- (-1/theta[3,])*(theta[2,]*plot_x + theta[1,]);

plot(plot_x, plot_y)


