install.packages("expm")
library(expm)

data1 <- read.table("ex2data1.txt", sep =",")

colnames(data1) <- c("Population","Profit")

plot(data1$V1, data1$V2, pch=19, 
     col=cut(data1$V3, breaks=c(2)))

data1.m <- as.matrix(data1)
dim(data1.m)

X.ini <- data1.m[, c(1,2)]
y <- (data1.m[, 3])

plot(data1$V1, data1$V2, pch=19, 
     col=cut(data1$V3, breaks=c(2)))

rc <- as.vector(dim(X.ini))
nr <- rc[1]
nc <- rc[2]

ones <- rep(1, nr)
           
X <- cbind(ones, X.ini)
theta <- matrix(0, nrow = nc+1, ncol = 1)

costFunction(X, y, theta)

initial_theta <- rep(0,ncol(X))
wrapper <- function(theta) costFunction(theta, X=X, y=y)
optim(initial_theta, wrapper) #, gr = "L-BFGS-B", control = list(maxit = 400))

#%% =========== Part 1: Regularized Logistic Regression ============
#  %  In this part, you are given a dataset with data points that are not
#%  linearly separable. However, you would still like to use logistic 
#%  regression to classify the data points. 
#%
#%  To do so, you introduce more features to use -- in particular, you add
#%  polynomial features to our data matrix (similar to polynomial
#                                           %  regression).



