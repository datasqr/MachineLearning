data1 <- read.table("ex1data1.txt", sep =",")
colnames(data1) <- c("Population","Profit")

plot(data1$Population, data1$Profit, pch=19, col="blue",
     ylab = "Profit in $10,000s", 
     xlab = "Population of City in 10,000s")

data1.m <- as.matrix(data1)
dim(data1.m)

X.ini <- data1.m[, 1]
y <- data1.m[, 2]

plot(X.ini,y, pch=19, col="blue")

m = length(y.ini); 
ones <- rep(1, m)
zeros <- rep(0, 2)
           
X <- cbind(ones, X.ini)

theta <- matrix(zeros, nrow = 2, ncol = 1)
iterations = 1500
alpha = 0.01

Cost_Function(X, y, theta)

theta <- GradientDescent(X, y, theta, alpha, iterations)[[1]]

predict <- matrix(c(1,3.5), nrow = 2, ncol =1)
t(predict) %*% theta

lines(X.ini, X %*% theta, type = "l", col="red")
abline(lm(y ~ X.ini))

########################################################
# Multi

data2 <- read.table("ex1data2.txt", sep =",")
   
data2.m <- as.matrix(data2)
X <- data2.m[, c(1,2)]
y = data2.m[, 3]
m = length(y); 

X <- featureNormalize(X)

ones <- rep(1, m)

X <- cbind(ones, X)

alpha = 0.01;
iterations = 400

theta <- matrix(0, nrow = ncol(X), ncol = 1)

J_hist <- GradientDescent(X, y, theta, alpha, iterations)[[2]]
theta <- GradientDescent(X, y, theta, alpha, iterations)[[1]]

plot(J_hist[,1], type = "l")


