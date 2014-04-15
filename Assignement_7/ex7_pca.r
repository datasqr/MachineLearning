install.packages('svd')
library("svd")


#%% ================== Part 1: Load Example Dataset  ===================
#  %  We start this exercise by using a small dataset that is easily to
#%  visualize

ex71 <- read.csv("ex7data1.csv", head = F, sep =";")
plot(ex71[,1], ex71[,2], pch = 19)
X1 <- as.matrix(ex71)

#%% =============== Part 2: Principal Component Analysis ===============
#  %  You should now implement PCA, a dimension reduction technique. You
#%  should complete the code in pca.m
#%

X_n <- featureNormalize(X1)

vect <- pca(X_n)
U <- vect$u

plot(ex71[,1], ex71[,2], pch = 19)


#%% =================== Part 3: Dimension Reduction ===================
#  %  You should now implement the projection step to map the data onto the 
#%  first k eigenvectors. The code will then plot the data in this reduced 
#%  dimensional space.  This will show you what the data looks like when 
#%  using only the corresponding eigenvectors to reconstruct it.
#%
#%  You should complete the code in projectData.m
#%

plot(X_n[,1], X_n[,2], pch = 19)

#%  Project the data onto K = 1 dimension
K = 1;
Z = projectData(X_n, U, K)
Z[1]

X_rec  = recoverData(Z, U, K)
plot(X_n[,1], X_n[,2], pch = 19, col="blue")
points(X_rec[,1], X_rec[,2], pch = 19, col="red")


###################################################################

#%% =============== Part 4: Loading and Visualizing Face Data =============
#  %  We start the exercise by first loading and visualizing the dataset.
#%  The following code will load the dataset into your environment

faces <- read.csv("faces.csv", head = F, sep =",")

faces1 <- faces[, 1:100]

train<-as.matrix(faces1)

image(train)
