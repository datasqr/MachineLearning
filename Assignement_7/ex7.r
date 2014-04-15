install.packages('png')
library("png")

ex72 <- read.csv("ex7data2.csv", head = F, sep =";")
plot(ex72[,1], ex72[,2], pch = 19)

# 3 centroids
K <- 3

# initial centriods
initial_centroids = matrix(c(3, 3, 6, 2, 8, 5), ncol = 2, nrow=3, byrow = T) 

# Find the closest centroids for the examples using the initial_centroids
X1 <- as.matrix(ex72)

idx <- findClosestCentroids(X1, initial_centroids)

centroids <- computeCentroids(X1, idx, K);

max_iters = 10;
newcent <- runkMeans(X1, initial_centroids, max_iters)
newcent[[1]]

# The same calculations with kmeans package
######################################################
kmeansObj <- kmeans(ex72,centers=3)
names(kmeansObj)
kmeansObj$centers

plot(ex72[,1], ex72[,2], pch = 19)
points(kmeansObj$centers, pch = 19, col = "red")

kmeansObj$cluster
table(kmeansObj$cluster)
#####################################################

#%% ============= Part 4: K-Means Clustering on Pixels ===============
#  %  In this exercise, you will use K-Means to compress an image. To do this,
#%  you will first run K-Means on the colors of the pixels in the image and
#%  then you will map each pixel on to it's closest centroid.

readImage <- readPNG("bird_small.png")
dm <- dim(readImage)
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

plot(y ~ x, data=rgbImage, main="Lloyd's building",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), asp = 1)

kColors <- 16  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors, iter.max = 10)

clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="Lloyd's building",
     col = clusterColour, asp = 1, 
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 5 colours")

#####################################################
#source
#http://lamages.blogspot.com/2012/12/now-i-see-it-k-means-cluster-analysis.html

#Running a k-means analysis on the three colour columns in my data
#frame allows me to reduce the picture to k colours. The output 
#gives me for each x and y coordinate the colour cluster it 
#belongs to. Thus, I plot my picture again, but replace the 
#original colours with the cluster colours.

kColors <- 5  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

plot(y ~ x, data=rgbImage, main="Lloyd's building",
     col = clusterColour, asp = 1, 
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 5 colours")


#Running the k-means algorithm not only over the colours but over 
#the whole data frame, including the x and y coordinate will find 
#localised clusters, or so called Voronoi cells. This gives quite 
#an artistic picture.

nRegions <- 2000 # Number of Voronoi cells. 
# Be patient, this takes time.
voronoiMeans <- kmeans(rgbImage, centers = nRegions, iter.max = 50)
voronoiColor <- rgb(voronoiMeans$centers[voronoiMeans$cluster, 3:5])

plot(y ~ x, data=rgbImage, col = voronoiColor, 
     asp = 1, main="Lloyd's building",
     axes=FALSE, ylab="", xlab="2000 local clusters")


#Instead of plotting all x and y coordinates I can also plot only
#the cluster centres. In the following chart I generated 500 
#cluster and plotted their centres as little rectangles, which 
#provides an interesting picture in itself.

nRegions <- 500  
voronoiMeans <- kmeans(rgbImage, centers = nRegions, iter.max = 50)
voronoiColor <- voronoiColor <- rgb(voronoiMeans$centers[,3:5])

plot(y ~ x, data=voronoiMeans$centers, 
     col = voronoiColor, 
     main="Lloyd's building", asp = 1, 
     pch = 15, cex=1.5, axes=FALSE, 
     ylab="", xlab="500 local clusters")


