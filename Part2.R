#----------------------------------------------------------------------------


#Part2

#Distance Based Clustering Function

#Dataset1

#download and install package "cluster", "rgl", "fpc", "igraph"
#load the libraries "cluster", "rgl", "fpc", "igraph"
library(cluster)
library(igraph)
library(fpc)
library(rgl)

#load data set from the working directory
data1 <- read.csv("dataset1.csv")

#removing cluster labels
data11 <- data1[, 1:3]

#plotting the given data
plot3d(data11)

#assigning weight attributes to x, y and z where x = 4, y = 2 and z = 1
a <- data11[, 1]*16
b <- data11[, 2]*4
c <- data11[, 3]

#creating new data to use for clustering
data111 <- data.frame(x=a,y=b,z=c)

#displaying the data to check if the desired changes have taken place
data111

#plotting the desired data in 3D before clustering
plot3d(data111)

#to check for the optimum number of clusters
wss <- (nrow(data111)-1)*sum(apply(data111,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data111, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#using K-MEANS and plotting the 3D graph as required
cluster4 <- kmeans(data111, centers=6)
plotcluster(data11, cluster4$cluster)
points(cluster4$centers[,c("x", "y", "z")], col=1:3)
plot3d(data11[c("x", "y", "z")], col=cluster4$cluster)

#purity function to calculate the accuracy of clustering performed by function above
purity <- function(clusters, classes)
{
sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

#this gives the accuracy of the clustering performed on a scale from 0 to 1
purity(cluster4$cluster, data1[, 4])


#------------------------------------------------------------------------------
