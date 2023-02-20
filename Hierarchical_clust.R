library(stats)
mydata <- read.csv("clust.csv")

# Compute cosine similarity matrix
similarity_matrix <- crossprod(mydata) / (sqrt(colSums(mydata^2)) %*% t(sqrt(colSums(mydata^2))))

# Perform hierarchical clustering using complete linkage and cosine similarity as distance measure
hc <- hclust(as.dist(1 - similarity_matrix), method = "complete")
# Plot the dendrogram
plot(hc, cex = 0.8,hang=-1,main = "Cluster Dendrogram")

# Performing k-means clustering for different values of k and calculates the within-cluster sum of squares (WSS) for each k.
wss <- c()
for (i in 1:10) {
  km <- kmeans(mydata, centers = i, nstart = 10)
  wss[i] <- sum(km$withinss)
}
#creating a line and points graph with WSS values
plot(1:10,wss, type = "b", xlab = "Number of Clusters", ylab = "")
