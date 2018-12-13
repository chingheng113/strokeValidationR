rm(list=ls())
cat("\014")
library("factoextra")
library("fpc")
source('my_util.R')

tsr_data <- load_tsr_data('5', 'is') # all, 0..5;is/he
bData <- tsr_data$b_data
pca_result_tsr <- princomp(bData, cor = TRUE)
bData_pca <- pca_result_tsr$scores
bData_pca <- bData_pca[,1:2]
bData_pca_unique <- unique(bData_pca)

# distance
dbscan::kNNdistplot(bData_pca_unique, k = 10)
abline(h = 0.5, lty = 2)

# ====================================================================
# https://uc-r.github.io/kmeans_clustering#distance
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(bData_pca_unique, k, nstart = 10 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# ====================================================================
# https://uc-r.github.io/kmeans_clustering#distance
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(bData_pca_unique, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(bData_pca_unique))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:10
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
# ====================================================================

# k-mean
km.res = kmeans(bData_pca_unique, 2, nstart = 25)
fviz_cluster(km.res, bData_pca_unique, frame = FALSE, geom = "point")
table(km.res$cluster)
