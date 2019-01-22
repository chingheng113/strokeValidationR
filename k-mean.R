rm(list=ls())
cat("\014")
library("factoextra")
library("fpc")
library("purrr")
source('my_util.R')

tsr_data <- load_tsr_data('0', '') # all, 0..5;is/he
bData <- tsr_data$b_data
pca_result_tsr <- princomp(bData, cor = FALSE)
bData_pca <- pca_result_tsr$scores
bData_pca <- bData_pca[,1:2]
bData_pca_unique <- unique(bData_pca)
# ====================================================================
# https://uc-r.github.io/kmeans_clustering#distance
# --------------
fviz_nbclust(bData_pca, 
             FUNcluster = kmeans,# K-Means
             method = "silhouette",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +
labs(title="Elbow Method for K-Means") +
geom_vline(xintercept = 3, linetype = 2)       # 在 X=n的地方畫一條垂直虛線
# ====================================================================
# k-mean
km.res = kmeans(bData_pca, 8, nstart = 10)
fviz_cluster(km.res, bData_pca, frame = FALSE, geom = "point")
min_clust <- min(table(km.res$cluster))
print(min_clust/nrow(bData))
# ====================================================================
# https://uc-r.github.io/kmeans_clustering#distance
# function to compute average silhouette for k clusters
# avg_sil <- function(k) {
#   km.res <- kmeans(bData_pca_unique, centers = k, nstart = 25)
#   ss <- silhouette(km.res$cluster, dist(bData_pca_unique))
#   mean(ss[, 3])
# }
# # Compute and plot wss for k = 2 to k = 15
# k.values <- 2:10
# # extract avg silhouette for 2-15 clusters
# avg_sil_values <- map_dbl(k.values, avg_sil)
# plot(k.values, avg_sil_values,
#      type = "b", pch = 19, frame = FALSE, 
#      xlab = "Number of clusters K",
#      ylab = "Average Silhouettes")
# ====================================================================


