rm(list=ls())
cat("\014")
library("factoextra")
library("fpc")
source('my_util.R')

tsr_data <- load_tsr_data('0', 'is') # all, 0..5;is/he
bData <- tsr_data$b_data
bData <- unique(bData)
# PCA reduction
pca_result_tsr <- princomp(bData, cor = TRUE)
bData <- pca_result_tsr$scores
# Elbow Method
# http://rpubs.com/skydome20/R-Note9-Clustering
fviz_nbclust(bData, 
             FUNcluster = hcut,  # hierarchical clustering
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
            ) + 
  
  labs(title="Elbow Method for HC") +
  
  geom_vline(xintercept = 3,       # 在 X=3的地方 
             linetype = 2)         # 畫一條虛線
# --------------
fviz_nbclust(bData, 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +
  
  labs(title="Elbow Method for K-Means") +
  
  geom_vline(xintercept = 3,        # 在 X=3的地方 
             linetype = 2)          # 畫一條垂直虛線

# distance
dbscan::kNNdistplot(bData, k = 2)
abline(h = 0.15, lty = 2)

# k-mean
km.res = kmeans(bData, 2, nstart = 25)
fviz_cluster(km.res, bData, frame = FALSE, geom = "point")


# dbscan
db = fpc::dbscan(bData, eps = 2, MinPts = 6)
# plot(db, bData, main = "DBSCAN", frame = FALSE)
fviz_cluster(db, bData, stand = FALSE, frame = FALSE, geom = "point")
print(db)
