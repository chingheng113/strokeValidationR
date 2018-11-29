rm(list=ls())
cat("\014")
library("factoextra")
library("fpc")
source('my_util.R')

tsr_data <- load_tsr_data('0', 'is') # all, 0..5;is/he
bData <- tsr_data$b_data
#bData <- unique(bData)
# PCA reduction
pca_result_tsr <- princomp(bData, cor = TRUE)
bData <- pca_result_tsr$scores

# distance
dbscan::kNNdistplot(bData, k = 2)
abline(h = 0.15, lty = 2)

# k-mean
km.res = kmeans(bData, 2, nstart = 25)
fviz_cluster(km.res, bData, frame = FALSE, geom = "point")


# dbscan
# http://kanchengzxdfgcv.blogspot.com/2017/08/r-dbscan.html
db = fpc::dbscan(bData, eps = 1.7, MinPts = 7)
# plot(db, bData, main = "DBSCAN", frame = FALSE)
fviz_cluster(db, bData, stand = FALSE, frame = FALSE, geom = "point")
print(db)
