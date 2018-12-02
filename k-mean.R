rm(list=ls())
cat("\014")
library("factoextra")
library("fpc")
source('my_util.R')

tsr_data <- load_tsr_data('5', 'is') # all, 0..5;is/he
bData <- tsr_data$b_data
bData <- unique(bData)
# PCA reduction
pca_result_tsr <- princomp(bData, cor = TRUE)
bData <- pca_result_tsr$scores

# distance
dbscan::kNNdistplot(bData, k = 2)
abline(h = 0.15, lty = 2)

# k-mean
km.res = kmeans(bData, 2, nstart = 25)
fviz_cluster(km.res, bData, frame = FALSE, geom = "point")

