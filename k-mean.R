rm(list=ls())
cat("\014")
library("factoextra")
library("fpc")
source('my_util.R')

tsr_data <- load_tsr_data('1', 'is') # all, 0..5;is/he
bData <- tsr_data$b_data
pca_result_tsr <- princomp(bData, cor = TRUE)
bData_pca <- pca_result_tsr$scores
bData_pca <- bData_pca[,1:2]
bData_pca_unique <- unique(bData_pca)

# distance
dbscan::kNNdistplot(bData_pca_unique, k = 10)
abline(h = 0.15, lty = 2)

# k-mean
km.res = kmeans(bData_pca_unique, 10, nstart = 25)
fviz_cluster(km.res, bData_pca_unique, frame = FALSE, geom = "point")

