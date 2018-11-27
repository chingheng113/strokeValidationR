rm(list=ls())
cat("\014")
library("factoextra")
library("fpc")
source('my_util.R')
tsr_data <- load_tsr_data('all', '') # all, 0..5;is/he
data <- tsr_data$b_data
# data <- file.path('data', paste('pca.csv', sep='')) %>%
  # read.csv(., header = TRUE, sep = ',')
# distance
dbscan::kNNdistplot(data, k = 5)
abline(h = 0.15, lty = 2)


# k-mean
km.res = kmeans(data, 5, nstart = 25)
fviz_cluster(km.res, data, frame = FALSE, geom = "point")


# dbscan
db = fpc::dbscan(data, eps = 0.3, MinPts = 3)
plot(db, data, main = "DBSCAN", frame = FALSE)
fviz_cluster(db, data, stand = FALSE, frame = FALSE, geom = "point")
print(db)
