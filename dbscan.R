rm(list=ls())
cat("\014")
library("factoextra")
library("fpc")
source('my_util.R')
tsr_data <- load_tsr_data('all', '') # all, 0..5;is/he

# distance
dbscan::kNNdistplot(tsr_data$b_data, k = 5)
abline(h = 0.15, lty = 2)


# k-mean
km.res = kmeans(tsr_data$b_data, 2, nstart = 25)
fviz_cluster(km.res, tsr_data$b_data, frame = FALSE, geom = "point")


# dbscan
db = fpc::dbscan(tsr_data$b_data, eps = 0.15, MinPts = 5)
plot(db, tsr_data$b_data, main = "DBSCAN", frame = FALSE)
fviz_cluster(db, tsr_data$b_data, stand = FALSE, frame = FALSE, geom = "point")
print(db)
