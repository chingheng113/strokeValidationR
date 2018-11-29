rm(list=ls())
cat("\014")
library("fpc")
source('my_util.R')

tsr_data <- load_tsr_data('5', 'is') # all, 0..5;is/he
bData <- tsr_data$b_data
#bData <- unique(bData)
# PCA reduction
pca_result_tsr <- princomp(bData, cor = TRUE)
bData <- pca_result_tsr$scores
bData <- bData[,1:2]

### get order and plot produces a reachability plot
res <- dbscan::optics(bData, minPts = 11) # <-------------------- use unique data or not
res$order

### extract a DBSCAN clustering by cutting the reachability plot at eps_cl
res <- dbscan::extractDBSCAN(res, eps_cl = 0.5)
plot(res)  ## black is noise
# dbscan::hullplot(bData, res)

## Do dbscan
db = fpc::dbscan(bData, eps = 1, MinPts = 11)
plot(db, bData, main = "DBSCAN", frame = FALSE)

### use OPTICS on a precomputed distance matrix
d <- dist(bData)
res <- dbscan::optics(d, minPts = 10)
plot(res)


### plot the order of points in the reachability plot
plot(bData, col = "grey")
polygon(bData[res$order,])