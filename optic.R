# https://cran.r-project.org/web/packages/dbscan/dbscan.pdf
rm(list=ls())
cat("\014")
library("fpc")
source('my_util.R')

tsr_data <- load_tsr_data('3', '') # all, 0..5;is/he
bData <- tsr_data$b_data

# if do PCA hide next
#bData <- scale(bData)
#bData <- unique(bData)# <-------------------- use unique data or not
# PCA reduction
pca_result_tsr <- princomp(bData, cor = FALSE)
bData_pca <- pca_result_tsr$scores
bData_pca <- bData_pca[,1:2]
bData_pca_unique <- unique(bData_pca)

### get order and plot produces a reachability plot
#num_sample <- round(nrow(bData)*0.01, digits = 0)
res <- dbscan::optics(bData_pca_unique, minPts = 3) 
res$order

### extract a DBSCAN clustering by cutting the reachability plot at eps_cl
res <- dbscan::extractDBSCAN(res, eps_cl = 0.5)
plot(res, main="Reachability plot of mRS-3")  ## black is noise
# text(110, 0.7, '[0.5]')
# dbscan::hullplot(bData, res)
# 
# ## Do dbscan
# db = fpc::dbscan(bData, eps = 0.5, MinPts = 11)
# outliers <- bData[db$cluster==0,]
# plot(db, bData, main = "DBSCAN", frame = FALSE)
# 
# ### use OPTICS on a precomputed distance matrix
# d <- dist(bData_pca_unique)
# res <- dbscan::optics(d, minPts = 10)
# plot(res)
# 
# 
# ### plot the order of points in the reachability plot
# plot(bData, col = "grey")
# polygon(bData[res$order,])