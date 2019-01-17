# https://cran.r-project.org/web/packages/dbscan/dbscan.pdf
rm(list=ls())
cat("\014")
library("fpc")
library("caret")
source('my_util.R')

mrs = '4'
tsr_data <- load_tsr_data(mrs, '') # all, 0..5;is/he
bData <- tsr_data$b_data

# PCA reduction, since the PCA betwenn python and R
pca_result_tsr <- princomp(bData, cor = FALSE)
bData_pca <- pca_result_tsr$scores
bData_pca <- bData_pca[,1:2]
bData_pca_unique <- unique(bData_pca)

#dbscan
res <- fpc::dbscan(bData_pca_unique, eps = 0.5, MinPts = 3)
# Paramaters: 0-1.0, 1-0.3, 2-0.6, 3-1.0, 4-0.7, 5-0.6



## plot clusters and add noise (cluster 0) as crosses.
# plot(bData_pca_unique, col=res$cluster)
# points(bData_pca_unique[res$cluster==0,], pch = 3, col = "grey")
# hullplot(bData_pca_unique, res)

train_label <- res$cluster
train_label <- replace(train_label, train_label == 0, -1) # <- consist with python
train_label <-replace(train_label, train_label > -1, 0) # <- consist with pyhton


test_data <- load_test('alias', mrs)
test_label <- test_data$label
test_bi <- subset( test_data, select = -label )
test_bi <- predict(pca_result_tsr, newdata=test_bi)[,1:2] # doing PCA
# write.csv(label, file = "dbscan_label.csv")
predicted_label <- predict(res, test_bi, data = bData_pca_unique)
predicted_label <- replace(predicted_label, predicted_label == 0, -1) # <- consist with python
predicted_label <-replace(predicted_label, predicted_label > -1, 0) # <- consist with pyhton


cft<-table(predicted_label, test_label)
tp <- cft[2, 2]
tn <- cft[1, 1]
fp <- cft[2, 1]
fn <- cft[1, 2]
print(sensitivity <- round(tp/(tp + fn), digits=4))
print(specificity <- round(tn/(tn + fp), digits=4))
print(accuracy <- round((tp + tn)/(tp + tn + fp + fn), digits=4))
