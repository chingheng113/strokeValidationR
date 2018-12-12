# https://cran.r-project.org/web/packages/dbscan/dbscan.pdf
rm(list=ls())
cat("\014")
library("fpc")
library("caret")
source('my_util.R')

mrs = '4'
tsr_data <- load_tsr_data(mrs, 'is') # all, 0..5;is/he
bData <- tsr_data$b_data

# PCA reduction
pca_result_tsr <- princomp(bData, cor = TRUE)
bData_pca <- pca_result_tsr$scores
bData_pca <- bData_pca[,1:2]
bData_pca_unique <- unique(bData_pca)

#dbscan
res <- fpc::dbscan(bData_pca_unique, eps = 1, MinPts = 11)
train_label <- res$cluster
train_label <- replace(train_label, train_label == 0, -1) # <- consist with python
train_label <-replace(train_label, train_label > -1, 0) # <- consist with pyhton

test_data <- load_nih_test(mrs)
test_label <- test_data$label
test_bi <- subset( test_data, select = -label )
# write.csv(label, file = "dbscan_label.csv")
predicted_label <- predict(res, test_bi, data = bData_pca_unique)
predicted_label <- replace(predicted_label, predicted_label == 0, -1) # <- consist with python
predicted_label <-replace(predicted_label, predicted_label > -1, 0) # <- consist with pyhton


cft<-table(predicted_label, test_label)
tp <- cft[2, 2]
tn <- cft[1, 1]
fp <- cft[2, 1]
fn <- cft[1, 2]
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
