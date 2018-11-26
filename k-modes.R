# The k-modes algorithm uses a simple matching dissimilarity measure to deal with categorical objects, replaces the means of clusters with modes, and uses a frequency-based method to update modes in the clustering process to minimise the clustering cost function.
rm(list=ls())
cat("\014")
library(klaR)
source('my_util.R')
tsr_data <- load_tsr_data(5, '') # all, 0..5;is/he
# -----
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){set.seed(100000)
                sum(kmodes(tsr_data$b_data, k, iter.max = 100 ,weighted = FALSE)$withindiff)})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# -----
tsr_results <-kmodes(tsr_data$b_data, 4 ,iter.max = 100, weighted = FALSE )

