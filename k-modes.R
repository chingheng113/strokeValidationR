# The k-modes algorithm uses a simple matching dissimilarity measure to deal with categorical objects, replaces the means of clusters with modes, and uses a frequency-based method to update modes in the clustering process to minimise the clustering cost function.
rm(list=ls())
cat("\014")
library(klaR)
source('my_util.R')
tsr_data <- load_tsr_data(5, 'is') # all, 0..5;is/he
tsr_results <-kmodes(tsr_data$b_data, 5 ,iter.max = 100, weighted = FALSE ) 
print(tsr_results)
