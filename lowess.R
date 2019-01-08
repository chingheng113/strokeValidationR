# https://cran.r-project.org/web/packages/dbscan/dbscan.pdf
rm(list=ls())
cat("\014")
library("fpc")
library("caret")
source('my_util.R')

data <- load_csv_data('TSR_2017')
data_bm <- dplyr::select(data, Barthel_Total, discharged_mrs)
plot(Barthel_Total ~ discharged_mrs, data=data_bm )

lowess.data <- lowess(data_bm$Barthel_Total ~ data_bm$discharged_mrs, f=1/32)

plot(as.numeric(data_bm$discharged_mrs, data_bm$Barthel_Total),xlab="mrs",ylab="bar",main="LOWESS (f=1/32)",col=2)
lines(lowess.data$x,lowess.data$y,lwd=2)
