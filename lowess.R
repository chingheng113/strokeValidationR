# https://cran.r-project.org/web/packages/dbscan/dbscan.pdf
rm(list=ls())
cat("\014")
library("fpc")
library("caret")
library("plotly")
source('my_util.R')

data <- load_csv_data('TSR_2017')
data_bm <- dplyr::select(data, Barthel_Total, discharged_mrs)
#plot(Barthel_Total ~ discharged_mrs, data=data_bm )

# mRS is X (independent variable), Barthel is Y (dependent variable)
# y ~ model ...
lowess.data <- lowess(data_bm$Barthel_Total ~ data_bm$discharged_mrs, f=1/32)

#plot(data_bm$discharged_mrs, data_bm$Barthel_Total,xlab="mrs",ylab="bar",main="LOWESS (f=1/32)",col=2)

f2 <- count(data_bm, c('Barthel_Total','discharged_mrs'))

ggplot(data_bm, aes(x = discharged_mrs, y = Barthel_Total, size = table(data_bm$Barthel_Total))) + geom_point()


#lines(lowess.data$x,lowess.data$y,lwd=2)