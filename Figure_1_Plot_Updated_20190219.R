# https://cran.r-project.org/web/packages/dbscan/dbscan.pdf
rm(list=ls())
cat("\014")
library("fpc")
library("caret")
library("plotly")
source('my_util.R')

# Loading data

data <- load_csv_data('TSR_2017')

data <- filter(data, data$discharged_mrs!='6')
data_bm <- dplyr::select(data, Barthel_Total, discharged_mrs)

# LOWESS
# mRS is X (independent variable), Barthel is Y (dependent variable)
# y ~ model ...
lowess.data <- lowess(data_bm$Barthel_Total ~ data_bm$discharged_mrs, f=1/32)

# boundary
mrs.0 <- filter(data_bm, data_bm$discharged_mrs == 0)[,1]
mrs.0.sd <- sd(mrs.0)

mrs.1 <-filter(data_bm, data_bm$discharged_mrs == 1)[,1]
mrs.1.sd <- sd(mrs.1)

mrs.2 <- filter(data_bm, data_bm$discharged_mrs == 2)[,1]
mrs.2.sd <- sd(mrs.2)

mrs.3 <- filter(data_bm, data_bm$discharged_mrs == 3)[,1]
mrs.3.sd <- sd(mrs.3)

mrs.4 <- filter(data_bm, data_bm$discharged_mrs == 4)[,1]
mrs.4.sd <- sd(mrs.4)

mrs.5 <- filter(data_bm, data_bm$discharged_mrs == 5)[,1]
mrs.5.sd <- sd(mrs.5)

trim.mean.sd.obs <- mean(c(mrs.0.sd,mrs.1.sd,mrs.2.sd,mrs.3.sd,mrs.4.sd,mrs.5.sd),trim=0.05)

lower.lowess <- lowess.data$y-trim.mean.sd.obs
lower.lowess2 <- lower.lowess-trim.mean.sd.obs

upper.lowess <- lowess.data$y+trim.mean.sd.obs
upper.lowess2 <- upper.lowess+trim.mean.sd.obs

# Using one standard deviation as boundary
filter.levels.lower <- unique(lower.lowess)
filter.levels.upper <- unique(upper.lowess)
print('filter.levels.lower')
print(filter.levels.lower)
print('filter.levels.upper')
print(filter.levels.upper)

# plot bubble ===
#freq_table <- as.data.frame(table(data_bm$Barthel_Total, data_bm$discharged_mrs))
#p <- ggplot(freq_table, aes(x = freq_table[,2], y = freq_table[,1], size = freq_table[,3]))+geom_point()
#p+geom_smooth(method="loess", formula=data_bm$Barthel_Total ~ data_bm$discharged_mrs, size = 1.5)

# plot regression line ===

cx <- NULL
for (i in 1:length(data_bm$discharged_mrs)) {
	temp1 <- data_bm$discharged_mrs[i]
	temp2 <- data_bm$Barthel_Total[i]
	temp3 <- paste(c(temp1,temp2),sep="",collapse="_")
	cx <- c(cx,temp3)
}
cx.table <- (table(cx)/sum(table(cx)))

cx.table.pos <- names(cx.table)
cx.table.size <- as.numeric(unlist(cx.table))
final.pos <- NULL
for (i in 1:length(cx.table.pos)) {
	final.pos <- rbind(final.pos,strsplit(cx.table.pos[i], "_")[[1]])
}
plot(data_bm$discharged_mrs,data_bm$Barthel_Total,xlab="MRS",ylab="BAR",main="LOWESS (f=1/32)",col=2,type='n')
ppp <- 12*(cx.table.size*5)
ppp <- ifelse(ppp>2.5,2.5,ppp)
points(final.pos[,1],final.pos[,2],col=2,cex=ppp)

lines(lowess.data$x,lowess.data$y,lwd=2)
#  
lines(lowess.data$x,lower.lowess,lwd=2,lty=2)
lines(lowess.data$x,lower.lowess2,lwd=2,lty=3)
# 
lines(lowess.data$x,upper.lowess,lwd=2,lty=2)
lines(lowess.data$x,upper.lowess2,lwd=2,lty=3)
