#------------------------------------------------------------------------------
# Code By: Kory R Johnson
# Affiliation: NINDS/NIH
# Contact information: johnsonko@ninds.nih.gov
#------------------------------------------------------------------------------

rm(list=ls())
options(object.size=Inf)

data <- read.table("data.txt",header=T,row.names=1)
lowess.data <- lowess(as.numeric(unlist(data[,1]))~as.numeric(unlist(data[,2])),f=1/32)
plot(as.numeric(unlist(data[,2])),as.numeric(unlist(data[,1])),xlab="BAR",ylab="MRS",main="LOWESS (f=1/32)",col=2)
lines(lowess.data$x,lowess.data$y,lwd=2)

mrs.0 <- data[data[,2]==0,1]
mrs.0.sd <- sd(mrs.0)

mrs.1 <- data[data[,2]==1,1]
mrs.1.sd <- sd(mrs.1)

mrs.2 <- data[data[,2]==2,1]
mrs.2.sd <- sd(mrs.2)

mrs.3 <- data[data[,2]==3,1]
mrs.3.sd <- sd(mrs.3)

mrs.4 <- data[data[,2]==4,1]
mrs.4.sd <- sd(mrs.4)

mrs.5 <- data[data[,2]==5,1]
mrs.5.sd <- sd(mrs.5)

mrs.6 <- data[data[,2]==6,1]
mrs.6.sd <- sd(mrs.6)

trim.mean.sd.obs <- mean(c(mrs.0.sd,mrs.1.sd,mrs.2.sd,mrs.3.sd,mrs.4.sd,mrs.5.sd,mrs.6.sd),trim=0.05)

lower.lowess <- lowess.data$y-trim.mean.sd.obs
lower.lowess2 <- lower.lowess-trim.mean.sd.obs
lines(lowess.data$x,lower.lowess,lwd=2,lty=2)
lines(lowess.data$x,lower.lowess2,lwd=2,lty=3)

upper.lowess <- lowess.data$y+trim.mean.sd.obs
upper.lowess2 <- upper.lowess+trim.mean.sd.obs
lines(lowess.data$x,upper.lowess,lwd=2,lty=2)
lines(lowess.data$x,upper.lowess2,lwd=2,lty=3)

filter.levels.lower <- unique(lower.lowess2)
filter.levels.upper <- unique(upper.lowess2)

keep.mrs.0 <- data[data[,2]==0,]
check1 <- keep.mrs.0[,1]>filter.levels.lower[1]
check2 <- keep.mrs.0[,1]<filter.levels.upper[1]
check12 <- check1+check2
check12 <- check12==2
keep.mrs.0 <- keep.mrs.0[check12,]
points(keep.mrs.0[,2],keep.mrs.0[,1],col=3)

keep.mrs.1 <- data[data[,2]==1,]
check1 <- keep.mrs.1[,1]>filter.levels.lower[2]
check2 <- keep.mrs.1[,1]<filter.levels.upper[2]
check12 <- check1+check2
check12 <- check12==2
keep.mrs.1 <- keep.mrs.1[check12,]
points(keep.mrs.1[,2],keep.mrs.1[,1],col=3)

keep.mrs.2 <- data[data[,2]==2,]
check1 <- keep.mrs.2[,1]>filter.levels.lower[3]
check2 <- keep.mrs.2[,1]<filter.levels.upper[3]
check12 <- check1+check2
check12 <- check12==2
keep.mrs.2 <- keep.mrs.2[check12,]
points(keep.mrs.2[,2],keep.mrs.2[,1],col=3)

keep.mrs.3 <- data[data[,2]==3,]
check1 <- keep.mrs.3[,1]>filter.levels.lower[4]
check2 <- keep.mrs.3[,1]<filter.levels.upper[4]
check12 <- check1+check2
check12 <- check12==2
keep.mrs.3 <- keep.mrs.3[check12,]
points(keep.mrs.3[,2],keep.mrs.3[,1],col=3)

keep.mrs.4 <- data[data[,2]==4,]
check1 <- keep.mrs.4[,1]>filter.levels.lower[5]
check2 <- keep.mrs.4[,1]<filter.levels.upper[5]
check12 <- check1+check2
check12 <- check12==2
keep.mrs.4 <- keep.mrs.4[check12,]
points(keep.mrs.4[,2],keep.mrs.4[,1],col=3)

keep.mrs.5 <- data[data[,2]==5,]
check1 <- keep.mrs.5[,1]>filter.levels.lower[6]
check2 <- keep.mrs.5[,1]<filter.levels.upper[6]
check12 <- check1+check2
check12 <- check12==2
keep.mrs.5 <- keep.mrs.5[check12,]
points(keep.mrs.5[,2],keep.mrs.5[,1],col=3)

keep.mrs.6 <- data[data[,2]==6,]
check1 <- keep.mrs.6[,1]>filter.levels.lower[7]
check2 <- keep.mrs.6[,1]<filter.levels.upper[7]
check12 <- check1+check2
check12 <- check12==2
keep.mrs.6 <- keep.mrs.6[check12,]
points(keep.mrs.6[,2],keep.mrs.6[,1],col=3)

final.keep.set <- rbind(keep.mrs.0,keep.mrs.1,keep.mrs.2,keep.mrs.3,keep.mrs.4,keep.mrs.5,keep.mrs.6)
write.table(final.keep.set,"final.keep.set",sep="\t")
