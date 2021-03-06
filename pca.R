#You tend to use the covariance matrix when the variable scales are similar
#and the correlation matrix when variables are on different scales.
rm(list=ls())
cat("\014")
library(ggfortify)
source('my_util.R')


# TSR PCA
tsr_data <- load_tsr_data('all', '') # all..0..5; is/he
# drop duplicate
bData <- tsr_data$b_data
#bData <- tsr_data$b_data
mData <- subset(tsr_data$m_data, rownames(tsr_data$m_data) %in% rownames(bData))

pca_result_tsr <- princomp(bData, cor = FALSE)
component.var <- (pca_result_tsr$sdev^2)
varPer <- round(component.var/sum(component.var)*100, 1) # % of pca
summary(pca_result_tsr)
pr_var <- pca_result_tsr$sde^2
prop_varex <- pr_var/sum(pr_var)

autoplot(pca_result_tsr, data = mData, colour = 'discharged_mrs')+
          scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


# NIH PCA
# nih_data <- load_nih_data(5) # all;1..6
# pca_result_nih <- princomp(nih_data$b_data, cor = TRUE) 
# autoplot(pca_result_nih, data = nih_data$m_data, colour = 'discharged_mrs')+
#           scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))
