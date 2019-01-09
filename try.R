rm(list=ls())
cat("\014")


x <- c(4,5,9,12)
y <- c(9,8,6,3)

#x <- c(4,5,9,9,9,9,9,9,9,9,9,9,9,12)
#y <- c(9,8,6,6,6,6,6,6,6,6,6,6,6,3)

model <- lm(y~x)
plot(x,y)
print(summary(model))
abline(model)
