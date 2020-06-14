# Project3RScript.R
#
# Author: M. Joseph Meyer
###############################################################################

#----------------------#
# Options and Packages #
#----------------------#

options(contrasts = c("contr.sum","contr.poly"))
library(psych)
library(car)

#------#
# Data #
#------#

multRegData <- read.csv("threePredictors.csv")

str(multRegData)
head(multRegData)


#------------------------#
# Descriptive Statistics #
#------------------------#

describe(multRegData)

#----------------------#
# Visualizing the Data #
#----------------------#

hist(resid(lm(Y~X1+X2+X3,multRegData)),breaks=10,
     main="Histogram of Residuals",xlab="Residuals")

#----------------------------------------#
# Testing Assumptions: Shapiro-Wilk test #
#----------------------------------------#

shapiro.test(resid(lm(Y~X1+X2+X3,multRegData)))

#----------------------------------------------------------#
# Testing Assumptions: Test for Nonconstant Error Variance #
#----------------------------------------------------------#

ncvTest(lm(Y~X1+X2+X3,multRegData))

#---------------------------------------#
# Testing Assumptions: Multicolinearity #
#---------------------------------------#
pairs(multRegData[,-1])
cor(multRegData[,-1])
  cor.test(multRegData$X1,multRegData$X2)
cor.test(multRegData$X1,multRegData$X3)
cor.test(multRegData$X2,multRegData$X3)

#---------------------------------#
# Multiple Regression, Full Model #
#---------------------------------#

fit <- lm(Y ~ X1 + X2 + X3, data = multRegData)
summary(lm(fit))

#--------------------------------#
# Multiple Regression, Submodels #
#--------------------------------#

fit1 <- lm(Y ~ 1, data = multRegData)
summary(lm(fit1))


fit2 <- lm(Y ~ X1, data = multRegData)
summary(lm(fit2))

fit3 <- lm(Y ~ X2, data = multRegData)
summary(lm(fit3))
  
fit4 <- lm(Y ~ X3, data = multRegData)
summary(lm(fit4))
  
fit5 <- lm(Y ~ X1 + X2, data = multRegData)
summary(lm(fit5))
  
fit6 <- lm(Y ~ X2 + X3, data = multRegData)
summary(lm(fit6)) 
  
fit7 <- lm(Y ~ X1 + X3, data = multRegData)
summary(lm(fit7))

