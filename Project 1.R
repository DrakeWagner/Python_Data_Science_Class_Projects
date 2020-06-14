# Project1RScript.R
# Date: Sep 19, 2018 5:16:46 PM 2018
#
# Author: M. Joseph Meyer
###############################################################################
#
# Revision History
# Sep 19, 2018 5:16:46 PM 2018: Project1RScript.R was created.
#
#
################################################################################

#----------------------#
# Options and Packages #
#----------------------#

options(contrasts = c("contr.sum","contr.poly"))
library(pwr)
library(psych)
library(car)
library(effsize)

#------#
# Data #
#------#

Calcium <- c(112, 101, 123, 122, 102, 100, 106, 115, 116, 103)
Placebo <- c(117, 130, 117, 115, 121, 113, 123, 120, 107, 119)

CalciumBP <- data.frame(Placebo,Calcium)

CalciumBP
str(CalciumBP)
head(CalciumBP)

# ----------------

CalciumBPLong <- stack(CalciumBP)[,c(2,1)]
colnames(CalciumBPLong) <- c("Treatment","BP")

CalciumBPLong
str(CalciumBPLong)
head(CalciumBPLong)

#--------------------------#
# Pre-study power analysis #
#--------------------------#

pwr.t.test(d=0.50,sig.level=.050,power=.800,type="two.sample",alternative="two.sided")

#------------------------#
# Descriptive Statistics #
#------------------------#

describe(Calcium)
describe(Placebo)

#----------------------#
# Visualizing the Data #
#----------------------#

# Run these plots one plot at a time!

barplot(Calcium, main = "Calcium",xlab="Participant", ylab="Final Blood Pressure",
        ylim=c(0,140))


barplot(Placebo, main = "Placebo",xlab="Participant", ylab="Final Blood Pressure",
        ylim=c(0,140))

#------------------------------#
# Testing Assumptions: Boxplot #
#------------------------------#

boxplot(BP~Treatment,data=CalciumBPLong,xlab="Treatment",ylab="BP")

#----------------------------------------#
# Testing Assumptions: Shapiro-Wilk test #
#----------------------------------------#

shapiro.test(CalciumBPLong$BP)

#------------------------------------#
# Testing Assumptions: Levene's Test #
#------------------------------------#

leveneTest(BP~Treatment,data=CalciumBPLong)

#-------------------------#
# t-test, traditional way #
#-------------------------#

t.test(BP~Treatment,data=CalciumBPLong,alternative="two.sided",var.equal=TRUE)

#----------------------#
# t-test by regression #
#----------------------#

(myRegression <- summary(lm(BP~Treatment,data=CalciumBPLong)))
myTestStatistic <- myRegression$coef[2,3]

#-------------#
# Effect Size #
#-------------#

(cohensDResults <- cohen.d(BP~Treatment,data=CalciumBPLong))
project1CohensD <- cohensDResults$estimate

#---------------------------#
# Post-study power analysis #
#---------------------------#

CalciumSampleSize <- length(Calcium)
PlaceboSampleSize <- length(Placebo)

pwr.t.test(n=CalciumSampleSize,d=project1CohensD,sig.level=.050,
           type="two.sample",alternative="two.sided")

qt(0.025,18,lower.tail = FALSE)
pt()