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

unpleasant <- c(3,3,4,4,4,3,1,2,2,4)
neutral <- c(5,4,4,3,6,3,4,2,4,5)
pleasant <- c(6,5,4,7,6,4,5,6,5,7)

ANOVAData1 <- data.frame(unpleasant,neutral,pleasant)

ANOVAData1
str(ANOVAData1)
head(ANOVAData1)

# ----------------

ANOVAData1Long <- stack(ANOVAData1)[c(2,1)]
colnames(ANOVAData1Long) <- c("cond","mood")

ANOVAData1Long
str(ANOVAData1Long)
head(ANOVAData1Long)

#--------------------------#
# Pre-study power analysis #
#--------------------------#

hypRSquared <- .06
correspondingf <- sqrt(hypRSquared/(1-hypRSquared))
pwr.anova.test(f=correspondingf,k=3,sig.level=.050,power=.80)

#------------------------#
# Descriptive Statistics #
#------------------------#

describe(neutral)
describe(pleasant)
describe(unpleasant)

#----------------------#
# Visualizing the Data #
#----------------------#

# Run these plots one plot at a time!

barplot(unpleasant, main = "Depressed-Mood Induction",xlab="Participant", ylab="Mood Scale",
        ylim=c(0,7))

barplot(neutral, main = "Neutral-Mood Induction",xlab="Participant", ylab="Mood Scale",
        ylim=c(0,7))

barplot(pleasant, main = "Elated-Mood Induction",xlab="Participant", ylab="Mood Scale",
        ylim=c(0,7))

#------------------------------#
# Testing Assumptions: Boxplot #
#------------------------------#

boxplot(unpleasant, neutral, pleasant)

#----------------------------------------#
# Testing Assumptions: Shapiro-Wilk test #
#----------------------------------------#

shapiro.test(unpleasant)
shapiro.test(neutral)
shapiro.test(pleasant)

#------------------------------------#
# Testing Assumptions: Levene's Test #
#------------------------------------#

leveneTest(mood~cond, data=ANOVAData1Long)

#----------------------------#
# ANOVA, The Traditional Way #
#----------------------------#

anova(lm(mood~cond, data=ANOVAData1Long))

#----------------------#
# Polynomial Contrasts #
#----------------------#

# Comparison 1: Linear trend

ANOVAData1Long$Linear <- Recode(ANOVAData1Long$cond, "'unpleasant' = -1;     
                                'neutral' = 0;
                                'pleasant' = 1",
                                as.factor = FALSE,
                                as.numeric = TRUE)

# Comparison 2: Quadratic trend

ANOVAData1Long$Quad <- Recode(ANOVAData1Long$cond, "'unpleasant' = 1;     
                              'neutral' = -2;
                              'pleasant' = 1",
                              as.factor = FALSE,
                              as.numeric = TRUE)

#--------------------#
# Adjusted Contrasts #
#--------------------#

# Comparison 1: Linear trend, Halved

ANOVAData1Long$LinearHalved <- Recode(ANOVAData1Long$cond, "'unpleasant' = -0.5;     
                                      'neutral' = 0;
                                      'pleasant' = 0.5",
                                      as.factor = FALSE,
                                      as.numeric = TRUE)

# Comparison 2: Quadratic trend, Doubled

ANOVAData1Long$QuadDoubled <- Recode(ANOVAData1Long$cond, "'unpleasant' = 2;     
                                     'neutral' = -4;
                                     'pleasant' = 2",
                                     as.factor = FALSE,
                                     as.numeric = TRUE)

#Doubling the linear trend
ANOVAData1Long$LinearDoubled <- Recode(ANOVAData1Long$cond, "'unpleasant' = -2;
                                       'neutral' = 0;
                                       'pleasant' = 2",
                                       as.factor = FALSE,
                                       as.numeric = TRUE)

summary(lm(mood~LinearDoubled+QuadDoubled, data=ANOVAData1Long))

#-------------------------------------------#
# ANOVA by Regression, Polynomial Contrasts #
#-------------------------------------------#

# Insert R code here (see question 12)

anova(lm(mood~Linear+Quad, data=ANOVAData1Long))
summary(lm(mood~Linear+Quad, data=ANOVAData1Long))


#Traditional Way: 
anova(lm(mood~cond, data=ANOVAData1Long))



#-----------------------------------------#
# ANOVA by Regression, Adjusted Contrasts #
#-----------------------------------------#

# Insert R code here (see question 17)

anova(lm(mood~LinearHalved+QuadDoubled, data=ANOVAData1Long))
summary(lm(mood~LinearHalved+QuadDoubled, data=ANOVAData1Long))

