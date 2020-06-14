# FinalScript.R
# Date: May 1, 2019 1:31:57 AM 2019
#
# Author: M. Joseph Meyer
###############################################################################
#
# Revision History
# May 1, 2019 1:31:57 AM 2019: FinalScript.R was created.
#
#
################################################################################


# Instructions

# Make sure to follow all instructions listed in this project.Â  As a reminder, you need to load
# theÂ R script using File ->Â Open File (The R script is located on Collab, under the "Final" tab).
#Â  As noted before, you are welcome to use any non-live and in-class resources (lab R scripts,
# lecture slides, textbooks, other Collab material, stackoverflow) as long as you properly cite
# all sources.Â  You are NOT allowed to use any in-person help (posting on forums, internet live chat,
# other students), with the exception of the instructor.Â  Good luck, and have fun!

# For each question, please make sure to write the code you used in the proper section of the
# R script, as well as in the space provided after each question.

# When you're done, please upload your R script and any other files using the respective file
# upload questions.

#----------------------------------------------------

# Dataset

# The dataset is a simulated dataset looking at high school seniors, their GPA, and their pursuit of higher
# education after graduation.  The dataset also includes demographics, what potential risk group they're
# currently in, and variables about parental and friend social support.  Note that "parental" is only defined
# as "mother figure" and "father figure" for the simplification of the project - beyond this course, the term
# "parental" in this type of research should include many other variations of family units (i.e., father-father,
# mother-mother, etc.).

# The descriptions for each variable are below:

# id: Participant ID
# studentSex: Sex of student
# age: Age of student
# What sort of higher education are you planning on pursuing? (None, Vocational School, 2-year college (Associate's),
#            4-year university (Bachelor's+), Other)
# riskGroup: Risk group (Low/Medium/High)
# GPA
# socialSupport: Types of social support (friends, father figure (if present), mother figure (if present))
# - Closeness of friends (not very close, somewhat close, very close)
# - Closeness of father figure (not very close, somewhat close, very close (Missing value if no father or father figure))
# - Closeness of mother figure (not very close, somewhat close, very close (Missing value if no mother or mother figure))
# closenessValue: Social support closeness rating

# The dataset (FinalExamData.csv) can be found under the "Final" tab on Collab.

#----------------------------------------------------

# Load packages

# Question 1: Load the psych, car, stringi, and reshape2 packages.
library("psych")
library("car")
library("stringi")
library("reshape2")


#----------------------------------------------------

# Read in data

# Question 2: Place the FinalExamData.csv file into your working directory (use getwd and setwd if needed), and
# use the read.csv function to read in the data.
getwd()
myData <- read.csv("FinalExamData.csv")
myData

#----------------------------------------------------

# Viewing data

# Question 3: Use dim, head, and tail on the data.  What are the dimensions of the data?
dim(myData)
head(myData)
tail(myData)
print("The dimensions of the data are 1800 by 8")

# Question 4: Use str on the data.  Which variables are numeric or integers?  Which variables
# are factors?
str(myData)
print("id, age, riskGroup, and closenessValue are all integers and GPA is numeric. studentSex, What.sort..., and socialSupport
      are all factors")
#----------------------------------------------------

# Cleaning data

# Question 5: Change the fourth column name to "higherEd".
colnames(myData)[4] <- "higherEd"
colnames(myData)

# Question 6: Reorder the levels for the higherEd column such that "None" is first, followed by "Vocational",
# "2-year", "4-year", and finally "Other".
levels(myData$higherEd)
myData$higherEd <- factor(myData$higherEd, levels = c("None", "Vocational", "2-year", "4-year", "Other"))

# Question 7: Use the stri_detect_fixed function to detect which elements of the socialSupport column in
# the dataset contains "father", and assign the output to the R object tfFather.  Do the same with
# detecting "mother", and assign that output to the R object tfMother.
tfFather <- stri_detect_fixed(myData$socialSupport, pattern = "father")
tfMother <- stri_detect_fixed(myData$socialSupport, pattern = "mother")
myData$socialSupport
# Question 8: Construct two (nested) ifelse functions, such that if tfFather is TRUE, the output should be
# "closenessFather"; if tfFather is FALSE and tfMother is TRUE, the output should be "closenessMother"; and
# if both are FALSE, the output should be "closenessFriends".  Assign the output of the nested ifelse
# functions back to the socialSupport column in the dataset.
myData$socialSupport <- ifelse(tfFather == TRUE, "closenessFather", ifelse(tfMother == TRUE, "closenessMother", "closenessFriends"))
myData$socialSupport

# Question 9: Recode the closenessValue column such that 1 becomes "Not Very Close", 2 becomes "Somewhat Close",
# and 3 becomes "Very Close".
myData$closenessValue[myData$closenessValue == 1] <- "Not Very Close"
myData$closenessValue[myData$closenessValue == 2] <- "Somewhat Close"
myData$closenessValue[myData$closenessValue == 3] <- "Very Close"
myData$closenessValue


# Question 10: Recode the riskGroup column such that 1 becomes "Low", 2 becomes "Medium", and 3 becomes "High".
myData$riskGroup[myData$riskGroup == 1] <- "Low"
myData$riskGroup[myData$riskGroup == 2] <- "Medium"
myData$riskGroup[myData$riskGroup == 3] <- "High"
myData$riskGroup

# Question 11: Use the dcast function to reshape the data from tall to wide, such that id, studentSex,
# age, higherEd, riskGroup, and GPA are id variables, socialSupport is the grouping variable, and
# closenessValue is the value variable.  Assign this wide data to the R object seniorEdPlans.
seniorEdPlans <- dcast(myData,
                 id + studentSex + age + higherEd + riskGroup + GPA ~ socialSupport,
                 value.var="closenessValue")

    

# Question 12: Construct a subset of seniorEdPlans that has all rows where the risk group is High, and all columns.
# Assign this subsetted dataset to the R object seniorHighRisk.

seniorHighRisk <- seniorEdPlans[seniorEdPlans$riskGroup == "High", c("id", "studentSex", "age", "higherEd", "riskGroup", "GPA",
                                                                     "closenessFather", "closenessFriends", "closenessMother")]



# Question 13: Assume that all missing values in the closenessMother and closenessFather columns are due to the parent
# not being present.  Use the is.na function on the closenessMother column, convert the output to numeric values, and
# assign the vector to the R object noMother.  Do the same with the closenessFather column, and name the R object
# noFather.
NumMother <- is.na(seniorHighRisk$closenessMother)
noMother <- ifelse(NumMother == TRUE, 0, 1)

NumFather <- is.na(seniorHighRisk$closenessFather)
noFather <- ifelse(NumFather == TRUE, 0, 1)


# Question 14: Use the expression 2 - (noMother + noFather) to find the number of parents each student has.  Assign
# this result to the column numParents in the seniorHighRisk dataset.

seniorHighRisk$numParents <- NA
Parents <- (2 - (noMother + noFather))
seniorHighRisk$numParents <- Parents

seniorHighRisk


# Question 15: Construct a subset of seniorHighRisk that has all rows where the number of parents is 1, and all columns.
# Assign this subsetted dataset to the R object seniorHighRiskSingle.

seniorHighRiskSingle <- subset(seniorHighRisk, seniorHighRisk$numParents == 1,
                               select=c(id:numParents))
seniorHighRiskSingle

# Question 16: Construct two empty vectors, closenessParent and parentSex, each with nrow(seniorHighRiskSingle) elements.

# Construct a for loop, using i as the iterator, that iterates over the vector 1:nrow(seniorHighRiskSingle).  

# For each
# value of i, use an if statement (not the ifelse function) to test whether seniorHighRiskSingle$closenessFather is
# missing using the is.na function. 

# If so, assign the ith element of closenessParent to the ith element of
# seniorHighRiskSingle$closenessMother, and assign the ith element of parentSex to "F".  

# Else, assign the ith element
# of closenessParent to the ith element of seniorHighRiskSingle$closenessFather, and assign the ith element of parentSex to "M".
closenessParent <- vector(length = nrow(seniorHighRiskSingle))
parentSex <- vector(length = nrow(seniorHighRiskSingle))

for (i in 1:nrow(seniorHighRiskSingle)) {
  if (is.na(seniorHighRiskSingle$closenessFather[i] == FALSE)) { 
    
    closenessParent[i] <- seniorHighRiskSingle$closenessMother[i]
    parentSex[i] <- "F"
  }
  else{
    closenessParent[i] <- seniorHighRiskSingle$closenessFather[i]
    parentSex[i] <- "M"
  }
}


# Question 17: Assign closenessParent to the closenessParent column and assign parentSex
# to the parentSex column in the seniorHighRiskSingle dataset.
seniorHighRiskSingle$closenessParent <- closenessParent
seniorHighRiskSingle$parentSex <- parentSex

head(seniorHighRiskSingle)
# Question 18: Use the tapply function with the mean function on seniorHighRiskSingle to get
# the average GPA at each level of closeness to the single parent (closenessParent).  Using
# the output of the tapply function, use the barplot function to make a barplot of the means.
# Set the colors of each bar to be any color of your choosing, as long as each bar has a
# unique color.  Set the limits of the y-axis to be from 0 to 4.  Add the label "Amount of
# Closeness to Parent" on the x-axis, add the label "GPA" on the y-axis, and add the title
# "GPA of high-risk high school seniors\nwho have a single parent".  You don't need to
# add a legend to the barplot.
gpaAverages <- tapply(seniorHighRiskSingle$GPA, seniorHighRiskSingle$closenessParent, mean)
COLORS <- c("cyan", "purple", "green")
barplot(gpaAverages,
        beside = TRUE,
        col = COLORS,
        ylim = c(0, 4),
        xlab = "Amount of Closeness to Parent",
        ylab = "GPA",
        main = "GPA of high-risk high school seniors\nwho have a single parent")

#----------------------------------------------------

# Hypothesis Testing

# Please write all code for this section in the "Hypothesis Testing" section of the R script and the space
# provided after each question on Collab.

# Step 1: State research and null hypotheses about populations

#H0 : µ1 = µ2
#H1 : µ1 [does not equal sign] µ2


# Hypothesis testing (null and alternative hypotheses)

# Question 19: Suppose we wanted to test whether the average GPA for high-risk high school seniors
# who have a single parent is significantly different from the national average (3.30).  State the null
# and research (or alternative) hypotheses for this research question.

#H0 : µseniorHighRiskSingle$GPA  = 3.30
#H1 : µseniorHighRiskSingle$GPA (does not equal sign) 3.30

#--------------------------

# Step 2: Determine the characteristics of the comparison distribution

# Step 2a: Check Descriptive Statistics

# Question 20: Use the describe function on the GPA column in the seniorHighRiskSingle dataset to get
# the descriptive statistics of GPA.

describe(seniorHighRiskSingle$GPA)


#-----------

# Step 2b: Visualize Data

# Question 21: Use the hist function on the GPA column in the seniorHighRiskSingle dataset to plot a
# histogram of the data.  Add the label "GPA" on the x-axis and add the title "GPA of high-risk high
# school seniors\nwho have a single parent".  Set the limits on the x-axis to be from 2 to 4.  Set
# the color to be "lightgray."
hist(seniorHighRiskSingle$GPA, 
     xlab="GPA",
     main="GPA of high-risk high school seniors\nwho have a single parent",
     xlim=c(2, 4),
     col="lightgray")



# Question 22: Use two lines functions to draw lines on the histogram plot from question 21 that represent
# the sample and population means. For the sample mean line, the x-values should be a vector of two values,
# which are both the mean of the GPA column, the y-values should be 0 and 23, and the color should be blue.
# For the population mean line, the two x-values should both be the population mean (3.30), the y-values
# should be 0 and 23, and the color should be red.  For both lines, set the line type to be 2, and the line
# thickness to be 2.


meansampleGPA <- mean(seniorHighRiskSingle$GPA)
xsample <- c(meansampleGPA, meansampleGPA)
ysample <- c(0, 23)
lines(xsample, ysample, col = "blue", lty=2, lwd=2)

xpop <- c(3.30, 3.30)
ypop <- c(0, 23)
lines(xpop, ypop, col = "red", lty=2, lwd=2)


# Question 23: Use the legend function to add a legend to the plot in the top left part of
# the histogram plot from question 21.  There should be two labels: "Sample Mean" and "Population Mean".
# Make sure that the types, colors, and thicknesses of the lines next to the two labels match the
# corresponding lines in the plot.
legend("topleft", legend=c("Sample Mean", "Population Mean"), lty=c(2,2), lwd=c(2,2), col=c("blue", "red"))

#-----------

# Step 2c: Test Assumptions

# Question 24: Use the boxplot function on the GPA column in the seniorHighRiskSingle dataset to plot a
# boxplot of the data.  Set the limits of the y-axis to be from 2 to 4.   Add the label "GPA" on the y-axis
# and add the title "GPA of high-risk high school seniors\nwho have a single parent".

boxplot(c(seniorHighRiskSingle$GPA), 
        ylim=c(2,4), 
        ylab="GPA", 
        main="GPA of high-risk high school seniors\nwho have a single parent")


# Question 25: Use the qqnorm and qqline functions on the GPA column in the seniorHighRiskSingle dataset to
# plot a QQ-plot of the data.
qqnorm(seniorHighRiskSingle$GPA)
qqline(seniorHighRiskSingle$GPA)


#--------------------------

# Step 3: Use analysis functions to analyze data

# Question 26: Given that we know the population mean (3.30) and do not know the population
# standard deviation, use an R function to perform the appropriate one-sample analysis
# (one-sample Z-test or one-sample t-test).
t.test(x = seniorHighRiskSingle$GPA,
       mu = 3.30,
       alternative = c("t"),
       var.equal = TRUE)

  
#--------------------------

# Step 4: Compare p-value in output to significance level

# Question 27: Compare the p-value from the output in Question 26 to .05, and make a decision
# about whether or not you can reject the null hypothesis.  Write an APA-style write-up of your results.
describe(seniorHighRiskSingle$GPA)

#reject null!!! :)


