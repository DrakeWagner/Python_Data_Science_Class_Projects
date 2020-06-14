# Assessment.R
# Date: Feb 28, 2019 11:19:30 AM 2019
#
# Author: M. Joseph Meyer
###############################################################################
#
# Revision History
# Feb 28, 2019 11:19:30 AM 2019: Assessment.R was created.
#
#
################################################################################

# Instructions

# Make sure to follow all instructions listed in this project.  As a reminder, you need to load
# the R script using File -> Open File (The R script is located on Collab, under the "Week 7" tab).
#  As noted before, you are welcome to use any non-live and in-class resources (lab R scripts,
# lecture slides, textbooks, other Collab material, stackoverflow) as long as you properly cite
# all sources.  You are NOT allowed to use any in-person help (posting on forums, internet live chat,
# other students), with the exception of the instructor.  Good luck, and have fun!

# For each question, please make sure to write the code you used in the proper section of the
# R script, as well as in the space provided after each question on Collab.

# When you're done, please upload your R script and any other files using the respective file
# upload questions.

#--------------------------

# Dataset

# This is a simulated dataset based on questions from the Toronto Empathy Questionnaire (Spreng, McKinnon,
# Mar, and Levine, 2009).  More specifically, the dataset contains simulated scores from items (or
# questionnaire questions) 1, 8, 9, and 13.  An empathy score can be obtained by summing all the scores
# after accounting for reversed-scored items: the higher the total score, the more empathetic one is.
# The sums of the other scores are in the sumOtherItems column.

# The dataset (midtermData.csv) can be found under the "Week 7" tab on Collab.

#--------------------------

# Load packages

# Please write all code for this section in the "Load packages" section of the R script and the space
# provided after each question on Collab.

# Question 1: Load the psych, car, and reshape2 packages.

library(psych)
library(car)
library(reshape2)
#--------------------------

# Read in data

# Please write all code for this section in the "Read in data" section of the R script and the space
# provided after each question on Collab.

# Question 2: Place the midtermData.csv file into your working directory (use getwd and setwd if needed), and
# use the read.csv function to read in the data.

getwd()
myData <- read.csv("midtermData.csv")

#--------------------------

# Viewing data

# Please write all code for this section in the "Viewing data" section of the R script and the space
# provided after each question on Collab.

# Question 3: Use dim on the data.  What are the dimensions of the data?
?dim()
dim(myData)
#180 rows and 8 columns

# Question 4: Use head and tail on the data.
head(myData)
tail(myData)

# Question 5: Use str on the data.  What is the output?
str(myData)


# Question 6: Looking at the output of the str function, what are the value types (numeric, character,
# logical, integer, complex) of each column?

class(myData$Participant)
class(myData$Age)
class(myData$Time)
class(myData$When_someone_else_is_feeling_excited_I_tend_to_get_excited_too)
class(myData$I_can_tell_when_others_are_sad_even_when_they_do_not_say_anything)
class(myData$I_find_that_I_am_in_tune_with_other_people.s_moods)
class(myData$I_get_a_strong_urge_to_help_when_I_see_someone_who_is_upset)
class(myData$sumOtherItems)
lapply(myData, class)



#--------------------------

# Cleaning data

# Please write all code for this section in the "Cleaning data" section of the R script and the space
# provided after each question on Collab.

# Question 7: Use the colnames function on the data to get the names of the columns, and assign it to the R object
# namesVars.

namesVars <- colnames(myData)
namesVars


# Question 8: Construct a subset of namesVars that only contains the names of the items (or questions from the
# questionnaire).  Assign this subset to the R object itemQuestions.
itemQuestions <- namesVars[4:7]
itemQuestions

# Question 9: In the dataset, rename the columns that represent the items (that is, the columns whose names are in
# itemQuestions) to "Item1", "Item2", "Item3", and "Item4".

names(myData)[4] <- "Item1"
names(myData)[5] <- "Item2"
names(myData)[6] <- "Item3"
names(myData)[7] <- "Item4"


# Question 10: Convert the Participant column to a factor.
myData$Participant=as.factor(myData$Participant)
class(myData$Participant)


# Question 11: Use as.numeric on the converted participant column.  Do you get back the original participant id
# numbers?  Why or why not?
as.numeric(myData$Participant)
as.factor(myData$Participant)
#Yes it does, since it is asking for the numeric values again.


# Question 12: Recode the Time column such that 1 becomes 'Time1', 2 becomes 'Time2', and 3 becomes 'Time3'.
myData$Time <- recode(myData$Time, "1 = 'Time1';
       2 = 'Time2';
              3 = 'Time3'")

       
myData

# Question 13: Recode the four items such that 'Always' becomes 4, 'Often' becomes 3, 'Sometimes' becomes 2, 'Rarely'
#  becomes 1, and 'Never' becomes 0. (Hint: The first part of the recoding should be 'Always' = 4;. Also, make sure to
# set the as.factor argument to be FALSE, and the as.numeric argument to be TRUE!)
str(myData)
myData$Item1 <- Recode(myData$Item1, "'Always' = 4;
                'Often' = 3; 
                'Sometimes' = 2;
                    'Rarely' = 1;
                      'Never' = 0",
                as.factor = FALSE,
                as.numeric = TRUE)

myData$Item2 <- Recode(myData$Item2, "'Always' = 4;
                'Often' = 3; 
       'Sometimes' = 2;
       'Rarely' = 1;
       'Never' = 0",
       as.factor = FALSE,
       as.numeric = TRUE)

myData$Item3 <- Recode(myData$Item3, "'Always' = 4;
                'Often' = 3; 
       'Sometimes' = 2;
       'Rarely' = 1;
       'Never' = 0",
       as.factor = FALSE,
       as.numeric = TRUE)

myData$Item4 <- Recode(myData$Item4, "'Always' = 4;
                'Often' = 3; 
       'Sometimes' = 2;
       'Rarely' = 1;
       'Never' = 0",
       as.factor = FALSE,
       as.numeric = TRUE)


# Question 14: Add together the scores of the four items (Item1, Item2, Item3, and Item4) and the sumOtherItems column.
# Assign this sum to the column TotalScore in the dataset.

TotalScore <- myData$Item1 + myData$Item2 + myData$Item3 + myData$Item4 + myData$sumOtherItems
myData$TotalScore <- c(TotalScore)




myData
TotalScore

# Question 15: Remove all the columns except for Participant, Age, Time, and TotalScore.
myData$Item1 <- NULL
myData$Item2 <- NULL
myData$Item3 <- NULL
myData$Item4 <- NULL
myData$sumOtherItems <- NULL



# Question 16: Use the order function to sort the data by the Participant column.  The head of the data should
# look like...
#     Participant Age  Time TotalScore
# 1          3001  23 Time1         38
# 61         3001  23 Time2         39
# 121        3001  23 Time3         24
# 2          3002  22 Time1         49
# 62         3002  22 Time2         40
# 122        3002  22 Time3         49

head(myData[order(myData$Participant),])


#--------------------------

# Reshaping data

# Please write all code for this section in the "Reshaping data" section of the R script and the space
# provided after each question on Collab.

# Question 17: Use the dcast function to reshape the data from tall to wide, such that Participant and Age are id
# variables, Time is the grouping variable, and TotalScore is the value variable.  Assign this wide data to the 
# R object cleanedData.

cleanedData <- dcast(myData, 
                     Participant + Age ~ Time,
                     value.var="TotalScore")
cleanedData

#--------------------------

# Descriptive statistics

# Please write all code for this section in the "Descriptive statistics" section of the R script and the space
# provided after each question on Collab.

# Question 18: Use the describe function on the cleaned data set to get the descriptive statistics, and assign the
# output to the R object descriptiveStats.
descriptiveStats <- describe(cleanedData)
descriptiveStats


# Question 19: Use the str, is.vector, is.data.frame, and is.matrix functions on the descriptiveStats object.
# What kind of data structure is descriptiveStats?
str(descriptiveStats)
is.vector(descriptiveStats)
is.data.frame(descriptiveStats)
is.matrix(descriptiveStats)
#It is a Data Frame

# Question 20: Use the proper names function on descriptiveStats to look at the column names.
colnames(descriptiveStats)

# Question 21: Create a subset of descriptive stats that has rows 2 through 5, and the columns mean, sd, median,
# mad, skew, and kurtosis.  Assign this subset to the R object descStatsSubset.
descStatsSubset <- descriptiveStats[2:5,c("mean", "sd", "median", "mad", "skew", "kurtosis")]
descStatsSubset
descriptiveStats
# Question 22 (Extra credit): What do the asterisks (*) mean in the output of the describe function? (Hint: look
# at the structure of the cleaned data, or str(cleanedData)). 

#It means that the p value is less than or equal to .05, or that it is a factor variable?

#--------------------------

# Write out data

# Please write all code for this section in the "Write out data" section of the R script and the space
# provided after each question on Collab.

# Question 23: Use the write.csv function to write out the cleaned data to a file.  Make sure that no row names are
# included in the file.
write.csv(cleanedData, "cleanedData.csv", row.names = FALSE)

# Question 24: Use the capture.output function to write the output of descStatsSubset to a file.
capture.output(describe(descStatsSubset), file="DescribeddescStatsSubset.txt")



