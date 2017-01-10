
## 0 Load the data
activitylabels <- read.table("activity_labels.txt") 
features <- read.table("features.txt")
subjecttest <- read.table("subject_test.txt")
subjecttrain <- read.table("subject_train.txt")
xtest <- read.table("X_test.txt")
ytest <- read.table("y_test.txt")
xtrain <- read.table("X_train.txt")
ytrain <- read.table("y_train.txt")
##check the dimensions of the data with dim() is row x column
dim(features) ##561x2 this is the names of the columns in xtest and xtrain 
dim(xtest) ##2947x561 - core data
dim(xtrain) ##7352x561 - core data 
dim(subjecttest) ##2947x1 contains the numbers 2-24 - who did it
dim(ytest) ##2947x1 contains the numbers 1-6 activity labels - what did they do
dim(subjecttrain) ##7352x1 contains the numbers 1-30 - Each row identifies the subject who performed the activity
dim(ytrain) ##7352x1 contains the numbers 1-6 - activity labels
dim(activitylabels) ##6x2 - decoder - merge last or it rearranges things
## 4 Appropriately labels the data set with descriptive variable names.Sorry, out of order!
## add column names to data
colnames(subjecttest) <- "subject"
colnames(subjecttrain) <- "subject"
colnames(ytest) <- "activity"
colnames(ytrain) <- "activity"
colnames(xtest) <- features$V2 
colnames(xtrain) <- features$V2
## 1 Merges the training and the test sets to create one data set.
## Put subject and activity data into the test set
test <- cbind(subjecttest,ytest,xtest)
train <- cbind(subjecttrain,ytrain,xtrain)
##add clarity about which is test and which is training data
testname <- rep("test",nrow(test))
trainname <- rep("train",nrow(train))
test <- cbind(testname,test)
train <- cbind(trainname,train)
colnames(test)[1] <- "datatype"
colnames(train)[1] <- "datatype"
##staple them together
mega <- rbind(test,train)

## 2 Extracts only the measurements on the mean and standard deviation for each measurement.
relevant <- mega[,(grepl("mean",names(mega)))|(grepl("std",names(mega)))|names(mega)=="datatype"|names(mega)=="subject"|names(mega)=="activity"]

## 3 Uses descriptive activity names to name the activities in the data set
##merge on activity labels so we have words
named <- merge(relevant,activitylabels,by.x = "activity",by.y = "V1",all = TRUE) 
##make the new column name descriptive
colnames(named)[83] <- "activitylabel"
##ditch the old column with activity numbers and move it to the front
freshdata <- named[,c(83,2:82)]
##get rid of the factors
freshdata$activitylabel <- as.character(freshdata$activitylabel)
freshdata$datatype <- as.character(freshdata$datatype)

## 5 From the data set in step 4, creates a second, independent tidy data 
## set with the average of each variable for each activity and each subject
library(plyr)
library(reshape2)
## switch to tall form of data, for simplicity of viewing
talltidy <- melt(freshdata, id = c("activitylabel","datatype","subject"),measure.vars = names(freshdata[4:82]))
avgtidy <- ddply(talltidy,.(activitylabel,subject,variable),summarize,mean = mean(value))
write.table(avgtidy,row.name=FALSE) 

##Result:  
##a run_analysis R script, 
##a ReadMe markdown document,
##a Codebook markdown document
##a tidy data text file

##citation: https://thoughtfulbloke.wordpress.com/2015/09/09/getting-and-cleaning-the-assignment/

## dfn of tidy data:  
##nicely categorised data so we can do an analysis easily
##has it got headings that make it clear which column is which
##one column per variable (though what the variables are will be different in the long and wide form)
##is it one row per observation (though what the observations are will be different in the long and wide form)