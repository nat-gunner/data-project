## This script creates a tidy data set from the "Human Activity Recognition 
## Using Smartphones Data Set"; subsets it based on the mean and standard
## deviation variables; and then creates a new data set with the average 
## of each variable for each activity and each subject

## Import the feature names and create a vector to use for populating the 
## column names of the experiment tables

col_names_x <- read.table("UCI HAR Dataset/features.txt")

x_vec <- c(col_names_x)

## Import the test data set, name the colummns, and create a single table with 
## subject, activity, and readings.

x_test <- read.table("UCI HAR Dataset/test/X_test.txt")

names(x_test) <- x_vec$V2

y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

subj_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

test <- cbind(y_test,x_test)

colnames(test)[1] <- c("activity")

test <- cbind(subj_test,test)

colnames(test)[1] <- c("subject_ID")

test$data_set <- rep(c("test"))

## Import the train data set, name the colummns, and create a single table with 
## subject, activity, and readings.

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")

y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

subj_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

names(x_train) <- x_vec$V2

train <- cbind(y_train,x_train)

colnames(train)[1] <- c("activity")

train <- cbind(subj_train,train)

colnames(train)[1] <- c("subject_ID")

## Add columns indicating whether the data is train or test

train$data_set <- rep(c("train"))

test$data_set <- rep(c("test"))

## Filter based on the mean, std; retrieve only the columns with "mean()", 
## "std()", "activity", "subject_ID", or "data_set"

train_filter <- train[, grep("mean\\(\\)|std\\(\\)|activity|subject_ID|data_set", colnames(train), ignore.case = TRUE)]

test_filter <- test[, grep("mean\\(\\)|std\\(\\)|activity|subject_ID|data_set", colnames(test), ignore.case = TRUE)]

## Merge the train and test data sets into a single table

complete <- rbind(train_filter,test_filter)

## Replace activity factors with descriptive names

complete$activity[complete$activity==1] <- rep(c("WALKING"))
complete$activity[complete$activity==2] <- rep(c("WALKING_UPSTAIRS"))
complete$activity[complete$activity==3] <- rep(c("WALKING_DOWNSTAIRS"))
complete$activity[complete$activity==4] <- rep(c("SITTING"))
complete$activity[complete$activity==5] <- rep(c("STANDING"))
complete$activity[complete$activity==6] <- rep(c("LAYING"))

## Make the variable names more readable

names(complete) <- gsub("tBodyAcc", "TimeBodyAccelerometer", names(complete))
names(complete) <- gsub("tGravityAcc", "TimeGravityAccelerometer", names(complete))
names(complete) <- gsub("Mag", "Magnitude", names(complete))
names(complete) <- gsub("tBodyGyro", "TimeBodyGyroscope", names(complete))
names(complete) <- gsub("fBodyAcc", "FreqBodyAccelerometer", names(complete))
names(complete) <- gsub("fBodyBodyGyro", "FreqBodyBodyGyroscope", names(complete))
names(complete) <- gsub("fBodyBodyAcc", "FreqBodyBodyAccelerometer", names(complete))

## Create a new data set with the average of each variable 
## for each activity and each subject

cAvg <- aggregate(complete[,3:68], by=list(subject_ID = complete$subject_ID, activity = complete$activity), FUN=mean)

