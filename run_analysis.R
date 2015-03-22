
##########################################################################################################
## Coursera Getting and Cleaning Data Course Project
## Bartosz £abiszak
## 2015-03-22
# runAnalysis.r File Description:
# This script will perform the following steps on the UCI HAR Dataset downloaded from
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##########################################################################################################

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('/Users/Bartek/Desktop/UCI HAR Dataset/')

#cheacking for requierd packages

if (!require("plyr")) {
  install.packages("plyr")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}
require("plyr")
require("dplyr")
# merging existing data sets (training and test) to create new data set

x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/Y_train.txt")
subject_train <- read.table("train/subject_train.txt")

x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/Y_test.txt")
subject_test <- read.table("test/subject_test.txt")


# creating one x data set
x_data <- rbind(x_train,x_test)

# creating one y data set
y_data <- rbind(y_train,y_test)

# creating "subject" data set
subject_data <- rbind(subject_train,subject_test)

# Extract only the measurements on the mean and standard deviation for each measurement
features <- read.table("features.txt")

# get only columns with mean() or std() in their names
mean_and_std_features <- grep("mean|std", features[,2])

# subset desired data
x_data <- x_data[,mean_and_std_features]

names(x_data) <- features[mean_and_std_features, 2]

# Use descriptive activity names to name the activities in the data set
activities <- read.table("activity_labels.txt")

# update values with correct activity names
y_data[, 1] <- activities[y_data[, 1], 2]

names(y_data)<- "activity"

# correct column name
names(subject_data) <- "subject"

#bind all data
# update subject name correctly
all_data <- cbind(x_data, y_data, subject_data)

# clearing variable names and making them more descriptive
colNames <- colnames(all_data)

for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames(all_data) = colNames





# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject
###############################################################################
# 66 <- 68 columns but last two (activity & subject)
averages_data <- ddply(all_data, .(subject, activity), function(x) colMeans(x[, 1:66]))
write.table(averages_data, "averages_data.txt", row.name=FALSE)

View(all_data)
