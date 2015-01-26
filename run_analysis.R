## R Script file for Coursera_Getting and Cleaning Data

##  You should create one R script called run_analysis.R that does the following.
##  1. Merges the training and the test sets to create one data set.
##  2. Extracts only the measurements on the mean and standard deviation for each measurement.
##  3. Uses descriptive activity names to name the activities in the data set
##  4. Appropriately labels the data set with descriptive variable names.
##  5. From the data set in step 4, creates a second, independent tidy data set with the average of each
##  variable for each activity and each subject.


## Read in all of the datasets - I kept it in the original file structure so I am changing working directories
## 

## Read in the subjects, features, and activities for the train data
setwd("C:/Users/rr046302/Documents/Bill's Stuff/Coursera/Getting and Cleaning Data/Getting and Cleaning Data Course Project/gettingandcleaningdata/UCI HAR Dataset/train")
train_subjects <- read.table("subject_train.txt")
train_features <- read.table("X_train.txt")
train_activity <- read.table("y_train.txt")

## Read in the subjects, features, and activities for the test data
setwd("C:/Users/rr046302/Documents/Bill's Stuff/Coursera/Getting and Cleaning Data/Getting and Cleaning Data Course Project/gettingandcleaningdata/UCI HAR Dataset/test")
test_features <- read.table("X_test.txt")
test_subjects <- read.table("subject_test.txt")
test_activity <- read.table("y_test.txt")

## rbind the test and train datasets
features <- rbind(test_features, train_features)
subjects <- rbind(test_subjects, train_subjects)
activities <- rbind(test_activity, train_activity)

merged_data <- cbind(features, subjects, activities)

setwd("C:/Users/rr046302/Documents/Bill's Stuff/Coursera/Getting and Cleaning Data/Getting and Cleaning Data Course Project/gettingandcleaningdata/UCI HAR Dataset")
features_names <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt", col.names = c("activity", "activity_name"))

colnames(merged_data) <- features_names[,2]

colnames(merged_data)[562] <- "subject"

colnames(merged_data)[563] <- "activity"

## select only the variables that include mean or standard deviation in their variable name
## also select with the subject and activity columns

selected_data <- merged_data[,grepl("mean|std|subject|activity", names(merged_data))]

## add the activity names back to the activity variable

require(plyr)
selected_data <- join(selected_data, activity_labels, by = "activity", match = "first")

# lean up the variable names
names  = colnames(selected_data)
for (i in 1:length(names)) 
{
  names[i] = gsub("\\()","",names[i])
  names[i] = gsub("-std$","StdDev",names[i])
  names[i] = gsub("-mean","Mean",names[i])
  names[i] = gsub("^(t)","time",names[i])
  names[i] = gsub("^(f)","freq",names[i])
  names[i] = gsub("([Gg]ravity)","Gravity",names[i])
  names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",names[i])
  names[i] = gsub("[Gg]yro","Gyro",names[i])
  names[i] = gsub("AccMag","AccMagnitude",names[i])
  names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",names[i])
  names[i] = gsub("JerkMag","JerkMagnitude",names[i])
  names[i] = gsub("GyroMag","GyroMagnitude",names[i])
};

colnames(selected_data) <- names

final_data <- selected_data %>% group_by(subject, activity_name) %>% summarise_each(funs(mean))

write.table(final_data,file = "finalDataSet.txt",row.names = FALSE)