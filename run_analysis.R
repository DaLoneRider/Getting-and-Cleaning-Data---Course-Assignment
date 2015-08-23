# The first step is to merge the training and the test sets to create one data set

data_set <- rbind(read.table("X_test.txt"), read.table("X_train.txt"))


# The second step is to extract only the measurements of the mean and standard deviation for each measurement
# To do this we need to identify which columns contain measurements of mean and standard deviation by 
# referring to the "features.txt" file which contains the names of all columns

features <- read.table("features.txt", stringsAsFactors = FALSE) 

# Each row in the above table represents a column number and the corresponding column name for the data_set table
# Now we need to identify which of the rows/column names actually contain "mean" or "std" in them to indicate that they represent mean or standard deviation

mean_index <- grep("*mean", features$V2)
std_index <- grep("*std", features$V2)
required_columns_index <- c(mean_index, std_index) # This vector gives us the rows in "features" that indicate columns in "data_set" that present measurements of means or standard deviations
required_columns_index <- sort(required_columns_index)
data_set <- data_set[,c(required_columns_index)]


# Next we need to use descriptive activity names to name the activities in the data set. 

activity_classes <- rbind(read.table("y_test.txt"), read.table("y_train.txt")) # This table gives us the activity which is measured for each row of the data_set
activity_labels <- read.table("activity_labels.txt") # This table extracts the labels that correspond to each of the 6 classes of activities from "activity_labels.txt"
activity_labels <- as.character(activity_labels$V2) # This transformation converts the labels to characters are removes the first column which is not necessary since the labels are ordered anyway
for (i in 1:nrow(activity_classes)) {
  if (activity_classes$V1[i] == 1) {activity_classes$V1[i] <- activity_labels[1]}
  if (activity_classes$V1[i] == 2) {activity_classes$V1[i] <- activity_labels[2]}
  if (activity_classes$V1[i] == 3) {activity_classes$V1[i] <- activity_labels[3]}
  if (activity_classes$V1[i] == 4) {activity_classes$V1[i] <- activity_labels[4]}
  if (activity_classes$V1[i] == 5) {activity_classes$V1[i] <- activity_labels[5]}
  if (activity_classes$V1[i] == 6) {activity_classes$V1[i] <- activity_labels[6]}
} # This loop replaces each of the classes with its appropriate label which can be considered a descriptive activity name
data_set <- cbind(activity_classes, data_set) # This transformation adds the respective descriptive activity names to the begining of the corresponding row with measurements in "data_set"

# Next we need to label the data set with descriptive variable names. To do this we need to select the names of columns that contain "mean" or "std" from features and clean them a little bit so they can become easier to read and more descriptive

column_names <- features$V2[required_columns_index] # These are the names of the 79 variables (columns) we need to label the columns in "data_set"
column_names <- c("activity", column_names) # These are the names of all 80 variables (columns) in the "data_set" table
column_names
column_names <- sub("tBodyAcc", "time domain-body acceleration signal", column_names)
column_names <- sub("tGravityAcc", "time domain-gravity acceleration signal", column_names)
column_names <- sub("body acceleration signalJerk", "body acceleration jerk signal", column_names)
column_names <- sub("tBodyGyro", "time domain-angular velocity", column_names)
column_names <- sub("angular velocityJerk", "angular velocity jerk signal", column_names)
column_names <- sub("body acceleration signalMag", "body acceleration signal magnitude", column_names)
column_names <- sub("gravity acceleration signalMag", "gravity acceleration signal magnitude", column_names)
column_names <- sub("body acceleration jerk signalMag", "body acceleration jerk signal magnitude", column_names)
column_names <- sub("angular velocityMag", "angular velocity magnitude", column_names)
column_names <- sub("angular velocity jerk signalMag", "angular velocity jerk signal magnitude", column_names)
column_names # A look at the column names shows that the columns that have been selected also include "meanFreq" which can not be considered strictly mean, because according to the "features_info.txt" file it is a weighted average. So we will remove it.
freqMean_index <- grep("meanFreq",column_names) # This vector shows us the columns to be removed from "data_set" and labels to be removed from "column_names"
data_set <- data_set[,-c(freqMean_index)] # This is the updated "data_set" with columns whose names contain "freqMean" removed
column_names <- column_names[-c(freqMean_index)] # This is the updated vector of column names with the names that contain "freqMean" removed
column_names
column_names <- sub("fBodyAcc", "frequency domain-body acceleration signal", column_names)
column_names <- sub("signalJerk", "jerk signal", column_names)
column_names <- sub("fBodyGyro", "frequency domain-angular velocity", column_names)
column_names <- sub("signalMag", "signal magnitude", column_names)
column_names <- sub("fBodyBodyAccJerkMag", "frequency domain-body acceleration jerk signal magnitude", column_names)
column_names <- sub("fBodyBodyGyroMag", "frequency domain-angular velocity signal magnitude", column_names)
column_names <- sub("fBodyBodyGyroJerkMag", "frequency domain-angular velocity jerk signal magnitude", column_names)
# After the names of the columns in "data_set" are made more descriptive by replacing the abbreviations with complete descriptions we need to append the vector with the names to the corresponding columns in "data_set"
colnames(data_set) <- column_names


# Finally, we need to create a second,independent tidy data set with the average of each variable for each activity and each subject
  # To do this we need to first append the subjects to the begining of the existing data_set
subjects <- rbind(read.table("subject_test.txt"), read.table("subject_train.txt")) # This table identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
colnames(subjects) <- "subject"
second_data_set <- cbind(subjects, data_set) # The "second_data_set" is almost like the first, but the subjects are added as the first column
library(dplyr)
by_activity <- group_by(second_data_set, activity) # This table is the data set grouped by activity
by_activity_and_subject <- group_by(by_activity, subject, add = TRUE) # This table is the data set grouped by activity and by subject
final_table <- summarise_each(by_activity_and_subject, funs(mean)) # The final table summarises the means of all variables in the second data set grouped by activity and subject
write.table(final_table, "tidy_data_set.txt", row.names = FALSE) # This function writes the results to a text file in my working directory

