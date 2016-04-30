## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)
library(reshape2)

#currentdir <- "UCI HAR Dataset"
# Read Activity Labels & features files
#setwd(filedirectory)
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity_labels <- activity_labels[,2] # only interested on the labels i.e 2nd column

features <- read.table("./UCI HAR Dataset/features.txt")
features <- features[,2] # only interested on feature labels

get_features <- grepl("mean|std",features) # return logical set index of mean & std labels only

# load all X, y and subject test files to be merge later

# represents the 30 human subjects numbered by 1:30
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
# represents all x & y data captured
test_X <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt")

# load all X, y and subject train files to be merge later

# represents the 30 human subjects numbered by 1:30
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
# represents all x & y data captured
train_X <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")

# let's give column name for test & train data first
# start with test data
names(test_X) <- features # map text_X columns names with feature factor list

# we are only interested with mean and std test data only
test_X <- test_X[,get_features]

# we need to change the y test column into a meaningful data from activity labels factors list
test_y[,2] = activity_labels[test_y[,1]] # replace col 2 by mapping the "key" join

# give appropriate col names 
names(test_y) = c("Activity_ID", "Activity_Label")
names(subject_test) = "Subject"

# now it's time to combind all the column for test
test_clean <- cbind(as.data.table(subject_test), test_y, test_X)

# let's do train data
names(train_X) <- features # map text_X columns names with feature factor list

# we are only interested with mean and std test data only
train_X <- train_X[,get_features]

# we need to change the y test column into a meaningful data from activity labels factors list
train_y[,2] = activity_labels[train_y[,1]] # replace col 2 by mapping the "key" join

# give appropriate col names 
names(train_y) = c("Activity_ID", "Activity_Label")
names(subject_train) = "Subject"

# now it's time to combind all the column
train_clean <- cbind(as.data.table(subject_train), train_y, train_X)

# add it all up!!
data = rbind(test_clean, train_clean)

# let's tidy up the data to be more readable
# let's get the colnames of data

my_data_col <- colnames(data)
# get the no of columns
my_data_col_len <- length(my_data_col)
# we want to collapse/melt the data with the first 3 column as the key variables
new_data_col <- my_data_col[4:my_data_col_len] # all col except the first 3
# using melt to collapse
new_data      = melt(data, id = my_data_col[1:3], measure.vars = new_data_col)
# get the average for each variable
tidy_data   = dcast(new_data, subject + Activity_Label ~ variable, mean)

# write to text file
write.table(tidy_data, file = "./tidy.txt")



