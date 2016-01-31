# Program run_analysis.R
#  This program is for the Data Science Class - Getting and Cleaning Data
#  The program works on the Human Activity Recognition Using Smartphones dataset
#   from the Non Linear Complex Systems Laboratory, Genoa Italy

# The output generated will be a tidy data set with average values for the variables that are means and standard deviations
#  of response variablesand will be calculated by subject and activity

# Description of the output data is contained in the codebook meas.tidy.txt

# Read in the test and training data files - 
#   y test/train are the identifiers of the activity for test and training data respectively
#   x test/train are the response variables for the activities for test and training data respectively
#   activity labels are the index code matching y test/training and an indentifier of the activities
#   subject test/train are the subject ids for the test and training data respectively

# dplyr is required so if not loaded should be
# library("dplyr", lib.loc="~/R/win-library/3.2")

#
# Step one of the project - read in test and training data and merge into one file
#

# Read in data from experiment

# Read labels of type of activity - name variables
activity.labels<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/activity_labels.txt")
names(activity.labels) <- c("activity.id", "activity")

# Process data from test group
#
# Read text file containing subject id from test group
# Name variable to subject id
subject.test<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt")
subject.test<-rename(subject.test, subject.id = V1)

# read text file containing y data (activity id) from test group
y.test<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt")
y.test<-rename(y.test, activity.id = V1)

# read text file containing x data (response data) from test group
x.test<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt")

# Combine Subject Information, Y information and Response Data from Test Group
test.data <- cbind(subject.test, y.test, x.test)

# Process data from training group
#
# Read text file containing subject id from training group
# Rename variable to subject id
subject.train<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt")
subject.train<-rename(subject.train, subject.id = V1)

# Read text file containing y data (activity id) from training group
y.train<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt")
y.train<-rename(y.train, activity.id = V1)

# Combine Subject Information, Y information and Raw Data from Training Group
train.data <- cbind(subject.train, y.train, x.train)

# Generate combined test and training data set
# Combine Test and Training Group Data
test.train.data<-merge(test.data, train.data, all=TRUE)

#
# Step two of the project - extract only the mean and standard deviation measurements
#

# Project is to evaluate the mean and standard deviation response varaibles so need to determine the variables to use 
# Identify the variable names containing mean or std
# This will be used to generate a subset of the merged test and training data sets that contains
# the id variables (subject and activity) and any response variables that are means or standard deviations

# create link to the features.txt file containing the list of all features (response variable descriptions)
features.file <- "~/features.txt"
#read the features file and then rename the variable
features <- read.table(features.file)
names(features)[2] <- "feature.names"

# the features file reads in the character data as a factor so change to a 
#  character vector
features$feature.names<-as.character(features$feature.names)

# Find the variable names containing mean or std (for sandard deviation)
#  store those variable names in the data.frame proc.vars
proc.vars<-features[regexpr("mean()|std()", features$feature.names)>0,]

# Subset the merged test and training data set.
# Select only the ID information and the mean and standard deviation variables
#  IDs are the subject id and the activity id
# subset the data
measurement.data<-test.train.data[,c(1,2,proc.vars$V1+2)]

#
# Step 3 of project
# 
# Create more descriptive activity names from the Activity data set
measurement.data$activity.id<-activity.labels$activity[measurement.data$activity.id]

#
# Step 4 of the project
#
# Label the data set with more descriptive variable names
# Created a vector of more descriptive variable names this file is stored as comma delimted
# Read in file with more explanitory variable names
new.names.meas.tidy<-read.csv("~/namesmeastidy.csv")
# Set variables to new more explanitory variable names
names(measurement.data) <- new.names.meas.tidy$New.Name

#
# Step 5 of the project
#
# Create tidy data set with with the average (mean) by Subject and Activity for all variables of interest
meas.tidy.data<-measurement.data %>% group_by(Subject.ID, Activity) %>% summarise_each(funs(mean))