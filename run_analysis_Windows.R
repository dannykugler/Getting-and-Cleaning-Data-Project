# Program run_analysis.R
# Read in data from experiment 

# library("dplyr", lib.loc="~/R/win-library/3.2")

# Read labels of type of activity - name variables
activity.labels<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/activity_labels.txt")
names(activity.labels) <- c("activity.id", "activity")

# Read text file containing subject id from test group
# Name variable to subject id
subject.test<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt")
subject.test<-rename(subject.test, subject.id = V1)

# read text file containing y data from test group
y.test<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt")
y.test<-rename(y.test, activity.id = V1)

x.test<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt")

# Read text file containing subject id from training group
# Rename variable to subject id
subject.train<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt")
subject.train<-rename(subject.train, subject.id = V1)

# Read text file containing y data from training group
y.train<-read.table("c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt")
y.train<-rename(y.train, activity.id = V1)

# Combine Subject Information, Y information and Raw Data from Test Group
test.data <- cbind(subject.test, y.test, x.test)

# Combine Subject Information, Y information and Raw Data from Training Group
train.data <- cbind(subject.train, y.train, x.train)

# Combine Test and Training Group Data
test.train.data<-merge(test.data, train.data, all=TRUE)

# Identify the variable names containing mean or std so that the
# merged test and training data sets can be subset to only contain the id variables
# and any variables that are means or standard deviations

# create link to the features.txt file containing the list of all features
features.file <- "c:/users/danny/Google Drive/Computing/DataScience/JohnsHopkinsCoursera/Getting and Cleaning Data/UCI HAR Dataset/features.txt"
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
measurement.data<-test.train.data[,c(1,2,proc.vars$V1+2)]
measurement.data$activity.id<-activity.labels$activity[measurement.data$activity.id]

#name.backup <- names(measurement.data)
#names(measurement.data)[3:ncol(measurement.data)] <- proc.vars$feature.names

# Read in file with more explanitory variable names
new.names.meas.tidy<-read.csv("namesmeastidy.csv")
# Set variables to new more explanitory variable names
names(measurement.data) <- new.names.meas.tidy$New.Name

# Check to make sure the groups are ok
measurement.data %>% group_by(Activity) %>% summarise(N=n()) %>% print %>% summarise(sum(N))

# Create tidy data set with mean by Subject and Activity for all variables of interest
meas.tidy<-measurement.data %>% group_by(Subject.ID, Activity) %>% summarise_each(funs(mean))
