# Getting-and-Cleaning-Data-Project--Assignment4
 ##Getting and Cleaning data Coursera Course Project
## Solutions
# Set up preprocessing libraries and temporary folders
# Clean up the working environment and load plyr,data.table and dplyr libraries
rm(list = ls(all = TRUE))
library(plyr)
library(data.table)
library(dplyr)

# create temporay folder and download the zip file in this folder
temp<-tempfile()
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)

#Create a list of variables that are applicable for this data set
unzip(temp, list = TRUE)

# Files in the Inertial Signals folders are not requiredfor this project, so are not used in the solution.
# The variables applicable for this data set are set out  below.
# X_test	
# Y_test
# Subject_test
# X_train
# Y_train
# Subject_train
#	Features

# Obtain the variables from the temporary folder
YTest <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))
XTest <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))
SubjectTest <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))
YTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))
XTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))
SubjectTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"))
Features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"))

#  Remove the temporary file
unlink(temp) 

# Fix Column Names:
 colnames(XTrain) <- t(Features[2])
 colnames(XTest) <- t(Features[2])

# Merge and Align XTrain and YTrain sets
# Merge and Align XTest and YTest sets
 
 XTrain$activities <- YTrain[, 1]
 XTrain$participants <- SubjectTrain[, 1]
 XTest$activities <- YTest[, 1]
 XTest$participants <- SubjectTest[, 1]
 
 # Solution1 - Merges the training and the test sets to create one data set.
  MDSet<-rbind(XTrain, XTest)
 duplicated(colnames(MDSet))
 
 # MDSet is the required single data set from the merger of training and test sets
 MDSet <- MDSet[, !duplicated(colnames(MDSet))]
 
 # Solution2 - Extracts only the measurements on the mean and standard deviation for each measurement.
 # Mean
 Mean <- grep("mean()", names(MDSet), value = FALSE, fixed = TRUE)
 # include  mean values of 555 : 559 as they associated with the gravity terms
 Mean <- append(Mean, 471:477)
 InstrumentMeanMatrix <- MDSet[Mean]
 
 # Standard Deviation
 STD <- grep("std()", names(MDSet), value = FALSE)
 InstrumentSTDMatrix <- MDSet[STD]    
 
 # Solution3 - Use descriptive activity names to name the activities in the data set
 
 # Use the class to replace the strings
 MDSet$activities <- as.character(MDSet$activities)
 MDSet$activities[MDSet$activities == 1] <- "Walking"
 MDSet$activities[MDSet$activities == 2] <- "Walking Upstairs"
 MDSet$activities[MDSet$activities == 3] <- "Walking Downstairs"
 MDSet$activities[MDSet$activities == 4] <- "Sitting"
 MDSet$activities[MDSet$activities == 5] <- "Standing"
 MDSet$activities[MDSet$activities == 6] <- "Laying"
 MDSet$activities <- as.factor(MDSet$activities)
 
 # Solution4 -  Appropriately label the data set with descriptive variable names.
 # We retain the five acronyms used in names MDSet data set
 # ACC for Accelerator
 # Mag for Magnitude
 # Gyro for Gyroscope
 # ^t for time
 # ^f for frequency
 # use gsub instead of list to process acronyms in the MDSet data set
 names(MDSet) <- gsub("Acc", "Accelerator", names(MDSet))
 names(MDSet) <- gsub("Mag", "Magnitude", names(MDSet))
 names(MDSet) <- gsub("Gyro", "Gyroscope", names(MDSet))
 names(MDSet) <- gsub("^t", "time", names(MDSet))
 names(MDSet) <- gsub("^f", "frequency", names(MDSet))
 
 # Label participants names in MDSet data set
 MDSet$participants<-as.character(MDSet$participants)
 MDSet$participants<-as.character(MDSet$participants)
 MDSet$participants[MDSet$participants == 1] <- "Participant 1"
 MDSet$participants[MDSet$participants == 2] <- "Participant 2"
 MDSet$participants[MDSet$participants == 3] <- "Participant 3"
 MDSet$participants[MDSet$participants == 4] <- "Participant 4"
 MDSet$participants[MDSet$participants == 5] <- "Participant 5"
 MDSet$participants[MDSet$participants == 6] <- "Participant 6"
 MDSet$participants[MDSet$participants == 7] <- "Participant 7"
 MDSet$participants[MDSet$participants == 8] <- "Participant 8"
 MDSet$participants[MDSet$participants == 9] <- "Participant 9"
 MDSet$participants[MDSet$participants == 10] <- "Participant 10"
 MDSet$participants[MDSet$participants == 11] <- "Participant 11"
 MDSet$participants[MDSet$participants == 12] <- "Participant 12"
 MDSet$participants[MDSet$participants == 13] <- "Participant 13"
 MDSet$participants[MDSet$participants == 14] <- "Participant 14"
 MDSet$participants[MDSet$participants == 15] <- "Participant 15"
 MDSet$participants[MDSet$participants == 16] <- "Participant 16"
 MDSet$participants[MDSet$participants == 17] <- "Participant 17"
 MDSet$participants[MDSet$participants == 18] <- "Participant 18"
 MDSet$participants[MDSet$participants == 19] <- "Participant 19"
 MDSet$participants[MDSet$participants == 20] <- "Participant 20"
 MDSet$participants[MDSet$participants == 21] <- "Participant 21"
 MDSet$participants[MDSet$participants == 22] <- "Participant 22"
 MDSet$participants[MDSet$participants == 23] <- "Participant 23"
 MDSet$participants[MDSet$participants == 24] <- "Participant 24"
 MDSet$participants[MDSet$participants == 25] <- "Participant 25"
 MDSet$participants[MDSet$participants == 26] <- "Participant 26"
 MDSet$participants[MDSet$participants == 27] <- "Participant 27"
 MDSet$participants[MDSet$participants == 28] <- "Participant 28"
 MDSet$participants[MDSet$participants == 29] <- "Participant 29"
 MDSet$participants[MDSet$participants == 30] <- "Participant 30"
 MDSet$participants <- as.factor(MDSet$participants)

 
 # Solution5 - Use data set in step 4, to create a second, independent tidy data set
 #             with the average of each variable for each activity and each subject. 
 MDSet.dt <- data.table(MDSet)
 #This takes the mean of every column broken down by participants and activities
 TidyData <- MDSet.dt[, lapply(.SD, mean), by = 'participants,activities']
 write.table(TidyData, file = "Tidy.txt", row.names = FALSE)
 
 
