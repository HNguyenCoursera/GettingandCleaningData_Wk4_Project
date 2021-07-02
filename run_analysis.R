

# Check the files in directory 
list.files("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4")
testfilelist <- paste(testpath, testfilelist, sep = "/")

library(data.table)
#library(dplyr)
featureNames <- read.table("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/UCI HAR Dataset/features.txt")
activityLabels <- read.table("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/UCI HAR Dataset/activity_labels.txt", header = FALSE)

# read the training data
subjectTrain <- read.table("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/UCI HAR Dataset/train/X_train.txt", header = FALSE)

# read the test data
subjectTest <- read.table("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/UCI HAR Dataset/test/X_test.txt", header = FALSE)

# 1 - Merges the training and the test sets to create one data set:
# use combine the respective data in training and test data sets corresponding to subject, activity and features. 
# The results are stored in subject, activity and features

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Naming the columns
colnames(features) <- t(featureNames[2])

# Merge data together and stored in completeData
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

# 2 - Extracts only the measurements on the mean and standard deviation 
# for each measurement. 
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# dimension of complete Data: 
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

# dimension of required Columns:
extractedData <- completeData[,requiredColumns]
dim(extractedData)

# 3 - Uses descriptive activity names to name the activities in the data set

# change activity field in extractedData type from numeric to character
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

# updated activity names in the activity variable
extractedData$Activity <- as.factor(extractedData$Activity)

# 4 - Appropriately labels the data set with descriptive variable names.
# first checking the vaiable names in extractedData
names(extractedData)

# based on the output, data showed some acronyms can be replaced for easy reading/following such as:
# "f" can be replaced with Frequency
# "t" can be replaced with Time and so on

names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

# checking the vaiable names in extractedData again
names(extractedData)

# 4 - From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# setting Subject as a factor variable
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# create a data set with average for each activity and subject
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

# creates a second, independent tidy data set  
write.table(tidyData, file = "C:/Users/nguyHT/Coursera_Training/10_Courses_Specialization_DataScience_JohnHopkins/03_Getting_and_Cleaning_Data/Week4/tidyData.txt", row.names = FALSE)

