

library(dplyr)
##First stp is to download data from the given link


url<-'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

zipfile<-'data_set.zip'

if(!file.exists(zipfile))
  
download.file(url,zipfile,mode="wb")

else
  
print("file already downloaded")

data_path<-'UCI HAR Dataset'

if(!file.exists(data_path))
{unzip(zipfile)}
else
print("file already zipped")

##Reading Data

Train_subject<-read.table(file.path(data_path,"train","subject_train.txt"))
Train_value<-read.table(file.path(data_path,"train","X_train.txt"))
Train_activity<-read.table(file.path(data_path,"train","y_train.txt"))

Test_subject<-read.table(file.path(data_path,"test","subject_test.txt"))
Test_value<-read.table(file.path(data_path,"test","X_test.txt"))
Test_activity<-read.table(file.path(data_path,"test","y_test.txt"))


features <- read.table(file.path(data_path, "features.txt"), as.is = TRUE)

activities<-read.table(file.path(data_path,"activity_labels.txt"))

colnames(activities)<-c("activity_id","activity_label")

##1-Merges the training and the test sets to create one data set.
Train_data<-cbind(Train_subject,Train_value,Train_activity)
test_data<-cbind(Test_subject,Test_value,Test_activity)

activity_dataset<-rbind(Train_data,test_data)

colnames(activity_dataset)<-c("subject", features[,2],"activity")

###2-Extracts only the measurements on the mean and standard deviation for each measurement.


activity_dataset2<-activity_dataset[,grepl("subject|activity|mean|std",colnames(activity_dataset))]

####3-Uses descriptive activity names to name the activities in the data set


activity_dataset2$activity<-factor(activity_dataset2$activity,labels = activities[,2])


###4-Appropriately labels the data set with descriptive variable names.


activity_dataset2_col<-colnames(activity_dataset2)


# remove special characters
activity_dataset2_col <- gsub("[\\(\\)-]", "", activity_dataset2_col)

# expand abbreviations and clean up names
activity_dataset2_col <- gsub("^f", "frequencyDomain", activity_dataset2_col)
activity_dataset2_col <- gsub("^t", "timeDomain", activity_dataset2_col)
activity_dataset2_col <- gsub("Acc", "Accelerometer", activity_dataset2_col)
activity_dataset2_col <- gsub("Gyro", "Gyroscope", activity_dataset2_col)
activity_dataset2_col <- gsub("Mag", "Magnitude", activity_dataset2_col)
activity_dataset2_col <- gsub("Freq", "Frequency", activity_dataset2_col)
activity_dataset2_col <- gsub("mean", "Mean", activity_dataset2_col)
activity_dataset2_col <- gsub("std", "StandardDeviation", activity_dataset2_col)

# correct typo
activity_dataset2_col <- gsub("BodyBody", "Body", activity_dataset2_col)

# use new labels as column names
colnames(activity_dataset2_col) <- activity_dataset2_col


###5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


activity_dataset2Means <- activity_dataset2 %>% group_by(subject,activity) %>% summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(activity_dataset2Means, "tidy_data.txt", row.names = FALSE,quote = FALSE)