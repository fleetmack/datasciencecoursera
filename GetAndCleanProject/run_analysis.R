setwd("C:/Users/bmack/Google Drive/school/coursera/3 - Getting and Cleaning Data/Week4/UCI HAR Dataset")

library(data.table)
library(dplyr)

###################################################################
###################################################################
## Merges the training and the test sets to create one data set.
###################################################################
###################################################################


#import the names of the features
features <- read.table("features.txt",header=FALSE)
#import the names of the activity labels
activity_labels <- read.table("activity_labels.txt", header=FALSE)

#read in train & test data -- one for subject, features(x), and activity(y)
subject_train   <- read.table("train/subject_train.txt",header=FALSE)
subject_test    <- read.table("test/subject_test.txt",header=FALSE)
features_train  <- read.table("train/X_train.txt",header=FALSE)
features_test   <- read.table("test/X_test.txt",header=FALSE)
activity_train  <- read.table("train/y_train.txt",header=FALSE)
activity_test   <- read.table("test/y_test.txt",header=FALSE)

# Can I Just rbind? Checking to see if values exist in both:
> subject_train %>% distinct(V1)
> subject_test %>% distinct(V1)
# All checks out, I can combine those two, no common V1 values
###################################################################
###################################################################
# Q1: Merges the training and the test sets to create one data set.
###################################################################
###################################################################
subject_comb <- rbind(subject_train, subject_test)
features_comb <- rbind(features_train,features_test)
activity_comb <- rbind(activity_train, activity_test)

#Since we want just a single data set, this data needs a label
colnames(subject_comb) <- "Subject"
colnames(features_comb) <- t(features[2])
colnames(activity_comb) <- "Activity"

#Now combine everything together
mack_data <- cbind(subject_comb,features_comb,activity_comb)

###################################################################
###################################################################
# Q2: Extracts only the measurements on the mean and standard deviation for each measurement.
###################################################################
###################################################################
mack_data2 <-mack_data[,grepl("mean|std|Subject|Activity",names(mack_data))]

###################################################################
###################################################################
# Q3: Uses descriptive activity names to name the activities in the data set
###################################################################
###################################################################
mack_data2 <- merge(mack_data2, activity_labels, by.x="Activity",by.y="V1")
#find the column number of the new column
colnames(mack_data2)[ncol(mack_data2)] <- "ActivityLabel"

###################################################################
###################################################################
# Q4: Appropriately labels the data set with descriptive names.
###################################################################
###################################################################
#Using regular expressions to tidy up the column names based on data from the metadata doc
#ACC= Accelerometer
#Gyro = Gyroscope
#BodyBody = Body
#Mag = Magnitude
#f = Frequency
#t = Time
names(mack_data2)<-gsub("Acc","Accelerometer",names(mack_data2))
names(mack_data2)<-gsub("Gyro","Gyroscope",names(mack_data2))
names(mack_data2)<-gsub("BodyBody","Body",names(mack_data2))
names(mack_data2)<-gsub("Mag","Magnitude",names(mack_data2))
names(mack_data2)<-gsub("^t","Time",names(mack_data2))
names(mack_data2)<-gsub("^f","Frequency",names(mack_data2))
#get rid of all that () crap
names(mack_data2)<-gsub("[Ff]req()","Frequency",names(mack_data2))
names(mack_data2)<-gsub("-[Mm]ean()","Mean",names(mack_data2))
names(mack_data2)<-gsub("-[Ss]td()","StdDev",names(mack_data2))

###################################################################
###################################################################
# Q5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
###################################################################
###################################################################
library(plyr)
mack_data3 <- aggregate(. ~Subject + ActivityLabel, mack_data2, mean)
mack_data3 <- mack_data3[order(mack_data3$Subject,mack_data3$ActivityLabel),]
write.table(mack_data3,file="mack_tidy.txt",row.name=FALSE)