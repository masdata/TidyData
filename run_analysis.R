#Read in data tables
Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
Ytest <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
Ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

#1. Merge the training and the test sets to create one data set.
Xdata <- rbind(Xtest,Xtrain)
Ydata <- rbind(Ytest,Ytrain)
subjectdata <- rbind(subjecttest,subjecttrain)

#Relabel column names
Xheadings <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
names(Xdata) <- Xheadings$V2
names(Ydata) <- c("activitynumber")
names(subjectdata) <- c("subject")

#Combine all data
Ysubjectdata <- cbind(subjectdata,Ydata)
alldata <- cbind(Ysubjectdata, Xdata)

#2. Extract only the measurements on the mean and standard deviation for each measurement.
msdheadings <- grep("mean\\(\\)|std\\(\\)",Xheadings$V2,value=TRUE)
selectedheadings <- union(c("subject","activitynumber"),msdheadings)
msddata <- subset(alldata,select=selectedheadings)

#3. Use descriptive activity names to name the activities in the data set
activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
names(activitylabels) <- c("activitynumber","activity")
activitydata <- merge(activitylabels,msddata,by.y="activitynumber", all.x=TRUE)
activitydata <- subset(activitydata,select=-c(activitynumber))

## 4. Appropriately label the data set with descriptive variable names.
## time for t
names(activitydata) <- gsub("^t","time",names(activitydata))
## frequency for f
names(activitydata) <- gsub("^f","frequency",names(activitydata))
## Gyroscope for Gyro
names(activitydata) <- gsub("Gyro","Gyroscope",names(activitydata))
## Accelerometer for Acc
names(activitydata) <- gsub("Acc","Accelerometer",names(activitydata))
## Magnitude for Mag
names(activitydata) <- gsub("Mag","Magnitude",names(activitydata))
## Fix Body repetition
names(activitydata) <- gsub("BodyBody","Body",names(activitydata))

#5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
avgdata <- activitydata %>%
        group_by(subject,activity) %>%
        summarize_each(funs(mean))
write.table(avgdata,"./avgdata.txt",row.names=FALSE)

