#Assignment: Getting and Cleaning Data Course Project

##The data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

##Files in the repo:
run_analysis.R, 
README.md, 
CodeBook.md

##The assignment
To create a tidy data set with the files provided. This includes merging multiple files, labeling the headings, extracting 
only mean and standard deviations removing abbreviations for clearer descriptions, adding activity descriptions, sorting, 
and averaging by subject and activity.

###First, we read in the files using read.table
####Read in X test table
Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
####Read in X training table
Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
####Read in Y test table
Ytest <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
####Read in Y training table
Ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
####Read in subject test table
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
####Read in subject training table
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

###Then, merge the training and the test sets to create one data set.
####bind X test and X training tables (data)
Xdata <- rbind(Xtest,Xtrain)
####bind Y test and Y training tables (activities)
Ydata <- rbind(Ytest,Ytrain)
####bind subject test and subject training tables
subjectdata <- rbind(subjecttest,subjecttrain)

###Relabel column names
####Read in the features.txt file
Xheadings <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
####Replace headings for Xdata using the features.txt file
names(Xdata) <- Xheadings$V2
####Rename Y heading with a more descriptive title
names(Ydata) <- c("activitynumber")
####Rename subject heading with more descriptive title
names(subjectdata) <- c("subject")

###And combine all data into one table labeled "alldata"
####Combine subject and activity data
Ysubjectdata <- cbind(subjectdata,Ydata)
####Add collected data to the subject and activity descriptive table
alldata <- cbind(Ysubjectdata, Xdata)

###For question 2, we extract only the measurements on the mean and standard deviation for each measurement.
####select only mean and standard deviation headings
msdheadings <- grep("mean\\(\\)|std\\(\\)",Xheadings$V2,value=TRUE)
####choose only the column headings we want to use
selectedheadings <- union(c("subject","activitynumber"),msdheadings)
####use only these headings in the table
msddata <- subset(alldata,select=selectedheadings)

###3. Use descriptive activity names to name the activities in the data set
####Read in the activity description file
activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
####Relabel activity description headings (originally V1 and V2)
names(activitylabels) <- c("activitynumber","activity")
####Merge files to add activity descriptions to the data
activitydata <- merge(activitylabels,msddata,by.y="activitynumber", all.x=TRUE)
####Remove the activity numbers, since the descritpion has been added
activitydata <- subset(activitydata,select=-c(activitynumber))

###4. Appropriately label the data set with descriptive variable names.
####substitute more description in place of abbrevations
#####time for t
names(activitydata) <- gsub("^t","time",names(activitydata))
#####frequency for f
names(activitydata) <- gsub("^f","frequency",names(activitydata))
#####Gyroscope for Gyro
names(activitydata) <- gsub("Gyro","Gyroscope",names(activitydata))
#####Accelerometer for Acc
names(activitydata) <- gsub("Acc","Accelerometer",names(activitydata))
#####Magnitude for Mag
names(activitydata) <- gsub("Mag","Magnitude",names(activitydata))
#####Fix Body repetition
names(activitydata) <- gsub("BodyBody","Body",names(activitydata))

###5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
####load dplyr library
library(dplyr)
####group by subject and activity, then get means by using summarize_each
avgdata <- activitydata %>%
        group_by(subject,activity) %>%
        summarize_each(funs(mean))
####Write the data to a text file
write.table(avgdata,"./avgdata.txt",row.names=FALSE)

