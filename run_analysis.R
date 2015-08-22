#Filename: run_analysis.R
#Author: Gangadhar Nittala
#Coursera course: GetData-031

#@fileName - the filename which has a large data (typically > 50MB)
#@header - does the file have any header, default=FALSE
#@commentChar - are there comments in the file, default=""
#@return - data.frame that has the data in the file
readLargeData <- function (fileName,header=FALSE,commentChar="") 
{
  #Approach to read large files faster borrowed from
  #http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html
  df.5rows   <- read.table(fileName, header = header, nrows = 5)
  df.classes <- sapply(df.5rows, class)
  #not using the nrows=xx option to keep the code generic
  df         <- read.table(fileName,header = header, 
                           colClasses = df.classes,comment.char=commentChar)
  df
}

#Activities performed by the participants, like walking, standing etc.
activities<-read.table("./UCI HAR Dataset/activity_labels.txt",sep=" ",stringsAsFactors = FALSE)
names(activities)<- c("activityID", "activity")

#Features of the data. These are numeric features collected for the participants
#i.e. these are the column names for the training and the testing data sets
#Ideally, this should have been named measures, so that the column names could have been
#measureID and measure - which is what these 'features' are
features <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
names(features) <- c("featureID","feature")


x.train <- readLargeData("./UCI HAR Dataset/train/X_train.txt")
x.test  <- readLargeData("./UCI HAR Dataset/test/X_test.txt")
#y data is the labels of the activities performed
#by each of the participant i.e. walking, sitting etc. 
y.train <- read.table("./UCI HAR Dataset/train/y_train.txt")
y.test  <- read.table("./UCI HAR Dataset/test/y_test.txt")

#setting meaningful names for the columns of x and y data frames
# X data - contains the values for the 561 features measured for each participant
# Y data - contains the activities that each participant did to get the measures
# Total X data == (x.train+x.test) == 2947 + 7352 == (y.train+y.test) == Total Y data
# X columns == 561 == Total number of features
names(x.train) <- c(features$feature)
names(x.test)  <- c(features$feature)
names(y.train) <- c("activityID")
names(y.test)  <- c("activityID")

#Question 1: 
#merge the x training and test data
x.all  <- rbind(x.train,x.test)
y.all  <- rbind(y.train,y.test)
#Answer to Question#1
xy.all <- cbind(x.all,y.all)
#cleanup data
rm(x.train,x.test,x.all)
rm(y.train,y.test,y.all)

#Question 2:
#Extract only the mean and standard deviation measurements from
#the merged data. Using dplyr, I noticed duplicate columns, so
#first get data that has only the required columns and then use
#dplyr functions. Unfortunately, select is unable to work with 
#duplicated columns in the data-set
xy.colnames<-colnames(xy.all)
#there are column names that have the word mean in their parameters
#like this: angle(Y,gravityMean). These observations are invalid
#To avoid a complicated regex and to keep things simple, just use the
#fixed=TRUE parameter to search for mean() and std()
meanCols <- grep("mean()",tolower(xy.colnames),fixed=TRUE)
stdCols  <- grep("std()" ,tolower(xy.colnames),fixed=TRUE)
#Answer to Question#2
xy.meanstd <- xy.all[,c(meanCols,stdCols)]
#cleanup
rm(meanCols,stdCols)

#Question 3:
#Uses descriptive activity names to name the activities in the 
#'complete' data set
activities_factor=factor(activities$activity)
#Answer to Question#3
xy.all$activity<-activities_factor[xy.all$activityID]
#reset xy.colnames, since there is a new column added
xy.colnames <- colnames(xy.all)
#store this tmp variable to use for the Qn# 5
xy.tmp <- xy.all

#Question 4:
#Appropriately labels the data set with descriptive variable names.
#The columns for the data is to be cleaned to make it a bit easier to read
names(xy.all) <- gsub("tBodyAcc"   ,"timeBodyAcceleration" , names(xy.all))
names(xy.all) <- gsub("tBodyGyro"  ,"timeBodyGyroscopeOrientation", names(xy.all))
names(xy.all) <- gsub("tGravityAcc","timeGravityAcceleration", names(xy.all))
names(xy.all) <- gsub("fBodyBody"  ,"frequencyBody", names(xy.all))
names(xy.all) <- gsub("fBodyAcc","frequencyBodyAccelerationSignal", names(xy.all))
names(xy.all) <- gsub("fBodyGyro","frequencyBodyGyroscopeOrientation",names(xy.all))
names(xy.all) <- gsub("mean","Mean", names(xy.all))
names(xy.all) <- gsub("std","StandardDeviation",names(xy.all))
names(xy.all) <- gsub("Mag","Magnitude", names(xy.all))
names(xy.all) <- gsub("mad","MAD",names(xy.all))
names(xy.all) <- gsub("iqr","IQR",names(xy.all))
names(xy.all) <- gsub("sma","SMA",names(xy.all))

#Question 5:
#From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.
subject.train <- read.table("./UCI HAR Dataset/train/subject_train.txt", stringsAsFactors=FALSE)
subject.test  <- read.table("./UCI HAR Dataset/test/subject_test.txt",stringsAsFactors=FALSE)
subject.all   <- rbind(subject.train,subject.test)
names(subject.all) <- c("subjectID")
#Add the subject to the xy.tidyData to build the aggregate
#Not using xy.all, since the column names are very long - for the write.table in question# 5
data.all <- cbind(xy.tmp,subject.all)
#xy.tmp is no longer required
rm(xy.tmp)

library(reshape2)
#we need to aggregate by the activity-name and the subjectID
#10299 observations melted using 2 observations as IDs to the long format
# Wide Format Data
#|observation#|tBodyAccMean-x|tBodyAccMean-y|....|activity|subjectID|
#|1           |0.288|-0.2|...|Walking|1|
#|2           |..|..|
#Melted into the Long Format data below
#|activity|subjectID|variable|value|
#|Walking|1|timeBodyAccMean-x|0.288|
#|Walking|2|...
melt.data.all      <- melt(data.all,id=c("activity","subjectID"))
#Get the averages of all the measurements (the variable)
#Answer to the Question# 5
aggregate.data.all <- dcast(melt.data.all, ... ~ variable,mean)

#write the data to a file
#181 rows each with 480 columns
write.table(aggregate.data.all, file="gngdhr-getdata-031.txt", row.name=FALSE)
#cleanup of large data
rm(melt.data.all)
