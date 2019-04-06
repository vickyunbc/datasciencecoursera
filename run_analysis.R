### This code will read the test and train data and will merge them
### and do further analysis as listed in the Assignment


### First to unzip the data provided in the weblink
### The data will be downloaded to directory called 
### "data"

unzip(zipfile="./data/Dataset.zip",exdir="./data")

### The Dataset.zip will unzip the directory called
### "UCI HAR Dataset"

### Now we will read the training and testing data from 
### UCI HAR Dataset

### UCI HAR Dataset contain "activity_labels.txt", "features.txt", features.txt
### README.txt, directories test and train
### README.txt have all information about what is present in test and train data
### Using that information we can write the following lines of codes

#Reading training tables - xtrain / ytrain, subject train
xtrain = read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"),header = FALSE)
ytrain = read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"),header = FALSE)
subject_train = read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"),header = FALSE)


### Now Reading the testing data with same variables

xtest = read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"),header = FALSE)
ytest = read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"),header = FALSE)
subject_test = read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"),header = FALSE)

#Read the features data
features = read.table(file.path("UCI HAR Dataset", "features.txt"),header = FALSE)
#Read activity labels data
activityLabels = read.table(file.path("UCI HAR Dataset", "activity_labels.txt"),header = FALSE)

#Creating Column Values to the Train Data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityId"
colnames(subject_train) = "subjectId"
#Creating column values to the test data
colnames(xtest) = features[,2]
colnames(ytest) = "activityId"
colnames(subject_test) = "subjectId"
#Creating activity labels value
colnames(activityLabels) <- c('activityId','activityType')

### Now the Step1 of the assignmen i.e. merge the training and 
### testing data

mrg_train = cbind(ytrain, subject_train, xtrain)
mrg_test = cbind(ytest, subject_test, xtest)
#The outcome of step 1 will be merging the tables of both train and test data
setAllInOne = rbind(mrg_train, mrg_test)


#### Now The step 2 of the assignment calculating the mean and SD for each measurement
colNames = colnames(setAllInOne)
#mean and standards of the activityID and subjectID 
mean_and_std = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))
# subset created to get the required dataset
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]


setWithActivityNames = merge(setForMeanAndStd, activityLabels, by='activityId', all.x=TRUE)


# Creating new tidy dataset 
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]


### Finally saving the newly crearted dataset. Final step of the assignment

write.table(secTidySet, "second_tidy_set.txt", row.name=FALSE)


