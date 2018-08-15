#Getting and Cleaning Data Course Project
#1.Merges the training and the test sets to create one data set.
library(dplyr)
setwd("C:/Users/zhangx01/Documents/data science")
TrainingSet=read.table("UCI HAR Dataset/train/X_train.txt")
TestSet=read.table("UCI HAR Dataset/test/X_test.txt")
mergedataset=rbind(TrainingSet,TestSet) #merge train and test dataset

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
feature=read.table("UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)[,2]
featureindex=grep("mean|std",feature)
mergedataset=mergedataset[,c(featureindex)]

#3.Uses descriptive activity names to name the activities in the data set
TrainingLabels=read.table("UCI HAR Dataset/train/y_train.txt")
TestLabels=read.table("UCI HAR Dataset/test/y_test.txt")
mergedatalables=rbind(TrainingLabels,TestLabels)#merge train and test activity labels
Activity_Labels=factor(mergedatalables[,1],labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING") )
mergedataset=cbind(Activity_Labels,mergedataset)

#4.Appropriately labels the data set with descriptive variable names.
names(mergedataset)[-1]=feature[featureindex]

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

subject_train=read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test=read.table("UCI HAR Dataset/test/subject_test.txt")
mergesubject=rbind(subject_train,subject_test)#merge train and test subject labels
mergedataset=cbind(mergesubject,mergedataset)
names(mergedataset)[1]="Subject_Labels"
groupdata=group_by(mergedataset,Activity_Labels,Subject_Labels)
finalresult=summarise_all(groupd,funs(mean))
write.table(finalresult,"finalresult.txt")