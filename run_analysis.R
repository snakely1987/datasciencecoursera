#1.Merges the training and the test sets to create one data set.
TrainingSet=read.table("UCI HAR Dataset/train/X_train.txt")
TestSet=read.table("UCI HAR Dataset/test/X_test.txt")
mergedataset=rbind(TrainingSet,TestSet) #merge train and test dataset




#2.Extracts only the measurements on the mean and standard deviation for each measurement.
library(dplyr)
feature=read.table("UCI HAR Dataset/features.txt")
meanandstd=mergedataset[,grep("mean|std",feature[,2])]




#3.Uses descriptive activity names to name the activities in the data set
TrainingLabels=read.table("UCI HAR Dataset/train/y_train.txt")
TestLabels=read.table("UCI HAR Dataset/test/y_test.txt")
mergedatalables=rbind(TrainingLabels,TestLabels)#merge train and test activity labels
Activity_Labels=factor(mergedatalables[,1],labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING") )
mergedataset=cbind(Activity_Labels,mergedataset)




#4.Appropriately labels the data set with descriptive variable names.
names(mergedataset)[2:ncol(mergedataset)]=as.character(feature[,2])





#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
splitbyactivity=split(mergedataset,mergedataset[,1])
meanbyact=vector()
for (i in 1:6)
{
      result=sapply(splitbyactivity[[i]],mean,na.rm=T)
      meanbyact=rbind(meanbyact,result)
}
meanbyact=meanbyact[,-1]
rownames(meanbyact)=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")



subject_train=read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test=read.table("UCI HAR Dataset/test/subject_test.txt")
mergesubject=rbind(subject_train,subject_test)#merge train and test subject labels
mergedataset=cbind(mergesubject,mergedataset)
splitbysub=split(mergedataset,mergedataset[,1])
meanbysub=vector()
for (i in 1:30)
{
      result=sapply(splitbysub[[i]],mean,na.rm=T)
      meanbysub=rbind(meanbysub,result)
}
meanbysub=meanbysub[,-(1:2)]#the first 2 columns are subject and activity labels
rownames(meanbysub)=paste("subject",rep(1:30))


finalresult=rbind(meanbyact,meanbysub)