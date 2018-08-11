#1.Merges the training and the test sets to create one data set.
TrainingSet=read.table("UCI HAR Dataset/train/X_train.txt")         #read trainingset data 
TestSet=read.table("UCI HAR Dataset/test/X_test.txt")               #read testset data
mergedataset=rbind(TrainingSet,TestSet)                             #merge train and test dataset




#2.Extracts only the measurements on the mean and standard deviation for each measurement.
library(dplyr)
feature=read.table("UCI HAR Dataset/features.txt")                  #read variable names
meanandstd=mergedataset[,grep("mean|std",feature[,2])]              #select columns with variable name containing "mean"or"std" 





#3.Uses descriptive activity names to name the activities in the data set
TrainingLabels=read.table("UCI HAR Dataset/train/y_train.txt")      #read activity labels of training 
TestLabels=read.table("UCI HAR Dataset/test/y_test.txt")            #read activity labels of test 
mergedatalables=rbind(TrainingLabels,TestLabels)                    #merge train and test activity labels
Activity_Labels=factor(mergedatalables[,1],labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING") )  #factorize labels
mergedataset=cbind(Activity_Labels,mergedataset)                    #merge activity labels with mergedataset,column 1 is activity lables now





#4.Appropriately labels the data set with descriptive variable names.
names(mergedataset)[2:ncol(mergedataset)]=as.character(feature[,2]) #set column names to be  variable names






#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
splitbyactivity=split(mergedataset,mergedataset[,1])               #split dataset by activity
meanbyact=vector()                                                 #creat a data.frame to record mean of each variable by activity
for (i in 1:6)                                                     #computer the mean of each variable
{
      result=sapply(splitbyactivity[[i]],mean,na.rm=T)
      meanbyact=rbind(meanbyact,result)
}
meanbyact=meanbyact[,-1]                                          #the first 1 columns is activity labels
rownames(meanbyact)=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")  #set the rownames to be "WALKING"-"LAYING"



subject_train=read.table("UCI HAR Dataset/train/subject_train.txt")         #read subject labels of train 
subject_test=read.table("UCI HAR Dataset/test/subject_test.txt")            #read subject labels of test 
mergesubject=rbind(subject_train,subject_test)                              #merge train and test subject labels
mergedataset=cbind(mergesubject,mergedataset)                               #merge subject labels with mergedataset,column 1 is subject labels now
splitbysub=split(mergedataset,mergedataset[,1])                             #split dataset by subject labels
meanbysub=vector()                                                          #creat a data.frame to record mean of each variable by subject labels
for (i in 1:30)                                                             #computer the mean of each variable
{
      result=sapply(splitbysub[[i]],mean,na.rm=T)
      meanbysub=rbind(meanbysub,result)
}
meanbysub=meanbysub[,-(1:2)]                                                #the first 2 columns are subject and activity labels
rownames(meanbysub)=paste("subject",rep(1:30))                              #set the rownames to be "subject 1" - "subject 30"


finalresult=rbind(meanbyact,meanbysub)