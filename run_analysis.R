run_analysis <- function() {
        # get features and clean names
        features<-read.table("./UCI HAR Dataset/features.txt")
        features[,2]<-gsub('[-()]', '', features[,2])
        selected_features_idx <- grep(".*mean.*|.*std.*", features[,2])
        features<-features[selected_features_idx,2]
        
        # get activity labels
        activity_label<-read.csv("./UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
        
        # get train data
        train_x<-read.table("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
        train_y<-read.table("./UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
        train_subject<-read.table("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
        train_activity<-sapply(train_y$V1,function(x) as.character(activity_label$V2[x]))
        train_set<-cbind(train_subject,train_activity,train_x[,selected_features_idx])
        colnames(train_set) <- c( "subject", "activity",features)
        
        # get test data
        test_x<-read.table("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
        test_y<-read.table("./UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
        test_subject<-read.table("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
        test_activity<-sapply(test_y$V1,function(x) as.character(activity_label$V2[x]))
        test_set<-cbind(test_subject,test_activity,test_x[,selected_features_idx]) 
        colnames(test_set) <- c( "subject", "activity",features)
        
        # merge train and test sets
        merged_data<-rbind(train_set,test_set)
        
        # generate tidy_data
        library(plyr)
        tidy_data <- ddply(merged_data, .(subject, activity), function(data) colMeans(data[,-c(1,2)]))
        names(tidy_data)[-c(1,2)] <- paste0("Mean", names(merged_data)[-c(1,2)])
        #tidy_data<-sapply(split(merged_data, list(merged_data$subject,merged_data$activity)),function(data) colMeans(data[,-c(1,2)]))
        #tidy_data<-t(tidy_data)
        
        # write tiday_data to txt file
        write.table(tidy_data, "tidy_data.txt", row.names = FALSE)
        
        # return data
        tidy_data
}
