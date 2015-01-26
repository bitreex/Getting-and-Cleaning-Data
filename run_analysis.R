run_analysis <- function() {
    
    # 1. Merging the training and the test sets to create one data set.
    
    ## Reading 'test' data
    test_label <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names="label")
    test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names="subject")
    test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
    ## Reading 'train' data
    train_label <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names="label")
    train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names="subject")
    train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
    ## Combining the datasets to create one dataset
    data <- rbind(cbind(test_subject, test_label, test_data), cbind(train_subject, train_label, train_data))
    
    # 2. Extracting only the measurements on the mean and standard deviation for each measurement. 
    
    ## Reading the features
    features <- as.data.frame(readLines("./UCI HAR Dataset/features.txt"), stringsAsFactors = FALSE)
    ## Label the column name
    colnames(features) <- "Feature"
    ## Leaving only 'mean' & 'standard deviation' for each measurement
    features_mean_std <- grep("mean|std", features$Feature)
    
    # 3. Uses descriptive activity names to name the activities in the data set
    
    act_df <- as.data.frame(readLines("./UCI HAR Dataset/activity_labels.txt"), stringsAsFactors = FALSE)
    act_vec <- as.data.frame(sapply(strsplit(act_df[,1], " "), function(elem) return(elem[2])))
    act_vec <- cbind(1:6, act_vec)
    colnames(act_vec) <- c("activity_label", "activity_description")
    
    # 4. Appropriately labels the data set with descriptive variable names
    
    features_df <- as.data.frame(readLines("./UCI HAR Dataset/features.txt"), stringsAsFactors = FALSE)
    features_vec <- as.data.frame(sapply(strsplit(features[,1], " "), function(elem) return(elem[2])))
    features_vec <- cbind(1:561, features_vec)
    colnames(features_vec) <- c("feature_index", "feature_description")
    
    # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    
    tidy_data <- aggregate(data['value'], by=list(subject=data$feature_index, activity=data$activity_description, variable=data$variable), FUN=mean)
    names(tidy_data) <- sub('value', 'average', names(tidy_data))
    write.table(tidy_data, 'tidy_data.txt', row.names=FALSE)
}
