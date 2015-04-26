run_analysis <- function () {

library(reshape2)
library(plyr)
# read test files

# file names
x_test_file <- "./data/test/x_test.txt"
y_test_file <- "./data/test/y_test.txt"
subject_test_file <- "./data/test/subject_test.txt"

# since x file is large, sample first 5 rows to get columns classes and use colClasses option 
# for read.table  
tab5rows <- read.table(x_test_file,sep="",nrows = 5)
classes <- sapply(tab5rows,class)
x_test <- read.table(x_test_file,sep="",colClasses = classes)
y_test <- read.table(y_test_file,sep="")
subject_test <- read.table(subject_test_file,sep="")


# read train files
#file names
x_train_file <- "./data/train/X_train.txt"
y_train_file <- "./data/train/y_train.txt"
subject_train_file <- "./data/train/subject_train.txt"

# since x file is large, sample first 5 rows to get columns classes and use colClasses option 
# for read.table
tab5rows <- read.table(x_train_file,sep="",nrows = 5)
classes <- sapply(tab5rows,class)
x_train <- read.table(x_train_file,sep="",colClasses=classes)
y_train <- read.table(y_train_file,sep="")
subject_train <- read.table(subject_train_file,sep="")

#read features
features_file <- "./data/features.txt"
features <- read.table(features_file,sep="")

#specify feature columns to select. Namely those containing 'mean' or 'std'
features_selected_vec <- grep('mean|std',features$V2)

# add 2 because I have chosen to set column 1 to subject and column 2 to activity. 
features_selected_vec <- features_selected_vec+2  
#selected desired features as well a cols 1 and 2
features_selected_vec <- c(1,2,features_selected_vec) 

#update column names for x_train and x_test
names(x_train) <- features$V2
names(x_test) <- features$V2

#cbind to combine subject_xxx, y_xxx, and x_xxx. Then rbind to combine test and train.
test_combined <- cbind(subject_train,y_train,x_train)
train_combined <- cbind(subject_test,y_test,x_test)
all_combined <- rbind(test_combined,train_combined)


#filter out metrics that don't pertain to std or mean
all_combined_selected <- all_combined[,features_selected_vec]

#update column names for first two columns - subject and activity
names(all_combined_selected)[1:2] <- c("subject","activity")

# change activity to user freindly names

activity_labels <- read.table("./data/activity_labels.txt") # read lables

#activity_labels$V1 <- as.factor(activity_labels$V1) # necc?
#all_combined_selected$activity <- as.factor(all_combined_selected$activity) #necc?

# change numeric values (1,2..) to descriptive values ("WALKING","WALKING UPSTAIRS")
all_combined_selected$activity <- mapvalues(all_combined_selected$activity, from = as.character(activity_labels$V1), to = as.character(activity_labels$V2) )

#-------------------------------------- Create second data set for return -----------------------------
# subject, activity1, avg var1, avg var2

# reshape to subject, activity, 

temp <- melt(all_combined_selected, id.vars= c("subject","activity"))
temp$value <- as.numeric(temp$value) # change value to numeric so we can average
tempAgg <- dcast(temp,subject+activity ~ variable, fun.aggregate=mean, na.rm = TRUE)
#head(tempAgg[which(tempAgg$subject<=2),1:4],20)

#---------Write the file
write.table(tempAgg,"./ProjectOutput.txt",row.name=FALSE)

}