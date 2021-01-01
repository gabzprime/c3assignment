library(dplyr)

main_folder <- "UCI HAR Dataset"
train_folder <- "train"
tst_folder <- "train"

#this function illustrates how to use the code wrttien
do_assignment <-function() {
  #load the merged train and test data
  ds<-load_data()
  #group by activity and subject then average the other fields
  ds<-generate_avg_dataset(ds)
  write.table(ds, file="out.txt", row.name=FALSE)
}

#gets raw train or test data
get_data <- function(data_set_type, base_folder) {
  if(data_set_type != "train" & data_set_type != "test") {
    stop (paste(data_set_type, "data_set_type is either test or train!"))
  }
  data_folder <- paste(base_folder,"/",data_set_type,sep="")
  if(!file.exists((data_folder))) {
    stop (paste(main_folder, "folder does not exists!"))
  }
  #get actual test/train data
  #get the subject performing activities
  fname<-paste(data_folder, "/", "subject_",data_set_type,".txt", sep="")
  subject<-read.delim(fname, header=FALSE, sep=" ")[,1]
  
  #get the vector of activity types
  fname<-paste(data_folder, "/", "y_",data_set_type,".txt", sep="")
  activity_id<-read.delim(fname, header=FALSE, sep=" ")[,1] 
  
  fname<-paste(data_folder, "/", "X_",data_set_type,".txt", sep="")
  ds <- read.delim(fname, header=FALSE, sep="")
  
  #bind the subject and the activity 
  ds <- cbind(ds,subject)
  ds <- cbind(ds, activity_id)
  ds
}


load_data <- function () {
  if(!file.exists(main_folder)) {
    stop (paste(main_folder, "folder does not exists!"))
  }
  base_folder <- paste("./",main_folder,"/",sep="")
  
  #get activity labels
  fname<-paste(base_folder, "/", "activity_labels.txt", sep="")
  activity_labels <- read.delim(fname, header=FALSE, sep=" ")
  names(activity_labels) <-c("id", "activity")
  
  #get features
  fname<-paste(base_folder, "/", "features.txt", sep="")
  features <- read.delim(fname, header=FALSE, sep=" ")[,2]

  testDs<-get_data("test",base_folder)
  #Req 4. Appropriately labels the data set with descriptive variable names. 
  names(testDs) <- append(features, c("subject","activity_id"))
  
  
  trainDs<-get_data("train",base_folder)
  #Req 4. Appropriately labels the data set with descriptive variable names. 
  names(trainDs) <- append(features, c("subject","activity_id"))
  
  ds <- rbind(testDs, trainDs)

  #Req 3. Uses descriptive activity names to name the activities in the data set
  ds <- merge(ds, activity_labels, by.x = "activity_id", by.y="id", all.x=TRUE)
  
  #Requirement 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  colnames <- append(c("subject", "activity"),grep("-std\\(\\)|-mean\\(\\)",names(ds),value=TRUE))
  
  ds <- ds %>% select(colnames)
  
  ds
}

#group dataset by activyt and subject then renames the columns
generate_avg_dataset <-function(ds){
  res<-ds %>% group_by(activity, subject) %>% summarise(across(everything(),mean))
  avgNames <- sapply(names(res)[c(3:68)],function(x) {paste("AVG-",x,sep="")})
  names(res) <-append(c("activity","subject"), avgNames)
  res
} 


