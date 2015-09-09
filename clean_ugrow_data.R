

# add an extra column following the naming convention of user IDs
BabyAge[is.na(BabyAge$BabyID),]$BabyID <- paste("HPT2_NR_", 16:33, sep = "")
BabyID.table <- data.frame(BabyAge$UserIDExport, BabyAge$BabyID)


# remove repetitive data entries
CryingData <- unique(CryingData)
FeedingData <- unique(FeedingData)
SleepData <- unique(SleepData)

# add an extra column following the naming convention of user IDs in each of the dataset

CryingData <- merge(CryingData, 
                    BabyID.table, 
                    by.x = "BabyID",
                    by.y = "BabyAge.UserIDExport")

colnames(CryingData) <- c("UserID", 
                          "Date", 
                          "Time", 
                          "CryingSeconds", 
                          "BabyID")

FeedingData <- merge(FeedingData, 
                    BabyID.table,
                    by.x = "BabyID",
                    by.y = "BabyAge.UserIDExport")

colnames(FeedingData) <- c("UserID", 
                           "Date", 
                           "Time", 
                           "BottleAmount", 
                           "PumpingLeft", 
                           "PumpingRight",
                           "BreastfeedingLeft",
                           "BreastfeedingRight",
                           "BabyID")

SleepData <- merge(SleepData,
                     BabyID.table, 
                     by.x = "BabyID",
                     by.y = "BabyAge.UserIDExport")

colnames(SleepData) <- c("UserID",
                           "Date", 
                           "Time", 
                           "SleepingTime",
                           "BabyID")

rm(BabyID.table)
