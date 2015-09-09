library("plyr")
library("dplyr")
library("ggplot2")

#-------------------------------------------------------------------#
# Generate usage data                                               #
#-------------------------------------------------------------------#
crying <- cbind(CryingData$BabyID, 
                CryingData$Date, 
                CryingData$Time, 
                "crying")

feeding <- cbind(FeedingData$BabyID, 
                 FeedingData$Date, 
                 FeedingData$Time, 
                 "feeding")

sleeping <- cbind(SleepData$BabyID, 
                  SleepData$Date, 
                  SleepData$Time, 
                  "sleeping")

UsageData <- data.frame(rbind(crying, feeding, sleeping))

colnames(UsageData) <- c("BabyID", 
                         "Date", 
                         "Time", 
                         "TypeOfTracker")

rm(crying, sleeping, feeding)


UsageData.PerTracker <- data.frame(table(UsageData$BabyID, 
                                         UsageData$TypeOfTracker))
colnames(UsageData.PerTracker) <- c("BabyID", 
                                    "TypeOfTracker", 
                                    "NumberOfRecords")

#-------------------------------------------------------------------#
# Plot usage data: number of records per user per tracker           #
#-------------------------------------------------------------------#
g <-  ggplot(UsageData.PerTracker, 
             aes(x = BabyID, y = NumberOfRecords)) +
      geom_bar(aes(fill = TypeOfTracker), 
               stat ="identity") +
      geom_text(data = subset(UsageData.PerTracker, NumberOfRecords != 0),
                aes(label = NumberOfRecords), 
                size = 3, 
                position = "stack",
                vjust = 1) +
      labs(title = "Number of records per user for each tracker") +
      theme(text = element_text(size = 12),
            plot.title = element_text(size = 20),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")

ggsave(filename = "usage_general.png",
       plot = g,
       path = "./plots",
       width = 8,
       height = 6)
rm(g)