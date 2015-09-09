library("plyr")
library("dplyr")
library("ggplot2")
library("scales")

# Data aggregation: generate usage data -----------------------------------

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


# Stacked bar plot: overview number of records per user per tracker -------

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


# Heat map overview: usage data per user per day --------------------------
UsageData.Heatmap <- data.frame(table(UsageData$BabyID, UsageData$Date))

colnames(UsageData.Heatmap) <- c("BabyID", 
                                 "Date",
                                 "NumberOfRecords")

g <-  ggplot(UsageData.Heatmap, aes(Date, BabyID)) + 
      geom_tile(aes(fill = NumberOfRecords), colour = "white") + 
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Number of records per user for each tracker") +
      theme(text = element_text(size = 12),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "usage_per_day_heatmap.png",
       plot = g,
       path = "./plots",
       width = 10,
       height = 6)
rm(g)


# Heat map overview: usage data per user per tracker ----------------------

UsageData.Heatmap.PerTracker <- data.frame(table(UsageData$BabyID, 
                                                 UsageData$Date,
                                                 UsageData$TypeOfTracker))

colnames(UsageData.Heatmap.PerTracker) <- c("BabyID", 
                                 "Date",
                                 "TypeOfTracker",
                                 "NumberOfRecords")

generate_heatmap_for_tracker<- function(dataset){
  
  for(i in unique(dataset$TypeOfTracker)){
    
    selected.data <- subset(dataset, TypeOfTracker == i)
    g <-  ggplot(selected.data, aes(Date, BabyID)) + 
      geom_tile(aes(fill = NumberOfRecords), colour = "white") + 
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = paste("Number of records per user for", i)) +
      theme(text = element_text(size = 12),
            plot.title = element_text(size = 20),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(filename = paste("usage_per_day_heatmap_",
                            i,
                            ".png",
                            sep = ""),
           plot = g,
           path = "./plots",
           width = 10,
           height = 6)
    rm(g)
  }
}

generate_heatmap_for_tracker(UsageData.Heatmap.PerTracker)


# Stacked bar plot: number of records per tracker per day for each --------

# dir.create("./plots/usage per day", 
#            showWarnings = TRUE, 
#            recursive = FALSE, 
#            mode = "0777")

for(i in unique(UsageData.Heatmap.PerTracker$BabyID)){
    
    selected.data <- subset(UsageData.Heatmap.PerTracker, BabyID == i)
    
    g <-  ggplot(selected.data, 
                 aes(x = Date, y = NumberOfRecords)) +
          geom_bar(aes(fill = TypeOfTracker), 
               stat ="identity") +
          geom_text(data = subset(selected.data, NumberOfRecords != 0),
                aes(label = NumberOfRecords), 
                size = 3, 
                position = "stack",
                vjust = 1) +
          scale_y_continuous(breaks = pretty_breaks(),
                             limits = c(0,
                                        max(UsageData.Heatmap.PerTracker$NumberOfRecords))) +
          labs(title = paste("Number of records per day for", i)) +
          theme(text = element_text(size = 12),
            plot.title = element_text(size = 20),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
    
    
    ggsave(filename = paste("Usage_per_day_",
                            i,
                            ".png",
                            sep = ""),
           plot = g,
           path = "./plots/usage per day",
           width = 8,
           height = 6)
    rm(g)
}
