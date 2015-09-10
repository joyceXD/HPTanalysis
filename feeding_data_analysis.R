library("dplyr")
library("plyr")
library("ggplot2")

# Extract feeding data from dataset ---------------------------------------

FeedingData.Feeding <- filter(FeedingData, 
                              is.na(PumpingLeft), 
                              is.na(PumpingRight))
keep.feeding <- c("BabyID", 
                  "Date",
                  "Time",
                  "BottleAmount",
                  "BreastfeedingLeft",
                  "BreastfeedingRight")

FeedingData.Feeding <- FeedingData.Feeding[keep.feeding]
                              
# bottle & breast feeding time per day ------------------------------------
FeedingData.Feeding$TypeOfFeeding <- ifelse(!is.na(FeedingData.Feeding$BottleAmount),
                                            "bottle feeding", 
                                            "breast feeding")

for(i in unique(FeedingData.Feeding$BabyID)){
  selected.data <- filter(FeedingData.Feeding,
                          BabyID == i)
  
  g <-  ggplot(selected.data, 
               aes(x = Date, y = Time)) +
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
  rm(selected.data, g)
}

