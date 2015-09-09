
# get feeding interval
data.feed.interval <- ddply(data.feed, 
                            ~BabyID, 
                            mutate, 
                            FeedInterval = c(difftime(DateTime, lag(DateTime), units = "hours")))
data.feed.interval <- select(data.feed.interval, BabyID, Obs, DateTime, FeedInterval)
data.feed.interval <- filter(data.feed.interval, !is.na(FeedInterval))
data.feed.interval <- filter(data.feed.interval, !(BabyID ==1 | BabyID == 6
                                                   | BabyID == 10))
# filter some outliers
data.feed.interval <- filter(data.feed.interval, FeedInterval < 12)

data.feed.interval.night <- ddply(data.feed.interval, .(BabyID, as.Date(DateTime)), function(x) x[1,])
data.feed.interval.night.stat <- ddply(data.feed.interval.night,
                                       ~BabyID, summarise, mean=mean(FeedInterval),
                                       std=sqrt(var(FeedInterval)))
data.feed.interval.day <- ddply(data.feed.interval, .(BabyID, as.Date(DateTime)), function(x) x[-1,])

# count number of feedings per day
data.feed.count <- as.data.frame.matrix(table(data.feed$BabyID, data.feed$Date))
data.feed.count$BabyID <- as.numeric(rownames(data.feed.count))

# calculate feeding duration
data.feed.duration <- select(data.feed, BabyID, Date, Time, DateTime, BreastLeft, BreastRight)
data.feed.duration <- filter(data.feed.duration, BreastLeft !=0 | BreastRight !=0)
data.feed.duration$BreastTotal <- data.feed.duration$BreastLeft + data.feed.duration$BreastRight
data.feed.duration.stat <- ddply(data.feed.duration, ~BabyID, summarise, 
                                 meanl=mean(BreastLeft), stdl=sqrt(var(BreastLeft)),
                                 meanr=mean(BreastRight), stdr=sqrt(var(BreastRight)),
                                 meant=mean(BreastTotal), stdt=sqrt(var(BreastTotal)))

