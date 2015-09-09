source("load_libraries.R")


# specify feeding data folder directory
filepath <- "C:/Data/uGrow/Home placement test 1/"

# retrieve all feeding data csv files in this folder
function <- readFileNames
filelist.feed <- list.files(path=filepath, pattern="FeedingData.csv$", full.names=TRUE)
data.feed <- data.frame(matrix(ncol = 8, nrow = 0))

# combine all the csv files into one data frame
for(i in 1:length(filelist.feed)){
  data <- read.csv(filelist.feed[i], header=TRUE, sep=",")
  data <- cbind(BabyID=filelist.feed[i], Obs=1:nrow(data),data)
  data.feed <- rbind(data.feed, data)
}
names(data.feed) <- c('BabyID', 'Obs','Date','Time','Bottle','PumpingLeft','PumpingRight','BreastLeft','BreastRight')

# replace all NA values with 0
data.feed[is.na(data.feed)] <- 0

# replace BabyID with numbers
data.feed$BabyID <- as.numeric(unlist(str_extract_all(data.feed$BabyID,"\\d+")))

data.feed$DateTime <- as.POSIXct(paste(data.feed$Date, data.feed$Time,sep=""))
data.pump <- filter(data.feed, PumpingLeft != 0 | PumpingRight != 0)
data.feed <- filter(data.feed, PumpingLeft == 0, PumpingRight == 0)
data.feed <- select(data.feed, -c(PumpingLeft,PumpingRight))
 
data.feed.interval <- ddply(data.feed, 
                            ~BabyID, 
                            mutate, 
                            FeedInterval = c(difftime(DateTime, lag(DateTime), units = "hours")))
data.feed.interval <- select(data.feed.interval, BabyID, Obs, DateTime, FeedInterval)
data.feed.interval <- filter(data.feed.interval, !is.na(FeedInterval))
data.feed.interval <- filter(data.feed.interval, !(BabyID ==1 | BabyID == 6
                                                   | BabyID == 10))
# filter some outliers
data.feed.interval <- filter(data.feed.interval, FeedInterval < 16)
data.feed.interval.melt <- melt(data.feed.interval, id.vars=c("BabyID","Obs","DateTime"))

data.feed.interval.stat <- ddply(data.feed.interval, ~BabyID, summarise, mean=mean(FeedInterval),
                              std=sqrt(var(FeedInterval)))

# plot feeding interval patterns
ggplot(data.feed.interval.melt, aes(x=Obs, y=value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ BabyID, nrow=3) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face="bold"),
        plot.title = element_text(size=20, face="bold")) +
  labs(list(title = "Feeding interval for each baby (hour)", x = "Time", y = "Feeding interval (hour)"))

# plot bottle feeding
data.feed.interval.melt.bottle <- filter(data.feed.interval.melt, BabyID %in% c(3,4,13,14,15))
ggplot(data.feed.interval.melt.bottle, aes(x=Obs, y=value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ BabyID, nrow=3) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face="bold"),
        plot.title = element_text(size=20, face="bold")) +
  labs(list(title = "Feeding interval for each baby - breast feeding (hour)", 
            x = "Time", y = "Feeding interval (hour)"))


data.feed.interval.night <- ddply(data.feed.interval, .(BabyID, as.Date(DateTime)), function(x) x[1,])
data.feed.interval.night.stat <- ddply(data.feed.interval.night,
                                       ~BabyID, summarise, mean=mean(FeedInterval),
                                 std=sqrt(var(FeedInterval)))
data.feed.interval.day <- ddply(data.feed.interval, .(BabyID, as.Date(DateTime)), function(x) x[-1,])
data.feed.interval.day.stat <- ddply(data.feed.interval.day,
                                       ~BabyID, summarise, mean=mean(FeedInterval),
                                       std=sqrt(var(FeedInterval)))
# calculate count feeding per day
data.feed.count <- as.data.frame.matrix(table(data.feed$BabyID, data.feed$Date))
data.feed.count$BabyID <- as.numeric(rownames(data.feed.count))
data.feed.count.melt <- arrange(melt(data.feed.count, id.vars="BabyID"), BabyID, as.Date(variable))
colnames(data.feed.count.melt) <- c("BabyID", "Date", "FeedCount")
data.feed.count.stat <- ddply(data.feed.count.melt, ~BabyID, summarise, mean=mean(FeedCount[FeedCount!=0]),
                              std=sqrt(var(FeedCount[FeedCount!=0])))

# calculate feeding duration
data.feed.duration <- select(data.feed, BabyID, Date, Time, DateTime, BreastLeft, BreastRight)
data.feed.duration <- filter(data.feed.duration, BreastLeft !=0 | BreastRight !=0)
data.feed.duration$BreastTotal <- data.feed.duration$BreastLeft + data.feed.duration$BreastRight
data.feed.duration.stat <- ddply(data.feed.duration, ~BabyID, summarise, 
                                 meanl=mean(BreastLeft), stdl=sqrt(var(BreastLeft)),
                                 meanr=mean(BreastRight), stdr=sqrt(var(BreastRight)),
                                 meant=mean(BreastTotal), stdt=sqrt(var(BreastTotal)))

# plot breast feeding durations
ggplot(data.feed.duration, aes(x=DateTime, y=BreastTotal)) +
  geom_line(aes(y = BreastLeft), colour="blue") + 
  geom_point(aes(y = BreastLeft), colour="blue") +
  geom_line(aes(y = BreastRight), colour="red") + 
  geom_point(aes(y = BreastRight), colour="red") +
  facet_wrap(~ BabyID, nrow=4) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20, face="bold")) +
  labs(list(title = "Breast feeding duration for each baby (in seconds)", x = "Date", y = "Feeding duration (second)"))


# bottle feed analysis
data.feed.bottle <- select(data.feed, as.numeric(BabyID), Date, Time, DateTime, Bottle)
data.feed.bottle <- filter(data.feed.bottle, Bottle > 0, BabyID %in% c(5,8,11,12))
data.feed.bottle.daysum <- ddply(data.feed.bottle, ~BabyID+Date, summarise, 
                                 dailysum=sum(Bottle))

# plot bottle feed for each baby
ggplot(data.feed.bottle, aes(x=DateTime, y=Bottle)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ BabyID, nrow=2) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20, face="bold")) +
  labs(list(title = "Bottle feed quantity for each baby", x = "Date", y = "Bottle feed quantity (ml)"))

# plot bottle feed of daily sum
ggplot(data.feed.bottle.daysum, aes(x=Date, y=dailysum)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ BabyID, nrow=2) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20, face="bold")) +
  labs(list(title = "Bottle feed quantity daily sum", x = "Date", y = "Bottle feed quantity (ml)"))


# plot 3 5 8 13 14
data.feed.u5 <- filter(data.feed, data.feed$BabyID==5)
data.moment.english$Date <- as.Date(data.moment.english$Date, "%d-%m-%Y")
data.moment.english$DateTime <- as.POSIXct(paste(data.moment.english$Date, data.moment.english$Time,sep=""))
data.moment.u5 <- filter(data.moment.english, data.moment.english$BabyID == 5)

# plot daily sum of baby number 5
ggplot(data.feed.bottle.daysum, aes(x=Date, y=dailysum)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ BabyID, nrow=2) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20, face="bold"))


# plot bottle feeding versus moments Baby #5
lower <- with(data.feed.u5,min(DateTime))
upper <- with(data.feed.u5,max(DateTime))
limits = c(lower,upper)

g1 <- ggplot(data.feed.u5, aes(x=DateTime, y=Bottle)) +
  geom_line() +
  geom_point() + 
  scale_x_datetime(limits=limits)

dislocations <- runif(nrow(data.moment.u5), 0.5, 20)
dislocations <- 0-dislocations
g2 <- ggplot(data.moment.u5) +
  geom_text(aes(x=DateTime, y=dislocations, label=English), 
            position="jitter", size=4, alpha=0.5) + 
  geom_hline(yintercept=0, size=1, scale="date" ) +
  geom_segment(aes(x = DateTime, y=dislocations, xend=DateTime, yend=0)) +
  scale_x_datetime(limits=limits)
grid.arrange(g1, g2, nrow = 2, main = "Baby's feeding activities vs moment data")


# further investigation on request of Anne

# duration of the longest recorded feeding sessions
data.feed.breast <- filter(data.feed, Bottle==0)
data.feed.breast.max.left <- max(data.feed.breast$BreastLeft)
data.feed.breast.max.right <- filter(data.feed, BreastRight == max(data.feed.breast$BreastRight))
data.feed.breast.max <- filter(data.feed, BreastLeft + BreastRight >= 3600)
data.usage <- ddply(data.feed, 
                    ~BabyID, 
                    summarize, 
                    min=min(DateTime),
                    max=max(DateTime))

# calculate total number of days the app in use
data.usage <- as.data.frame(table(data.feed$BabyID, data.feed$Date))
colnames(data.usage) <- c("BabyID","Date","Entries")
data.usage$Date <- as.Date(data.usage$Date)
data.usage <- data.usage[order(data.usage$BabyID,as.Date(data.usage$Date)),]
ggplot(data.usage, aes(Date, y=Entries)) +
  geom_bar(stat="identity") +
  facet_wrap(~ BabyID, nrow=3) +
  theme(axis.text=element_text(size=12,face="bold"), 
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face="bold"),
        plot.title = element_text(size=20, face="bold")) +
  labs(list(title = "Number of records per day", x = "Date", y = "Number of records"))

# Does the app usage also include the moments people tracked or the tracker data only?
# Are parents who’s second child it is tracking less?
# Are parents who are tracking bottle feeding, more consistent in filling in the timers than parents who are tracking breastfeeding?

# read moment files from folder
filelist.moment <- list.files(path=filepath, pattern="MomentData.csv$", full.names=TRUE)
data.moment <- data.frame()

# !!! PROBLEM WITH REGULAR EXPRESSION PART !!!
for(i in 1:length(filelist.moment)){
  data <- readLines(con = file(filelist.moment[i]))
  data <- data[-1,]
  matches <- str_match(data, pattern="([0-9]{4}-[0-9]{2}-[0-9]{2}),([0-9]{2}:[0-9]{2}:[0-9]{2}[+][0-9]{2}:[0-9]{2}),([a-zA-Z]+),(.+)")
  data <- cbind(BabyID = filelist.moment[i], data)
  data.moment <- rbind.fill(data.moment, data)
}

## need to manually process some lines and reload it

names(data.moment) <- c('BabyID','Date','Time','Momenttype','Note')
data.moment[is.na(data.moment)] <- ""

# combine columns of free text into one column, concatenating by space
data.moment$Note <- do.call(paste, c(data.moment[,5:7], sep = " "))
data.moment$BabyID <- as.numeric(unlist(str_extract_all(data.moment$BabyID,"\\d+")))
data.moment <- data.moment[,1:5]

# tracker data vs moment data
data.moment$Date <- as.Date(data.moment$Date)
data.moment$DateTime <- as.POSIXct(paste(data.moment$Date, data.moment$Time,sep=""))
data.usage.both <- data.frame(rbind(cbind(data.feed$BabyID, 
                                     as.character(data.feed$DateTime), 
                                     c(rep("Feeding", times = nrow(data.feed)))),
                               cbind(data.moment$BabyID, 
                                     as.character(data.moment$DateTime),
                                     c(rep("Moment", times = nrow(data.moment))))))

colnames(data.usage.both) <- c("BabyID", "DateTime", "DataType")
# data.usage.both$BabyID <- as.numeric(data.usage.both$BabyID)
data.usage.both <- data.usage.both[order(data.usage.both$BabyID, data.usage.both$DateTime),]
data.usage.both <- droplevels(data.usage.both)
data.usage.both.stat <- ddply(data.usage.both,
                         .(BabyID,as.Date(DateTime),DataType), nrow)
data.usage.both.stat <- data.frame(lapply(data.usage.both.stat, as.character), stringsAsFactors=FALSE)
data.usage.both.stat$BabyID <- as.numeric(data.usage.both.stat$BabyID)
data.usage.both.stat$Date <- as.Date(data.usage.both.stat$Date)
data.usage.both.stat$Count <- as.numeric(data.usage.both.stat$Count)
colnames(data.usage.both.stat)[2] <- "Date"
colnames(data.usage.both.stat)[4] <- "Count"

data.usage.both.stat <- data.usage.both.stat[order(data.usage.both.stat$BabyID, data.usage.both.stat$Date),]

for(i in c(1,3,4,5,6,8,10,11,12,13,14,15)){
  data.usage.both.baby <- filter(data.usage.both.stat, BabyID == i)
  g <- ggplot(data.usage.both.baby, aes(x=Date, y=Count, fill=DataType)) +
    geom_bar(stat="identity", position="dodge", width=0.7) +
    xlim(as.Date(c('2015-04-13','2015-04-30')))+
    ylim(c(0, 12))+
    theme(axis.text=element_text(size=12, face="bold"), 
          axis.title=element_text(size=14,face="bold"),
          plot.title = element_text(size=20, face="bold"),
          legend.position = "none") +
    labs(list(title = i, x = "Date", y = "Number of inputs"))
  ggsave(filename=paste(i,".png"), g)
}

ggplot(data.usage.both.stat, aes(x=Date, y=Count, fill=DataType)) +
  geom_bar(stat="identity", position="dodge", width=0.7) +
  facet_wrap(~ BabyID, nrow=3) +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face="bold"),
        plot.title = element_text(size=20, face="bold")) +
  labs(list(title = "Tracker and moment data usage", x = "Date", y = "Number of inputs"))


