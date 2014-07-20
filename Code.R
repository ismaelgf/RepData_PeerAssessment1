data<- read.csv("activity.csv", header=TRUE)
tidy<- subset(data, complete.cases(data))
tidy$date<-as.character(tidy$date)

tidy.sum<- data.frame(as.Date(unique(tidy$date)), 
                      tapply(tidy$steps, tidy$date, sum),
                      row.names=NULL)
names(tidy.sum)<- c("Date", "Sum.Steps")

mean.steps<- mean(tidy.sum$Sum.Steps)
median.steps<- median(tidy.sum$Sum.Steps)

tidy.interval<- ddply(tidy, ~interval, summarise, Mean.Steps=mean(steps))

plot(tidy.interval$interval, tidy.interval$Mean.Steps, 
     col="blue", type="l", main= "Average Steps taken per 5 min interval",
     xlab= "5 in interval", ylab="Mean number os steps")

max.steps<-max(tidy.interval$Mean.Steps)
max.interval<- tidy.interval[tidy.interval$Mean.Steps==max.steps, 1]

fill.data <- data
fill.data$steps <- mapply(function(steps, interval) 
        if (is.na(steps)) 
                tidy.interval[tidy.interval$interval == interval, 2]
        else
                steps, fill.data$steps, fill.data$interval)

fill.data$Wday <- mapply(function(steps, interval) 
        if (is.na(steps)) 
                tidy.interval[tidy.interval$interval == interval, 2]
        else
                steps, fill.data$steps, fill.data$interval)