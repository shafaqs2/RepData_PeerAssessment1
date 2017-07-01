rm(list=ls())
library(data.table)
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp, mode="wb")
unzip(temp, "activity.csv")
data<- read.csv("activity.csv")
unlink(temp)




###1. Code for reading in the dataset and/or processing the data
data<- read.csv("D:/R/5_REPDATA/Week_2/activity.csv")

###2. Histogram of the total number of steps taken each day
Steps_Per_Day<- with(data, tapply(steps, date, sum))
windows()
hist(Steps_Per_Day, col=3, xlim = c(0, 30000))

###3. Mean and median number of steps taken each day On histogram
data<- data[complete.cases(data)&data$steps>0, ]
Mean_Steps_Per_Day<- with(data, tapply(steps, date, mean))
Median_Steps_Per_Day<- with(data, tapply(steps, date, median))
Mean_Steps_Per_Day
Median_Steps_Per_Day

###4. Time series plot of the average number of steps taken
# windows()
# Average_5mins_Steps_Per_Day<- with(data, tapply(steps, date, mean))
# Date<-as.Date(levels(unique(data$date)), "%Y-%m-%d")
# plot.ts(Date, Average_5mins_Steps_Per_Day, type="1", xaxt = "n")
# axis(1, Date, format(Date, "%b %d"))
windows()
Average_5mins_Steps_Per_Day<- with(data, tapply(steps, interval, mean))
Time_Interval<- row.names(Average_5mins_Steps_Per_Day)
plot(Time_Interval, Average_5mins_Steps_Per_Day, type="l")


###5. The 5-minute interval that, on average, contains the maximum number of steps
a<-split(data$steps, data$interval)
b<- lapply(a, mean)
c<-which.max(b)
Five_min_interval=names(c)
Five_min_interval

###6. Code to describe and show a strategy for imputing missing data
dataNA<- read.csv("D:/R/5_REPDATA/Week_2/activity.csv")
NA_find<- is.na(dataNA)
sum(NA_find==TRUE)
a<-which(!is.na(dataNA$steps))


###7. Histogram of the total number of steps taken each day after missing values are imputed
dataNA<- read.csv("D:/R/5_REPDATA/Week_2/activity.csv")
a<-which(!is.na(dataNA$steps))
data_No_NA<-dataNA[a, ]
Steps_Per_Day<- with(data_No_NA, tapply(steps, date, sum))

windows()
hist(Steps_Per_Day, col=3, xlim = c(0, 30000))

##8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
data$days<-weekdays(as.Date(as.character(data$date),  "%Y-%m-%d")) 
data$days<-ifelse(data$days=="Saturday" | data$days=="Sunday", "weekend", "weekday")

panel_weekday<- with(subset(data, data$days=="weekday"), tapply(steps, interval, mean))
panel_weekend<- with(subset(data, data$days=="weekend"), tapply(steps, interval, mean))
Time_Interval_weekday<- row.names(panel_weekday)
Time_Interval_weekend<- row.names(panel_weekend)
windows()
par(mfrow=c(2, 1))
plot(Time_Interval_weekday, panel_weekday, type="l", xlab="Intervals", ylab="Average number of step", main="weekdays")
plot(Time_Interval_weekend, panel_weekend, type="l", xlab="Intervals", ylab="Average number of step", main="Weekends")
