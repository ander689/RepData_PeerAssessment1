---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```{}
activity <-
read.csv("~/Documents/Coursera/RepData_PeerAssessment1/activity.csv")
```

## What is mean total number of steps taken per day?

Make a histogram, and calculate the mean and median of total steps taken each day!

```{}
stored<-c()
for(i in 1:length(unique(activity$date))){
  stored[i]<-sum(activity$steps[which(activity$date==unique(activity$date)[i])])
}
hist(na.omit(stored),breaks=15,xlab="Total Steps",main="Histogram of total steps")
mean(stored,na.rm=TRUE)
median(stored,na.rm=TRUE)
```


## What is the average daily activity pattern?

Make a time series plot! 

```{}
avg<-aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),FUN=mean, na.rm=TRUE)
plot(avg,type="l",xlab="Interval",ylab="Average Number of Steps")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{}
avg[which.max(avg$steps),]
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset.

```{}
sum(is.na(activity))
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Then create a new dataset that is equal to the original dataset but with the missing data filled in.

```{}
fillMissing <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (avg[avg$interval==interval, "steps"])
    return(filled)
}
filled <- activity
filled$steps <- mapply(fillMissing, filled$steps, filled$interval)
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{}
stored2<-c()
for(i in 1:length(unique(filled$date))){
  stored2[i]<-sum(filled$steps[which(filled$date==unique(filled$date)[i])])
}
hist(na.omit(stored2),breaks=15,xlab="Total Steps",main="Histogram of total steps")
mean(stored2,na.rm=TRUE)
median(stored2,na.rm=TRUE)
```

The new imputed values seemed to boost the other bins near the mean of the histogram. The mean stayed the same, and the median went up a little bit. The imputed values made the median the same as the mean.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{}
filled$day<-weekdays(as.Date(filled$date))
for(i in 1:length(filled$day)){
  if(filled$day[i]==c("Monday","Tuesday","Wednesday","Thursday","Friday")){
    filled$day[i]<-"weekday"
  }
  else{
    filled$day[i]<-"weekend"
  }
}
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```{}
library(ggplot2)
avg2<-aggregate(steps ~ interval + day,data=filled,mean)
ggplot(avg2,aes(interval,steps))+geom_line()+facet_grid(vars(day))+xlab("Interval (Seconds)")+ylab("Number of Steps")

```
