---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Loading my libraries

```r
library(lubridate)
library(dplyr)
library(lattice)
```

Set some options

```r
options(scipen=1,digits=3)
```



## Loading and preprocessing the data

```r
d <- read.csv("activity.csv",stringsAsFactors=FALSE)
d$date <- ymd(d$date)
```



## What is mean total number of steps taken per day?


```r
dailyActivity<-group_by(d,date)
dailyActivity<-filter(dailyActivity,!is.na(steps))
dailyActivity <- summarise(dailyActivity,steps=sum(steps))
hist(dailyActivity$steps,main = "Histogram of the Total Number of Steps Taken Each Day",xlab = "steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
myMean <- mean(dailyActivity$steps)
myMedian <- median(dailyActivity$steps)
```

The mean number of steps per day is 10766.189 and the median number of steps per day is 10765.

## What is the average daily activity pattern?


```r
dailyActivity<-filter(d,!is.na(steps))
dailyActivity<-arrange(dailyActivity,interval)
dailyActivity<-group_by(dailyActivity,interval)
dailyActivity<-summarise(dailyActivity,avg = mean(steps))
with(dailyActivity, plot(interval,avg,type="l",main="Average Steps Taken",ylab="Average Steps"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
myMaxInterval<- dailyActivity$interval[which.max(dailyActivity$avg)]
```

The interval 835 contains the maximum average steps of 206.17.

## Imputing missing values

First we need to find the number of rows that have NA's


```r
numNAs <- nrow(d)-sum(complete.cases(d))
```

The number of rows with NA's is 2304.

So what I decided to do for this was to replace NA's with the average number of steps for that interval across all days.

In order to do this I first did a join of my average values with my original dataset

```r
d <- left_join(d,dailyActivity)
```

```
## Joining by: "interval"
```

Now with a bit of magic using ifelse I combine the steps and avg steps columns where I take steps if it exists or avg if it does not.


```r
d <- within(d, newSteps <- ifelse(!is.na(steps),steps,avg))
```


Create the histogram on this new steps field.

```r
dailyActivity<-group_by(d,date)
dailyActivity <- summarise(dailyActivity,steps=sum(newSteps))
hist(dailyActivity$steps,main = "Histogram of the Total Number of Steps Taken Each Day (replacing NA's with average)",xlab = "steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
myMean2 <- mean(dailyActivity$steps)
myMedian2 <- median(dailyActivity$steps)
```

The new mean is 10766.189 which is the same as the old mean but the new median is 10766.189 as opposed to 10765.  The mean didn't change because I was replacing the values with the means.  

## Are there differences in activity patterns between weekdays and weekends?

So first we have to add in a factor variable for weekday vs weekend


```r
d$isWeekend <- wday(d$date) %in% c(1,7)
d$isWeekend <- as.factor(d$isWeekend)
levels(d$isWeekend) <- c("weekday","weekend")
```

Now we need to do some grouping to get the average steps per interval split out by weekday/weekend

```r
d <- as.data.frame(d)
activity <- group_by(d,isWeekend,interval)
activity <- summarise(activity,newSteps=mean(newSteps))
```

Now simply make the plot

```r
xyplot(newSteps ~ interval | isWeekend, data=activity, type='l',layout=c(1,2),xlab="Interval",ylab="Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

