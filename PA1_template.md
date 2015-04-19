---
output: html_document
---
Reproducible Research: Peer Assessment 1
========================================
Author: Toni Borrallo  
Coursera April 2015 
 
github repo:
https://github.com/toniborrallo/RepData_PeerAssessment1  


```r
require(ggplot2)
```
Initial configuration:  
Set to US English date format

```r
Sys.setlocale("LC_TIME", "en_US.UTF-8")
```

```
## [1] "en_US.UTF-8"
```

## Loading and preprocessing the data

Loading activity data.  


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

Create a formal date and time column from raw data 


```r
time <- formatC(activity$interval/100, 2, format = "f")
activity$date.time <- as.POSIXct(paste(activity$date, time), format = "%Y-%m-%d %H.%M", 
    tz = "GMT")
```

Sample of activity data content.  


```r
rbind(head(activity), tail(activity))
```

```
##       steps       date interval           date.time
## 1        NA 2012-10-01        0 2012-10-01 00:00:00
## 2        NA 2012-10-01        5 2012-10-01 00:05:00
## 3        NA 2012-10-01       10 2012-10-01 00:10:00
## 4        NA 2012-10-01       15 2012-10-01 00:15:00
## 5        NA 2012-10-01       20 2012-10-01 00:20:00
## 6        NA 2012-10-01       25 2012-10-01 00:25:00
## 17563    NA 2012-11-30     2330 2012-11-30 23:30:00
## 17564    NA 2012-11-30     2335 2012-11-30 23:35:00
## 17565    NA 2012-11-30     2340 2012-11-30 23:40:00
## 17566    NA 2012-11-30     2345 2012-11-30 23:45:00
## 17567    NA 2012-11-30     2350 2012-11-30 23:50:00
## 17568    NA 2012-11-30     2355 2012-11-30 23:55:00
```

## What is the mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
stepsbydate <- aggregate(steps ~ date, data=activity, FUN=sum)
ggplot(stepsbydate, aes(as.Date(date), steps)) + 
  geom_bar(stat = "identity", color = "blue", fill="lightblue") + 
  labs(title = "Histogram of Total number of steps taken each day", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-6](instrunctions_fig/unnamed-chunk-6-1.png) 

2. Calculate and report the **mean** and **median** total number of
   steps taken per day


```r
mean(stepsbydate$steps)
```

```
## [1] 10766.19
```

```r
median(stepsbydate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)


```r
stepsbyinterval <- aggregate(steps ~ interval, data=activity, FUN=mean)
ggplot(stepsbyinterval, aes(interval, steps)) + 
  geom_line() +
  labs(title = "Average daily activity pattern", x = "Time of day", y = "Mean number of steps")
```

![plot of chunk unnamed-chunk-8](instrunctions_fig/unnamed-chunk-8-1.png) 

2. Which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?


```r
stepsbyinterval$interval[which.max(stepsbyinterval$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

I will use the means for the 5-minute intervals as fillers for missing
values.

3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
activity <- merge(activity, stepsbyinterval, by="interval", suffixes=c("",".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[,c(1:3)]
```


4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
stepsbydate <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(stepsbydate$steps, names.arg=stepsbydate$date, xlab="date", ylab="steps")
```

![plot of chunk unnamed-chunk-12](instrunctions_fig/unnamed-chunk-12-1.png) 

```r
mean(stepsbydate$steps)
```

```
## [1] 10766.19
```

```r
median(stepsbydate$steps)
```

```
## [1] 10766.19
```

The impact of the missing data seems rather low, at least when
estimating the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
activity$wday <- weekdays(as.Date(activity$date))
activity$daytype <- ifelse(activity$wday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```


2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
par(mfrow=c(2,1))
for (type in c("Weekend", "Weekday")) {
    steps.type <- aggregate(steps ~ interval,
                            data=activity,
                            subset=activity$daytype==type,
                            FUN=mean)
    plot(steps.type, type="l", main=type)
}
```

![plot of chunk unnamed-chunk-14](instrunctions_fig/unnamed-chunk-14-1.png) 


