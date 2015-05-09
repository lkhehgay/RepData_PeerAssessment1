---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
---

**Loading and preprocessing the data**


1.  Load the data (i.e. read.csv()).


2.  Process/transform the data (if necessary) into a format suitable for your analysis.


```r
setwd("C://LKG//MOOC//Reproducible Research//Peer Assignment 1")
activity1 <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
head(activity1)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

**What is mean total number of steps taken per day?**


1.  Calculate the total number of steps taken per day.

```r
TotalNumberOfSteps1 <- aggregate(steps ~ date, data = activity1, sum, na.rm = TRUE)
```
2.  Make a histogram of the total number of steps taken each day.

```r
hist(TotalNumberOfSteps1$steps, main = "Total Steps By Day", xlab = "Day", col = "red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


3.  Calculate and report the mean and median of the total number of steps taken per day.

Mean is

```r
mean(TotalNumberOfSteps1$steps)
```

```
## [1] 10766.19
```
Median is

```r
median(TotalNumberOfSteps1$steps)
```

```
## [1] 10765
```

**What is the average daily activity pattern?**


1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
steps.interval <- aggregate(steps ~ interval, data = activity1, FUN = mean)
plot(steps.interval, type = "l", main = "Average Number of Steps Taken", xlab = "5-Minute Interval", ylab = "Average Number of Steps Taken, Averaged Across All Days", col = "blue")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

**Imputing missing values**


1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
sum(is.na(activity1))
```

```
## [1] 2304
```
2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
StepsAverage <- aggregate(steps ~ interval, data = activity1, FUN = mean)
FillUpNAValues <- numeric()
for (i in 1:nrow(activity1))
{
  Data_Set <- activity1[i, ]
  if (is.na(Data_Set$steps))
  {
    steps <- subset(StepsAverage, interval == Data_Set$interval)$steps
  }
  else
  {
    steps <- Data_Set$steps
  }
  FillUpNAValues <- c(FillUpNAValues, steps)
}
```
3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity2 <- activity1
activity2$steps <- FillUpNAValues
```
4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps.date <- aggregate(steps ~ date, data = activity2, FUN = sum)
TotalNumberOfSteps2 <- aggregate(steps ~ date, data = activity2, sum, na.rm = TRUE)
hist(TotalNumberOfSteps2$steps, main = "Total Steps By Day", xlab = "Day", col = "red")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


Mean is

```r
mean(TotalNumberOfSteps2$steps)
```

```
## [1] 10766.19
```
Median is

```r
median(TotalNumberOfSteps2$steps)
```

```
## [1] 10766.19
```
The mean is the same for the first part and second part of the assignment.
However, the median is just minimally different, and hence the impact of the missing data seemed low.

**Are there differences in activity patterns between weekdays and weekends?**


1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
TypeOfDay <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday"))
    {
        "Weekend"
    }
    else
    {
        "Weekday"
    }
}
activity1$TypeOfDay <- as.factor(sapply(activity1$date, TypeOfDay))
```
2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
layout = c(1,2)
for (type in c("Weekend", "Weekday"))
{
  steps.type <- aggregate(steps ~ interval, data = activity1, subset = activity1$TypeOfDay == type, FUN = mean)
  plot(steps.type, type = "l", main = type, xlab = "Interval", ylab = "Number of Steps", col = "blue")
}
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) ![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-2.png) 
