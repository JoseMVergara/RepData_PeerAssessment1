---
title: "Reproducible Research: Peer Assessment 1"
author: "JoseMVergara"
date: "Tuesday, September 01, 2020"
output:  
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
unzip("activity.zip")
datos <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
steps.date <- aggregate(steps ~ date, data=datos, FUN=sum)
library(ggplot2)
qplot(steps.date$steps, names.arg=steps.date$date, geom="bar")
```

```
## Warning: Ignoring unknown parameters: names.arg
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

2. Calculate and report the **mean** and **median** total number of
   steps taken per day


```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)


```r
steps.interval <- aggregate(steps ~ interval, data=datos, FUN=mean)
library(lattice)
xyplot(steps ~ interval, steps.interval, type = "s",
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(datos))
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
datos <- merge(datos, steps.interval, by="interval", suffixes=c("",".y"))
nas <- is.na(datos$steps)
datos$steps[nas] <- datos$steps.y[nas]
datos <- datos[,c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
steps.date <- aggregate(steps ~ date, data=datos, FUN=sum)
barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps", col=c("darkblue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
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



2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
day <- weekdays(as.Date(datos$date))
daylevel <- vector()
for (i in 1:nrow(datos)) {
    if (day[i] == "sábado") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "domingo") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
datos$daylevel <- daylevel
datos$daylevel <- factor(datos$daylevel)
stepsByDay <- aggregate(steps ~ interval + daylevel, data = datos, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
library("lattice")
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
