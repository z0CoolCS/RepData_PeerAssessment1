---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lattice)
file <- "activity.csv"
if (!file.exists(file)) {
  unzip("activity.zip")  
}
act <- read.csv(file)
act$date <- as.Date(act$date, format = "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
#### 1. Calculate the total number of steps taken per day

```r
act_byday <- act[!is.na(act$steps),] %>% group_by(date) %>% summarise(steps = sum(steps))
act_byday
```

```
## # A tibble: 53 x 2
##    date       steps
##    <date>     <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # … with 43 more rows
```

#### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day  

```r
hist(act_byday$steps, breaks = length(act_byday$date), xlab = "Days", main = "Histogram of steps by day" )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


#### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_act <- mean(act_byday$steps); 
median_act <- median(act_byday$steps)
```
The mean steps per day in dataset is 1.0766189\times 10^{4}.  
The median steps per day in dataset is 10765.  

## What is the average daily activity pattern?
#### 1. Make a time series plot(type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
act_bydaymean <- act[!is.na(act$steps),] %>% group_by(interval) %>% summarise(mean_steps = mean(steps))
plot(act_bydaymean$interval,act_bydaymean$mean_steps, type = "l", xlab = "interval", ylab = "mean by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval_max <- which.max(act_bydaymean$mean_steps)
```
The interval with the max steps across all the day is : 104

## Imputing missing values

```r
miss_values <- sum(!complete.cases(act))
```
There are 2304 missing values in the data set.

##### The mean of each interval is taken to put in missing values positions.  

```r
act_full <- act
act_full[is.na(act$steps),1] <- act_bydaymean$mean_steps
```


#### Make a histogram of the total number of steps taken each day 

```r
actfull_byday <- act_full %>% group_by(date) %>% summarise(steps = sum(steps))
hist(actfull_byday$steps, breaks = length(actfull_byday$date), xlab = "Days", main = "Histogram of steps by day" )
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
#### Calculate and report the mean and median total number of steps taken per day

```r
mean_actfull <- mean(actfull_byday$steps); 
median_actfull <- median(actfull_byday$steps)
```
The mean steps per day in full dataset is 1.0766189\times 10^{4}.  
The median steps per day in full dataset is 1.0766189\times 10^{4}.  

#### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
There is not so much variance. In fact, the absolute difference between mean and median between actual data set and new data set are: 0 and  1.1886792.  

## Are there differences in activity patterns between weekdays and weekends?
#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  

```r
week <- ifelse(weekdays(act_full$date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekday", "weekend")
act_full$week <- as.factor(week) 
```

#### Make a panel plot containing a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
actfull_bydaymean <- act_full %>% group_by(interval, week) %>% summarise(mean_steps = mean(steps))
xyplot(mean_steps ~ interval | week, data=actfull_bydaymean, aspect=1/3, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


