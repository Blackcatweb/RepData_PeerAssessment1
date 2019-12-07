---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## Load relevant libraries


```r
library(ggplot2)
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

## Loading and preprocessing the data


```r
unzip("activity.zip")
data1 <- read.csv("activity.csv")
head(data1)
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

```r
str(data1)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data1)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?

Remove rows with NA


```r
datanoNA <- data1[!is.na(data1$steps),]
```

1. Calculate the total number of steps taken per day


```r
stepsPerDay <- summarise(group_by(datanoNA, date), 
                               totalSteps=sum(steps))
```

2. Make a histogram of the total number of steps taken each day


```r
ggplot(stepsPerDay, aes(x = totalSteps)) + 
  geom_histogram() + 
  labs(title = "Total Steps Per Day", x = "Steps", y = "Frequency")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsPerDay$totalSteps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$totalSteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Calculate average steps taken by interval


```r
meanStepsPerInt <- summarise(group_by(datanoNA, interval), 
                               meanSteps=mean(steps))
```

Plot time series plot


```r
ggplot(meanStepsPerInt, aes(interval,meanSteps)) +
  geom_line() +
  labs(title = "Average Steps by Interval", x = "5-min interval", 
       y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
meanStepsPerInt[which.max(meanStepsPerInt$meanSteps),]
```

```
## # A tibble: 1 x 2
##   interval meanSteps
##      <int>     <dbl>
## 1      835      206.
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data1$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy is to fill in missing values using the mean for that interval

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataNA <- is.na(data1$steps)

data1withMean <- merge(data1, meanStepsPerInt)

data1withMean$imputesteps <- data1withMean$steps

data1withMean$imputesteps[is.na(data1withMean$imputesteps)] <-
  data1withMean$meanSteps[is.na(data1withMean$imputesteps)]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Calculate the total number of steps taken per day


```r
stepsPerDaywithImpute <- summarise(group_by(data1withMean, date), 
                               totalStepswImp=sum(imputesteps))
```

Create histogram


```r
ggplot(stepsPerDaywithImpute, aes(x = totalStepswImp)) + 
  geom_histogram() + 
  labs(title = "Total Steps Per Day", x = "Steps", y = "Frequency")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Calculate the mean and median total number of steps taken per day


```r
mean(stepsPerDaywithImpute$totalStepswImp)
```

```
## [1] 10766.19
```

```r
median(stepsPerDaywithImpute$totalStepswImp)
```

```
## [1] 10766.19
```

What is the impact of imputing missing data on the estimates of the total daily number of steps?

Based on original data (without NA values), the mean changed by: 

```r
mean(stepsPerDaywithImpute$totalStepswImp) - mean(stepsPerDay$totalSteps)
```

```
## [1] 0
```

Based on original data (without NA values), the median changed by: 

```r
median(stepsPerDaywithImpute$totalStepswImp) - median(stepsPerDay$totalSteps) 
```

```
## [1] 1.188679
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data1withMean$day <- weekdays(as.Date(data1withMean$date))
data1withMean$weekdayend <- 
  ifelse (data1withMean$day == "Saturday" | 
            data1withMean$day == "Sunday","Weekend", 
          "Weekday")
head(data1withMean)
```

```
##   interval steps       date meanSteps imputesteps      day weekdayend
## 1        0    NA 2012-10-01  1.716981    1.716981   Monday    Weekday
## 2        0     0 2012-11-23  1.716981    0.000000   Friday    Weekday
## 3        0     0 2012-10-28  1.716981    0.000000   Sunday    Weekend
## 4        0     0 2012-11-06  1.716981    0.000000  Tuesday    Weekday
## 5        0     0 2012-11-24  1.716981    0.000000 Saturday    Weekend
## 6        0     0 2012-11-15  1.716981    0.000000 Thursday    Weekday
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Calculate the average number of steps taken per weekday / weekend


```r
avgStepsPerInt <- summarise(group_by(data1withMean, interval, weekdayend), 
                               avgStepswImp=mean(imputesteps))
```

Create histogram


```r
ggplot(avgStepsPerInt, aes(interval,avgStepswImp)) + 
  geom_line() + 
  facet_grid(weekdayend ~ .) +
  labs(title = "Average Steps Per Interval", x = "Interval", y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->



