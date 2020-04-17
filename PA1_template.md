---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading necessary libraries



## Loading and preprocessing the data


```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activity <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?


```r
dailysteps <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```

## What is the average daily activity pattern?

#### 1. Make a histogram of the total number of steps taken each day

```r
qplot(dailysteps, xlab='Total steps per day', ylab='Frequency', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### 2. Calculate and report the mean and median total number of steps taken per day

```r
dailystepsMean <- mean(dailysteps)
dailystepsMedian <- median(dailysteps)
```
* Mean: 9354.2295082
* Median:  10395

-----

## What is the average daily activity pattern?

```r
averageActivityPattern <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
```

#### 1. Make a time series plot of the 5-minute interval 

```r
ggplot(data=averageActivityPattern, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("Average Number of Steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- which.max(averageActivityPattern$meanSteps)
timeMaxSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageActivityPattern[maxSteps,'interval'])
```

* Most Steps at: 8:35

----

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset 

```r
numMissingValues <- length(which(is.na(activity$steps)))
```

* Number of missing values: 2304

#### 2. Devise a strategy for filling in all of the missing values in the dataset.
#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityImputed <- activity
activityImputed$steps <- impute(activity$steps, fun=mean)
```

#### 4. Make a histogram of the total number of steps taken each day 

```r
dailyStepsImputed <- tapply(activityImputed$steps, activityImputed$date, sum)
qplot(dailyStepsImputed, xlab='Total steps per day (Imputed)', ylab='Frequency', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

#### Calculate and report the mean and median total number of steps taken per day. 

```r
dailyStepsMeanImputed <- mean(dailyStepsImputed)
dailyStepsMedianImputed <- median(dailyStepsImputed)
```
* Mean (Imputed): 1.0766189\times 10^{4}
* Median (Imputed):  1.0766189\times 10^{4}

----

## Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityImputed$dateType <-  ifelse(as.POSIXlt(activityImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

#### 2. Make a panel plot containing a time series plot


```r
averagedActivityImputed <- aggregate(steps ~ interval + dateType, data=activityImputed, mean)
ggplot(averagedActivityImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
