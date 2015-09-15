# Reproducible Research: Peer Assessment 1

In this report we analyze data from a personal activity monitoring device as described [here](README.md).

## Loading and preprocessing the data

We will use the `dplyr` pckage to manipulate the data, so load it first.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

We load the data directly from the zip file, and turn it into a `tbl`.


```r
activity <- tbl_df(read.csv(file = unz(description = "activity.zip", filename = "activity.csv")))
activity
```

```
## Source: local data frame [17,568 x 3]
## 
##    steps       date interval
##    (int)     (fctr)    (int)
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## ..   ...        ...      ...
```

## What is mean total number of steps taken per day?

We calculate the total number of steps taken per day, ignoring missing values.


```r
by_day <- activity %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(steps = sum(steps))
by_day
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
##        (fctr) (int)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```

We make a histogram of the total number of steps taken each day.


```r
library(ggplot2)
ggplot(data = by_day, mapping = aes(x = steps)) + geom_histogram()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

We calculate the mean and median of the total number of steps taken each day.


```r
summ <- by_day %>% summarize(mean = mean(steps), median = median(steps))
summ
```

```
## Source: local data frame [1 x 2]
## 
##       mean median
##      (dbl)  (int)
## 1 10766.19  10765
```

So the mean number of steps taken per day is 10766.19 and the median is 10765.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
