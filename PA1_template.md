------

title: "PA1_template.Rmd"

output: 

     html_document:

     keep_md: true
------

Reproducible Research Peer Assessment 1
=======================================

## Assignment

### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

```r
setwd("~/My Exercises/DataScience")
data <- read.csv("data/activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```


### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(steps ~ date, data, sum)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day$steps, breaks=10, col="gray", main="Histogram of Total Number of Steps per Day", xlab="Total Number of Steps per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_per_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps,na.rm=TRUE)
```

```
## [1] 10765
```


### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
ave_steps_int <- aggregate(steps ~ interval, data, mean)
plot(ave_steps_int, type="l", col="red", main="Average Daily Activity Pattern", xlab="5-minute Interval", ylab="Average Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
ave_steps_int[which.max(ave_steps_int$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We will fill in all the missing values in steps with the mean for that 5-minute interval.

```r
new_steps <- numeric()
for (i in 1:nrow(data)) {
        item <- data[i, ]
        if (is.na(item$steps)) {
                new_item <- subset(ave_steps_int, interval == item$interval)$steps
        } else {
                new_item <- item$steps
        }
        new_steps <- c(new_steps, new_item)
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_activity <- data
new_activity$steps <- new_steps
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
new_steps_per_day <- aggregate(steps ~ date, new_activity, sum)
hist(new_steps_per_day$steps, breaks=10, col="green", main="(filled) Histogram of Total Number of Steps per Day", xlab="Total Number of Steps per Day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
mean(new_steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(new_steps_per_day$steps)
```

```
## [1] 10766.19
```
These values differ slightly. While the mean values are unchanged, the medians are shifted slightly. From the hist diagrams, the distributions are unchanged, but the peak after the filling is higher.

- Before filling

        1. Mean: 10766.19
        2. Median: 17065

- after filling

        1. Mean: 10766.19 
        2. Median: 17066.19 

### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dd <- weekdays(new_activity$date)
dd_fac <- vector()
for (i in 1:nrow(new_activity)) {
        if (dd[i] == "Saturday") {
                dd_fac[i] <- "Weekend"
        } else if (dd[i] == "Sunday") {
                dd_fac[i] <- "Weekend"
        } else {
                dd_fac[i] <- "weekday"
        }
}
new_activity$dayfactor <- dd_fac
new_activity$dayfactor <- factor(new_activity$dayfactor)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
steps_week <- aggregate(steps ~ interval + dayfactor, new_activity, mean)
library("lattice")
xyplot(steps ~ interval | dayfactor, steps_week, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
