# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
if (!file.exists("activity.zip")) {
  download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                destfile = "activity.zip",
                method = "curl")
  }

if (!file.exists("activity.csv")) {
  unzip ("activity.zip")
  }

activity <- read.csv("activity.csv")

activity[, "clean_date"] <- as.Date(strptime(activity[, "date"], format='%Y-%m-%d'))

stepsByDay <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
names(stepsByDay) <- c("date", "steps")

stepsByInterval = aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(stepsByInterval) <- c("interval", "steps")
```

## What is mean total number of steps taken per day?

```r
hist(stepsByDay$steps,
     breaks = 20,
     main = "Histogram of total steps taken per day",
     xlab = "Total Steps")
```

![plot of chunk histogram](figure/histogram.png) 


```r
totalStepsMean <- round(mean(stepsByDay$steps, na.rm=TRUE))
totalStepsMedian <- median(stepsByDay$steps, na.rm=TRUE)
```

The mean number of steps taken per day is 9354.
The median number of steps taken per day is 10395.


## What is the average daily activity pattern?

```r
plot(stepsByInterval$interval,
     stepsByInterval$steps,
     type="l",
     main="Daily Activity Pattern",
     xlab = "Time Interval",
     ylab = "Steps")
```

![plot of chunk dailyPlot](figure/dailyPlot.png) 


```r
maxStepRow <- activity[which(activity[, 1] == max(activity$steps, na.rm = TRUE)), ]
```

The maximum number of steps in a 5 minute interval was taken on 2012-11-27 during the interval 615


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
