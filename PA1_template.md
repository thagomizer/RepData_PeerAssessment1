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
totalStepsMean <- as.character(round(mean(stepsByDay$steps, na.rm=TRUE)))
totalStepsMedian <- as.character(median(stepsByDay$steps, na.rm=TRUE))
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


```r
naCount <- sum(is.na(activity$steps))
```

There are 2304 missing step values in the data set.


```r
row.names(stepsByInterval) = stepsByInterval$interval

activityExtrapolated <- activity

for (i in 1:nrow(activityExtrapolated)) {
  if (is.na(activityExtrapolated[i,"steps"])) {
    interval <- toString(activityExtrapolated[i, "interval"])
    activityExtrapolated[i,"steps"] <- stepsByInterval[interval, "steps"]
  }
}

stepsByDayExtrapolated <- aggregate(activityExtrapolated$steps,
                               by=list(activityExtrapolated$date),
                               FUN=sum)
names(stepsByDayExtrapolated) <- c("date", "steps")
```

```r
hist(stepsByDayExtrapolated$steps,
     breaks = 20,
     main = "Histogram of total steps taken per day",
     xlab = "Total Steps")
```

![plot of chunk histogramWithExtrapolated](figure/histogramWithExtrapolated.png) 


```r
totalStepsExtrapolatedMean <- as.character(round(mean(stepsByDayExtrapolated$steps, na.rm=TRUE)))
totalStepsExtrapolatedMedian <- as.charater(median(stepsByDayExtrapolated$steps, na.rm=TRUE))
```

```
## Error: could not find function "as.charater"
```

The mean number of steps taken per day is 10766.
The median number of steps taken per day is 1.0766 &times; 10<sup>4</sup>.


## Are there differences in activity patterns between weekdays and weekends?
