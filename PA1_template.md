# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



```r
df <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
steps.per.day <- tapply(df$steps, df$date, FUN=sum, na.rm=TRUE)
library(ggplot2)
qplot(steps.per.day, binwidth=1000, xlab="Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(steps.per.day, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(steps.per.day, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
steps.on.average <- tapply(df$steps, df$interval, mean, na.rm = TRUE)
plot(row.names(steps.on.average), steps.on.average, type = "l", xlab = "5-minute Time Intervals", 
     ylab = "Average number of steps", main = "Average Daily Activity Pattern", 
     col = "magenta")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Counting missing values

```r
na.tot <- sum(is.na(df))
na.tot
```

```
## [1] 2304
```

## Imputing missing values

```r
average.by.interval <- (tapply(df$steps, df$interval, mean, na.rm = TRUE))
intervals <- unique(df$interval)
averages <- data.frame(cbind(average.by.interval, intervals))

imputed.steps <- function(steps, interval) {
  imputed <- NA
  if (!is.na(steps))
    imputed <- c(steps)
  else
    imputed <- (averages[averages$intervals==interval, "average.by.interval"])
  return(imputed)
}

# Clone the original dataframe

complete.df <- df

# Replace NAs with average values

complete.df$steps <- mapply(imputed.steps, complete.df$steps, complete.df$interval)

# Double-checking that there are no more NAs

na.tot <- sum(is.na(complete.df))
na.tot
```

```
## [1] 0
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# Weekdays vs Weekend (French getting in the way...)

complete.df$day <- ifelse(weekdays(as.Date(complete.df$date)) %in% c("samedi", "dimanche"), "weekend","weekday" )

averages.steps <- aggregate(complete.df$steps, by = list(complete.df$interval, complete.df$day), 
                       mean)
names(averages.steps) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, averages.steps, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
