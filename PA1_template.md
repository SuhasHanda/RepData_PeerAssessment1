``` r
library(ggplot2)
activity <- read.csv("./activity.csv",colClasses = c("integer","Date","integer"),na.strings = "NA")
```

What is mean total number of steps taken per day?
-------------------------------------------------

-   Calculate the total number of steps taken per day

``` r
totals <- aggregate(steps ~ date,activity,sum)
```

-   aggregated total steps per day

``` r
head(totals)
```

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

-   Make a histogram of the total number of steps taken each day

``` r
g <- ggplot(totals,aes(x = totals$steps)) + 
    labs(x="steps") + 
    theme(plot.title = element_text(color="#666666", face="bold", size=32, hjust=0))

g + geom_histogram(aes(fill=..count..,label=..count..,alpha=0.1),bins=5)+
    stat_bin(aes(label=..count..),size=10, geom="text",vjust= +1,bins = 5)+
    scale_fill_gradient("Count", low = "red", high = "green4")+
    theme(legend.position="none",text = element_text(size=20))
```

    ## Warning: Ignoring unknown aesthetics: label

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

-   Calculate and report the mean and median of the total number of steps taken per day

``` r
mean.activity <- format(mean(totals$steps),digits = 6)
median.activity <- median(totals$steps)
```

mean of total steps per day is 10766.2 median of total steps per day is 10765

What is the average daily activity pattern?
-------------------------------------------

``` r
means <- aggregate(steps ~ interval,activity,mean)
```

-   Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
plot(means$interval,means$steps,type = "l",xlab = "Interval",
     ylab = "Average Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png)

-   Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
max <- means[which.max(means$steps),1]
```

Five min interval which contains max average number of steps is 835

Imputing missing values
-----------------------

-   Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
rowcount <- sum(is.na(activity))
```

Rows w missing values is equal to 2304

-   Create a new dataset that is equal to the original dataset but with the missing data filled in (replacing w mean by date).

``` r
activity.imputed <- activity[is.na(activity$steps),]
merge <- merge(activity.imputed,means,by = "interval")
activity.imputed <- subset(merge,select = c("steps.y","date","interval"))
names(activity.imputed) <- names(activity)
activity.imputed <- rbind(activity[!(is.na(activity$steps)),],
                         activity.imputed)
activity.imputed <- activity.imputed[order(activity.imputed$date),]
activity.imputed$steps <- round(activity.imputed$steps,0) 
head(activity.imputed,10)
```

    ##    steps       date interval
    ## 1      2 2012-10-01        0
    ## 10     0 2012-10-01        5
    ## 17     0 2012-10-01       10
    ## 29     0 2012-10-01       15
    ## 33     0 2012-10-01       20
    ## 45     2 2012-10-01       25
    ## 49     1 2012-10-01       30
    ## 58     1 2012-10-01       35
    ## 67     0 2012-10-01       40
    ## 73     1 2012-10-01       45

``` r
totals.imputed <- aggregate(steps ~ date,activity.imputed,sum)
```

-   Make a histogram of the total number of steps taken each day (w imputed)

``` r
g <- ggplot(totals.imputed,aes(x = totals.imputed$steps)) + 
    labs(x="steps") + 
    theme(plot.title = element_text(color="#666666", face="bold", size=32, hjust=0))

g + geom_histogram(aes(fill=..count..,label=..count..,alpha=0.1),bins=5)+
    stat_bin(aes(label=..count..),size=10, geom="text",vjust= +1,bins = 5)+
    scale_fill_gradient("Count", low = "red", high = "green4")+
    theme(legend.position="none",text = element_text(size=20))
```

    ## Warning: Ignoring unknown aesthetics: label

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

-   Calculate and report the mean and median of the total number of steps taken per day

``` r
mean.imputed <- format(mean(totals.imputed$steps),digits = 6)
median.imputed <- format(median(totals.imputed$steps),digits=5)
```

mean of total steps per day is 10765.6 median of total steps per day is 10762

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

-   Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
activity.imputed$Day <- as.factor(weekdays(activity.imputed$date))
activity.weekend <- activity.imputed [activity.imputed$Day %in% 
                                          c("Saturday","Sunday"),]
activity.weekday <- activity.imputed [!activity.imputed$Day %in% 
                                          c("Saturday","Sunday"),]
activity.weekday$Day <- "weekday"
activity.weekend$Day <- "weekend"

activity.imputed <- rbind(activity.weekend,activity.weekday)
activity.imputed <- activity.imputed[order(activity.imputed$date),]
activity.consolidated <- aggregate(steps~Day + interval,activity.imputed,mean)
```

-   Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken

``` r
library(lattice)
xyplot(steps~interval|Day,data =activity.consolidated,type = "l",
       panel = function(...){
            panel.xyplot(...)
            panel.abline(h=37.38)
       })
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-14-1.png)
