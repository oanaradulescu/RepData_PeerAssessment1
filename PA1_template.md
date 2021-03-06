# Reproducible Research: Peer Assessment 1

---

### Presets ###


```r
library(knitr)
opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE, fig.width=10)
```


```r
library(dplyr)
library(ggplot2)
library(scales)
```


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. ```read.csv()```)


```r
df <- read.csv("activity.csv", colClasses=c("integer","Date","integer"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# calculate: mean_total_steps, median_total_steps
# daily_total_steps is not used by ggplot so it's not saved in the data frame
# rather it is calculated by geom_bar()

ddf <- df %.%
    group_by(date) %.%
    summarise(daily_total_steps=sum(steps, na.rm=TRUE))

df$mean_total_steps <- ( ddf %.%
    summarise(mean_total_steps=round(mean(daily_total_steps, na.rm=TRUE))) )$mean_total_steps
df$median_total_steps <- ( ddf %.%
    summarise(median_total_steps=round(median(daily_total_steps, na.rm=TRUE))) )$median_total_steps

# calculate: mean_int_steps, max_int_steps_int

ddf <- df %.%
    group_by(interval) %.%
    summarise(mean_int_steps=round(mean(steps, na.rm=TRUE)))
df <- inner_join(df, ddf, by="interval")
df$max_int_steps_int <- df[df$mean_int_steps == max(df$mean_int_steps), "interval"][1]
```

## What is the mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day

Mean: 9354 steps per day ( ```max(df$mean_total_steps)``` )  
Median: 10395 steps per day ( ```max(df$median_total_steps)``` )


```r
legend <- c("mean"="solid", "median"="dashed")
p <- ggplot(df, aes(x=date, y=steps))
p <- p + geom_bar(stat="identity", fill="grey55")
p <- p + geom_line(stat="hline", aes(yintercept=mean_total_steps, linetype="mean"))
p <- p + geom_line(stat="hline", aes(yintercept=median_total_steps, linetype="median"))
p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), labels=comma) +
        scale_x_date(breaks=pretty_breaks(n=10), labels=date_format("%b-%d")) +
        scale_linetype_manual(name="", values=legend)
p <- p + labs(x="Date", 
              y="Daily Steps (Total)") +
        theme(axis.title=element_text(size=11, color="grey50"), 
              axis.title.x=element_text(vjust=-0.5),
              axis.title.y=element_text(vjust=0.25))

p
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

Interval with the most number of steps: 835 ( ```max(df$max_int_steps_int)``` )


```r
legend <- c("most steps"="dotted")
p <- ggplot(df, aes(x=interval, y=mean_int_steps))
p <- p + geom_line(color="grey55")
p <- p + geom_line(stat="vline", aes(xintercept=max_int_steps_int, linetype="most steps"))
p <- p + scale_y_continuous(breaks=pretty_breaks(n=10)) +
        scale_x_continuous(breaks=pretty_breaks(n=10), labels=function(x)paste(substr(sprintf("%04s",x),1,2),":",substr(sprintf("%04s",x),3,4),sep="")) + 
        scale_linetype_manual(name="", values=legend)
p <- p + labs(x="5-min Interval (24-hour Period)", 
              y="Steps per Interval (Avg Across All Days)") +
        theme(axis.title=element_text(size=11, color="grey50"), 
              axis.title.x=element_text(vjust=-0.5),
              axis.title.y=element_text(vjust=0.25))

p
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


## Imputing missing values

Note that there are a number of days/intervals where there are missing 
values (coded as NA). The presence of missing days may introduce bias 
into some calculations or summaries of the data.

1. Calculate and report the total number of missing values 
in the dataset (i.e. the total number of rows with NAs)

The total number of missing steps-measurements in the dataset is 2304. ( ```sum(is.na(df$steps))``` )

2. Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, 
you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# calculate: steps_imp

df$steps_imp <- df$steps
df[is.na(df$steps), "steps_imp"] <- df[is.na(df$steps), "mean_int_steps"]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# new dataset, equal to the original dataset but with the missing data filled in

df_imp <- df[,c("steps_imp", "date", "interval")]
names(df_imp) <- c("steps", "date", "interval")
```

4. Make a histogram of the total number of steps taken each day and 
Calculate and report the **mean** and **median** total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# calculate: mean_total_steps, median_total_steps

ddf <- df_imp %.%
    group_by(date) %.%
    summarise(daily_total_steps=sum(steps, na.rm=TRUE))

df_imp$mean_total_steps <- ( ddf %.%
    summarise(mean_total_steps=round(mean(daily_total_steps, na.rm=TRUE))) )$mean_total_steps

df_imp$median_total_steps <- ( ddf %.%
    summarise(median_total_steps=round(median(daily_total_steps, na.rm=TRUE))) )$median_total_steps

legend <- c("mean"="solid", "median"="dashed")
p <- ggplot(df_imp, aes(x=date, y=steps))
p <- p + geom_bar(stat="identity", fill="grey55")
p <- p + geom_line(stat="hline", aes(yintercept=mean_total_steps, linetype="mean"))
p <- p + geom_line(stat="hline", aes(yintercept=median_total_steps, linetype="median"))
p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), labels=comma) +
        scale_x_date(breaks=pretty_breaks(n=10), labels=date_format("%b-%d")) +
        scale_linetype_manual(name="", values=legend)
p <- p + labs(x="Date", 
              y="Daily Steps (Total)") +
        theme(axis.title=element_text(size=11, color="grey50"), 
              axis.title.x=element_text(vjust=-0.5),
              axis.title.y=element_text(vjust=0.25))

p
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

What is the impact of imputing missing data on the estimates of the total daily number of steps?

In the case of a lot of missing measurements per day, the total daily number of 
steps can change dramatically.

## Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

![sample panelplot](instructions_fig/sample_panelplot.png)


```r
# calculate: day_of_week (weekday or weekend ?)
# calculate: mean_total_steps_imp per day_of_week

df_imp$day_of_week <- "weekday"
df_imp[weekdays(df_imp$date) %in% c("Saturday", "Sunday"), ]$day_of_week <- "weekend"
df_imp$day_of_week <- factor(df_imp$day_of_week)

ddf <- df_imp %.%
    group_by(interval, day_of_week) %.%
    summarise(mean_int_steps=mean(steps))

df_imp <- inner_join(df_imp, ddf, by=c("interval", "day_of_week"))

p <- ggplot(df_imp, aes(x=interval, y=mean_int_steps))
p <- p + geom_line()
p <- p + facet_grid(day_of_week ~ .)
p <- p + scale_x_continuous(breaks=pretty_breaks(n=24), 
                            labels=function(x) paste(substr(sprintf("%04s",x),1,2),":",substr(sprintf("%04s",x),3,4),sep="")) + 
        scale_linetype_manual(name="", values=legend)
p <- p + labs(x="5-min Interval (24-hour Period)", 
              y="Steps per Interval (Avg Across All Days)") +
        theme(axis.title=element_text(size=11, color="grey50"), 
              axis.title.x=element_text(vjust=-0.5),
              axis.text.x=element_text(size=8),
              axis.title.y=element_text(vjust=0.25))
p
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

