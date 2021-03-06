
Reproducible Research: Peer Assessment 1
===================================

This assignment uses data collected from a personal activity monitoring device to answer certain questions about the data and produce a reproducible report about the same. 

### Data

The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

There around 17568 observations of 3 variables, namely:

- steps  
- date  
- interval  

### Loading and preprocessing the data


```r
library(ggplot2)
library(scales)
library(lattice)

data = read.csv("activity.csv")
data$date <- as.Date( data$date, format="%Y-%m-%d")                                  # Format date strings to Date format

# First few lines of the dataset df after preprocessing
head(data)
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

### What is mean total number of steps taken per day?

Aggregate the total number of steps taken each day by `date` variable using FUN = sum.


```r
df_withoutNA<-na.exclude(data)                                                    # Create a new dataset excluding the NAs
df<-aggregate(df_withoutNA$steps,                                                # Aggregate data by date             
              by = list(Date = format(df_withoutNA$date, '%Y-%m-%d')),
              FUN = sum, 
              na.rm=TRUE)

colnames(df)[2]<-"steps"                                                         # rename second column as 'steps'
```

Plot a histogram of the total number of steps taken each day with red solid line for mean and blue dashed line for the median of the distribution.


```r
ggplot(df, aes(x=steps)) + 
    geom_histogram(binwidth=1000, 
                   colour="black", 
                   fill="white") +
    ylab("Frequency") +                                                              # y axis label
    xlab("Total Number of Steps taken each day") +                                   # x axis label
    ggtitle("Histogram of Total Number of Steps taken each day") +                   # plot title 
    theme(plot.title = element_text(face = "bold", colour="#990000", size=14),       # title text font
           axis.title.x = element_text(face="bold", colour="#990000", size=12),      # x axis text font
           axis.title.y = element_text(face="bold", colour="#990000", size=12)) +    # y axis text font
    geom_vline(aes(xintercept=mean(steps)),                                
               color="red", 
               linetype="solid", 
               size=1) + 
    geom_vline(aes(xintercept=median(steps)),                              
               color="blue", 
               linetype="dashed", 
               size=1)
```

![plot of chunk Histogram](figure/Histogram-1.png) 


```r
options(scipen = 1, digits = 0)
df.mean<-mean(df$steps )
```
Mean of total steps taken per day, `df.mean` = 10766


```r
df.median<-median(df$steps)
```
Median of total steps taken per day, `df.median` = 10765

The distribution seems to be a normal distribution with mean and median of the distribution approximately equal to each other.

### What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Aggregate the total number of steps taken each day by `interval` variable using FUN = mean.


```r
interval.mean<-aggregate(as.numeric( as.character( data$steps ) ) ,
               by = list(interval = as.factor(data$interval)),
               FUN = mean, 
               na.rm=TRUE)

colnames(interval.mean)[2]<-"steps"                                           # rename second column as 'steps'  

# Save interval as POSIXct in a new column
interval.mean$timeOfTheDay <- as.POSIXct(strptime(sprintf("%04d", as.numeric(as.character(interval.mean$interval))), "%H%M"))  

ggplot(interval.mean, aes(timeOfTheDay,steps)) + geom_line(aes(group=1)) +
  scale_x_datetime(labels = date_format("%H:%M")) +                           # display x label in required format
  xlab("5-minutes Interval") + 
  ylab("Average number of steps taken, averaged across all days") + 
  ggtitle("Average number of steps taken, averaged across all days \n at 5-minute intervals for October and November, 2012") +
  theme(plot.title = element_text(face = "bold", colour="#990000",size=14),       # plot title text font
           axis.title.x = element_text(face="bold", colour="#990000", size=12),   # x axis text font
           axis.title.y = element_text(face="bold", colour="#990000", size=12))   # y axis text font
```

![plot of chunk Time_Series_Plot](figure/Time_Series_Plot-1.png) 

5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is:


```r
interval.mean[interval.mean$steps == max(interval.mean$steps), c("interval","steps")]
```

```
##     interval steps
## 104      835   206
```

### Imputing missing values

Total number of missing values in the dataset (i.e. the total number of rows with NAs) can be calculated as:


```r
nrow(data[is.na(data$steps),])
```

```
## [1] 2304
```

Total number of rows with NAs is 2304  

We can impute the missing steps in the data by substitute NA for each such step with the average number of steps in that interval calculated across all the days in the dataset. 


```r
df.imputed <- transform(data, steps = ifelse(is.na(data$steps), interval.mean$steps[match(data$interval, interval.mean$interval)] , data$steps))
```

This is how the imputed dataset looks.


```r
head(df.imputed)
```

```
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

Aggregate the total number of steps taken each day by `date` variable using FUN = sum.


```r
df.imputed_sum<-aggregate(df.imputed$steps,                                 # Aggregate data by date 
               by = list(Date = format(df.imputed$date, '%Y-%m-%d')),
                FUN = sum, 
                na.rm=TRUE)  
colnames(df.imputed_sum)[2]<-"steps"                                         # rename second column as 'steps'  
head(df.imputed_sum)
```

```
##         Date steps
## 1 2012-10-01 10766
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

Plot a histogram of the total number of steps taken each day with red solid line for mean and blue dashe line for the median of the distribution.


```r
ggplot(df.imputed_sum, aes(x=steps)) + 
    geom_histogram(binwidth=1000, 
                   colour="black", 
                   fill="white") +
    ylab("Frequency") +                                                              # y axis label
    xlab("Total Number of Steps taken each day") +                                   # x axis label
    ggtitle("Histogram of Total Number of Steps taken each day") +                   # plot title 
    theme(plot.title = element_text(face = "bold", colour="#990000",size=14),        # plot title text font
           axis.title.x = element_text(face="bold", colour="#990000", size=12),      # x axis text font
           axis.title.y = element_text(face="bold", colour="#990000", size=12)) +    # y axis text font
    geom_vline(aes(xintercept=mean(steps)),                                 
               color="red", 
               linetype="solid", 
               size=1) + 
    geom_vline(aes(xintercept=median(steps)),                              
               color="blue", 
               linetype="dashed", 
               size=1)
```

![plot of chunk Histogram_Imputed](figure/Histogram_Imputed-1.png) 


```r
imputed.mean<-mean(df.imputed_sum$steps )
```
Mean of total steps taken per day = 10766


```r
imputed.median<-median(df.imputed_sum$steps)
```
Median of total steps taken per day = 10766

Calculate the difference between the imputed mean and mean with missing data:


```r
diff.mean <- imputed.mean - df.mean
```

Difference in means is 0

Calculate the difference between the imputed median and median with missing data:


```r
diff.median <- imputed.median - df.median
```

Difference in medians is 1

Calculate the difference in the total daily number of steps:


```r
diff.steps <- sum(df.imputed$steps) - sum(df$steps)
```

Difference in the total daily number of steps is 86130

The distribution seems to be little different from the earlier distribution, but the means and medians of the distribution do not change and are approximately equal to each other in both the cases.

### Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
df.imputed$day<-as.factor(c("weekend", rep("weekday",5), "weekend")[as.POSIXlt(df.imputed$date)$wday + 1])
```

Aggregate the total number of steps taken each day by `day` and `interval` using FUN = mean.


```r
df.week<-aggregate(df.imputed$steps, 
                by = list(day = df.imputed$day, interval = as.factor(df.imputed$interval)),
                FUN = mean, 
                na.rm=TRUE)

colnames(df.week)[3] = 'steps'

# Save interval as POSIXct in a new column
df.week$timeOfTheDay <- as.POSIXct(strptime(sprintf("%04d", as.numeric(as.character(df.week$interval))), "%H%M"))

xyplot(steps ~ timeOfTheDay | day,
        data = df.week,
        main = "Average number of steps, averaged across weekends and weekdays \n at 5-minute intervals for October and November, 2012",
        xlab = "5-minute Interval", 
        ylab = "Average number of steps",
        scales = list(x = list(format = "%H:%M")),                        #format the x-axis labels
        layout = c(1,2),
        type = "l")
```

![plot of chunk Time_Series_Imputed](figure/Time_Series_Imputed-1.png) 

Looking at the plot, we can notice substantial difference in the activity pattern between weekends and weekdays, with activities in general peaking 8 to 10 o'clock in the morning for both weekends and weekdays. Also, subject appears to be more active throughout a weekend as compared to weekdays.

