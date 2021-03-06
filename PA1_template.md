# Reproducible Research: Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web
site: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]. But since the GitHub repository already contains the dataset for the assignment the code to download the data separately is not provided here. The code works as long as the dataset is located in the current working directory.

## Loading and preprocessing the data

First, the script unzips a file if necessary and reads it into a variable called `activity`. The second column of the dataset is transformed into a Date class object for further needs.


```r
## unzip if necessary
if(!file.exists("activity.csv")) {
    unzip(zipfile = "activity.zip")
}

## read data, transform dates
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)
```

Here is a sample of the dataset:


```r
head(activity)
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

## What is mean total number of steps taken per day?
## summarise by dates, calculate total number of steps taken each day

Next, the data is summarised by dates into a new dataset `by_dates` using **dplyr** package, and the total number of steps taken each day is calculated and added to this dataset as a variable `totalsteps`. Missing values are ignored.


```r
require(dplyr)
by_dates <- summarise(group_by(activity, date), 
                   totalsteps = sum(steps))
```

Below is a histogram of the total number of steps taken each day.


```r
hist(by_dates$totalsteps, breaks = 20, 
     main = "Total number of steps taken each day",
     xlab = "Number of steps")
```

![](PA1_template_files/figure-html/totalsteps-1.png) 

**Mean** and **median** of total number of steps taken per day are as follows:


```r
mean(by_dates$totalsteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(by_dates$totalsteps, na.rm = TRUE)
```

```
## [1] 10765
```

The results demonstrate that there are 8 days with no data recorded at all.

## What is the average daily activity pattern?

In order to get the average daily activity pattern, the dataset is summarised by 5-minute intervals. Below is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). Note that intervals are named in a specific hour-minute notation without leading zeroes so that 2355 stands for 23.55 (or 11.55 pm), 5 stands for 00.05 (00.05 am), etc.  


```r
by_intervals <- summarise(group_by(activity, interval),
                          mean = mean(steps, na.rm = TRUE))

plot(by_intervals$interval, by_intervals$mean, type = "l",
     main = "Average daily activity pattern",
     xlab = "5-minute intervals", 
     ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/intervals-1.png) 

The following code calculates the 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps.


```r
maxnum <- by_intervals[which.max(by_intervals$mean), ]
print(maxnum)
```

```
## Source: local data frame [1 x 2]
## 
##   interval     mean
## 1      835 206.1698
```

The maximum value 206.1698113 corresponds to the interval 835.

## Imputing missing values

Since the presence of missing days may introduce bias into some data manipulation, it may be necessary to fill in some values instead of NAs. First, let's calculate the total number of missing values in the dataset.


```r
removeNA <- complete.cases(activity)
sum(!removeNA)
```

```
## [1] 2304
```

2304 missing values correspond to 8 days with missing data (288 intervals per day). To fill in these values we use the mean for that 5-minute interval. The new dataset without NAs is called `newdata`.


```r
newdata <- activity
for (i in 1:nrow(newdata)) {
    if (is.na(newdata[i, "steps"])) {
        newdata[i, "steps"] <- 
            by_intervals[which(by_intervals$interval == newdata[i, "interval"],
                               arr.ind = TRUE), "mean"]
        newdata[i, "steps"] <- round(newdata[i, "steps"], digits = 0)
    }
}
```

Then `newdata` with filled in values is summarised by dates into a new dataset `new_by_dates` using **dplyr** package, and the total number of steps taken each day is calculated and added to this dataset as a variable `totalsteps`. **Mean** and **median** of total number of steps taken per day are also calculated and added as `mean` and `median` variables respectively.


```r
new_by_dates <- summarise(group_by(newdata, date), 
                       totalsteps = sum(steps))
```

Below is a histogram of the total number of steps taken each day (with some data filled in instead of NA).


```r
hist(new_by_dates$totalsteps, breaks = 20, 
     main = "Total number of steps taken each day (without NAs)",
     xlab = "Number of steps")
```

![](PA1_template_files/figure-html/newtotalsteps-1.png) 

The new values of `mean` and `median` are as follows:


```r
mean(new_by_dates$totalsteps, na.rm = TRUE)
```

```
## [1] 10765.64
```

```r
median(new_by_dates$totalsteps, na.rm = TRUE)
```

```
## [1] 10762
```

Thereby imputing missing values didn't affect the original data significantly. `mean` and `median` values are quite close to those calculated before imputing. Values of total number of steps taken each day demonstrate a shift towards average values.

## Are there differences in activity patterns between weekdays and weekends?

First, `weekday` factor variable is added to the `newdata` dataset. The script checks the day of a week of the corresponding Date object and assignes one of the two labels ("weekday" and "weekend") indicating whether a given date is a weekday or weekend day. Since the output of the `weekdays()` is locale dependent, usage of this function together with character representation of days of a week may require adjusting locale for the end user and in its turn may result in problems with reproducibility. Therefore a more universal approach was chosen to code weekdays as decimal numbers 1 to 7 using `strftime(..., format = "%u")` where 1 stands for Monday. Also for the sake of this assignment **weekend days are considered to be Saturday and Sunday**. If the research is going to take place in a country with another weekend policy the code should be adjusted as appropriate.



```r
newdata$weekday <- as.numeric(strftime(newdata$date, format = "%u"))

for (i in 1:5) {
    newdata$weekday <- gsub(i, "weekday", newdata$weekday)
}

for (i in 6:7) {
    newdata$weekday <- gsub(i, "weekend", newdata$weekday)
}

newdata$weekday <- as.factor(newdata$weekday)
```

Below is a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). **ggplot2** package is required for this plot.


```r
require(ggplot2)
new_by_intervals <- summarise(group_by(newdata, interval, weekday), 
                                   mean = mean(steps))

tsplot <- ggplot(new_by_intervals, aes(interval, mean))
tsplot + geom_line() + facet_grid(weekday ~ .) + 
    scale_x_continuous(breaks = pretty(new_by_intervals$interval, n = 12)) + 
    labs(x = "5-minute intervals", y = "Average number of steps")
```

![](PA1_template_files/figure-html/weekdays-1.png) 

According to the plot, weekdays demonstrate a longer activity period starting at around 5.30 am while on weekends it starts slightly later and is shifted towards the end of a day. Also the weekday pattern includes an evident peak in the morning with lower activity later in the day. The weekend pattern is more even across the day.

