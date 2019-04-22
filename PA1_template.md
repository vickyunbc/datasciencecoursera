---
output: 
  html_document:
    keep_md: true
---

This is the code for Project of week 2 of Reproducible Research by Coursera

We will now load the data here which is in csv format

# 1. Start of Loading the data and pre process of the data

```r
library("data.table")
library(ggplot2)

activity <- read.csv("activity.csv")
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

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
# # End of Loading the data and pre process of the data


# 2. Start of Mean Total Number of Steps taken per day

## For that we need couple of steps before we calculate the total number of steps.

## The first is to change the date type to class, then NA to logical vector and create a clean base for later mean caclulations


```r
# change date type to date class 
activity$date <- as.Date(as.character(activity$date))
# create NA logical vector
activityNA <- is.na(activity$steps)
# create clean base for later mean calculations
clean_activity <- activity[!activityNA,]
```

# 2a. What is mean total number steps taken per day?

## Now we will calculate the totl number of steps by using the aggregeate function 


```r
# aggregate the daily sum of steps taken
DataByDay <- aggregate(activity$steps, by=list(activity$date), sum)
# adjust column names
names(DataByDay)[1] ="date"
names(DataByDay)[2] ="totalsteps"
# top 10 of Summed Steps by day
head(DataByDay,10)
```

```
##          date totalsteps
## 1  2012-10-01         NA
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08         NA
## 9  2012-10-09      12811
## 10 2012-10-10       9900
```

## 2b. Now we are going to plot the histogram of number of steps taken using ggplot. And This will be the question number 2 of the project.


```r
ggplot(DataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.print(png, file = "plot1.png", width = 480, height = 480)
```

```
## quartz_off_screen 
##                 2
```

```r
png(file = "plot1.png", bg = "transparent")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

## 2c. Calculate and report the mean and median of the total number of steps taken per day


```r
# Mean of steps taken per day
mean(DataByDay$totalsteps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
#median of steps taken per day
median(DataByDay$totalsteps,na.rm=TRUE)
```

```
## [1] 10765
```


# 3. What is the average daily activity pattern

## 3a. Making Time Series plot of 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
No_NA <- activity[!activityNA,]
mean_data_Interval <- aggregate(No_NA$steps, by=list(No_NA$interval), mean)
# set the column names
names(mean_data_Interval)[1] ="interval"
names(mean_data_Interval)[2] ="steps"

ggplot(mean_data_Interval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line(color="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
dev.print(png, file = "plot2.png", width = 480, height = 480)
```

```
## quartz_off_screen 
##                 2
```

```r
png(file = "plot2.png", bg = "transparent")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

## 3b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxsteps <- mean_data_Interval[which.max(mean_data_Interval$steps),]
maxsteps
```

```
##     interval    steps
## 104      835 206.1698
```


# 4. Imputing the Missing Values

## 4a, 4b and 4c. Calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs

```r
missing_values <- sum(activityNA)
missing_values
```

```
## [1] 2304
```

```r
## Devise a strategy for filling in all of the missing values in the dataset. 

### New Data Set
activity2 <- activity
### Removing NA values from new dataset
No_NA_activity <- activity2[is.na(activity2$steps),]
clean_activity <- activity2[!is.na(activity2$steps),]


# calculating the mean of new dataset
Mean_activity_new <- aggregate(clean_activity$steps, by=list(clean_activity$interval), sum)
names(Mean_activity_new)[1] ="interval"
names(Mean_activity_new)[2] ="steps"


# Imputing
activity2 <- activity
missing_values <- is.na(activity2$steps)
meanVals <- tapply(clean_activity$steps, clean_activity$interval, mean, na.rm=TRUE, simplify=TRUE)
activity2$steps[missing_values] <- meanVals[as.character(activity2$interval[missing_values])]
# count of NA values
sum(is.na(activity2$steps))
```

```
## [1] 0
```

## 4d. Making the histogram of total number os steps tke and calculating the mean and median of total number of steps taken per day.


```r
final_sum_data_by_data <- aggregate(activity2$steps, by=list(activity2$date), sum)

names(final_sum_data_by_data)[1] ="date"
names(final_sum_data_by_data)[2] ="totalsteps"
head(final_sum_data_by_data,10)
```

```
##          date totalsteps
## 1  2012-10-01   10766.19
## 2  2012-10-02     126.00
## 3  2012-10-03   11352.00
## 4  2012-10-04   12116.00
## 5  2012-10-05   13294.00
## 6  2012-10-06   15420.00
## 7  2012-10-07   11015.00
## 8  2012-10-08   10766.19
## 9  2012-10-09   12811.00
## 10 2012-10-10    9900.00
```

```r
ggplot(final_sum_data_by_data, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
dev.print(png, file = "plot3.png", width = 480, height = 480)
```

```
## quartz_off_screen 
##                 2
```

```r
png(file = "plot3.png", bg = "transparent")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
mean(final_sum_data_by_data$totalsteps)
```

```
## [1] 10766.19
```

```r
median(final_sum_data_by_data$totalsteps)
```

```
## [1] 10766.19
```

### The mean remains same but median value increased 1.19 steps

### The over all effect is that data is pushed towards mean

# 5. Are there differences in activity patterns between weekdays and weekends?

## 5a. Creating new factor variable in the dataset with two levels "weekday" and "weekend"


```r
activity2$weekday <- weekdays(activity2$date)
activity2$weekend <- ifelse (activity2$weekday == "Saturday" | activity2$weekday == "Sunday", "Weekend", "Weekday")
head(activity2,5)
```

```
##       steps       date interval weekday weekend
## 1 1.7169811 2012-10-01        0  Monday Weekday
## 2 0.3396226 2012-10-01        5  Monday Weekday
## 3 0.1320755 2012-10-01       10  Monday Weekday
## 4 0.1509434 2012-10-01       15  Monday Weekday
## 5 0.0754717 2012-10-01       20  Monday Weekday
```

## 5b. Making a panel plot containing the time series plot of 5 minute interval 


```r
combined_weekday_weekend_mean <- aggregate(activity2$steps, by=list(activity2$weekend, activity2$interval), mean)
names(combined_weekday_weekend_mean)[1] ="weekend"
names(combined_weekday_weekend_mean)[2] ="interval"
names(combined_weekday_weekend_mean)[3] ="steps"

ggplot(combined_weekday_weekend_mean, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
dev.print(png, file = "plot4.png", width = 480, height = 480)
```

```
## quartz_off_screen 
##                 2
```

```r
png(file = "plot4.png", bg = "transparent")
dev.off()
```

```
## quartz_off_screen 
##                 2
```







