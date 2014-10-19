#Peer assesment 1

Setup `knitr` options.


```r
library(knitr)
opts_chunk$set(echo = TRUE, results = "asis", cache=FALSE)
```

###Loading and preprocessing the data

Download and read the data.


```r
url<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists('activity.csv')){
        if(!file.exists('activity.zip')){
                download.file(url,"activity.zip", mode="wb")
                unzip('activity.zip')       
        } else {
                unzip('activity.zip')
        }       
        
}
dat <- read.csv('activity.csv')
```

###What is mean total number of steps taken per day?

Plot the histogram.


```r
stepsPerDay<-tapply(dat$steps, dat$date, FUN = sum, na.rm = TRUE)

plot(stepsPerDay, type = "h", lwd = 3, col="blue", main = "Total Number of Steps Per Day (missing values input)",  xlab = "Days", ylab = "Total number of steps", xaxt = "n" )
axis(1, at = c(1, 19, 39, 59), labels=c(as.character(unique(dat$date)[1]),as.character(unique(dat$date)[19]),as.character(unique(dat$date)[39]),as.character(unique(dat$date)[59])))
```

![plot of chunk plot.histogram](figure/plot.histogram-1.png) 

Calculate mean and median total number of steps taken per day.



```r
meanSteps<-mean(stepsPerDay, na.rm = TRUE)
medianSteps<-median(stepsPerDay, na.rm = TRUE)
```

Mean total number of steps taken per day is **9354.23**.  
Median total number of steps taken per day is **10395**.

###What is the average daily activity pattern?

Plot a time series of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
numberOfSteps<-tapply(dat$steps, dat$interval, FUN = mean, na.rm = TRUE)
plot( unique(dat$interval), numberOfSteps, type = "l", ylab = "Average Number of Steps", xlab = "Interval", main = "Average number of steps per interval")
```

![plot of chunk plot.daily.activity](figure/plot.daily.activity-1.png) 

Find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.


```r
intervalMax<-unique(dat$interval)[which.max(numberOfSteps)]
```

**835th** interval contains maximum maximum number of steps across all days in the dataset

###Inputing missing values

Calculate the total number of missing values in the dataset.


```r
totalMissing<-sum(!complete.cases(dat))
```

Total number of missing values in the dataset is **2304**.  
  
Use the mean for the 5-minute interval for filling the missing values in the dataset.  


```r
dat2<-dat

for (i in 1:nrow(dat2)){
        if(is.na(dat2[i,"steps"])){
                temp<-numberOfSteps[which(unique(dat$interval) == dat2[i,"interval"])]
                temp<-as.numeric(temp)
                dat2[i,"steps"] <- temp                
        }
}
```

Plot the histogram of the total number of steps taken each day with missing data being input.  


```r
stepsPerDay2<-tapply(dat2$steps, dat2$date, FUN = sum, na.rm = TRUE)

plot(stepsPerDay2, type = "h", lwd = 3, col="blue", main = "Total Number of Steps Per Day (missing values input)",  xlab = "Days", ylab = "Total number of steps", xaxt = "n" )

axis(1, at = c(1, 19, 39, 59), labels=c(as.character(unique(dat2$date)[1]),as.character(unique(dat2$date)[19]),as.character(unique(dat2$date)[39]),as.character(unique(dat2$date)[59])))
```

![plot of chunk plot.histogram2](figure/plot.histogram2-1.png) 

Calculate mean and median total number of steps taken per day


```r
meanSteps2<-mean(stepsPerDay2)
medianSteps2<-median(stepsPerDay2)
```

Mean total number of steps taken per day after data input is **10766.19**. Median total number of steps taken per day after data input is **10766.19**.  
As we can see these values differ from the estimates without data input.

###Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "English")
dat2["day"] <- as.factor(weekdays(as.Date(as.character(dat2$date))))
levels(dat2$day)<- c("weekday", "weekday", "weekend", "weekend", "weekday", "weekday", "weekday")
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days.




```r
par(mfcol = c(2, 1))

datWeekday<-dat2$day=="weekday"
numberOfStepsWeekday<-tapply(dat2$steps[datWeekday], dat2$interval[datWeekday], FUN = mean, na.rm = TRUE)
plot( unique(dat$interval[datWeekday]), numberOfStepsWeekday, type = "l", ylab = "Average Number of Steps", xlab = "Interval", main = "Average number of steps per interval, weekdays")


datWeekend<-dat2$day=="weekend"
numberOfStepsWeekend<-tapply(dat2$steps[datWeekend], dat2$interval[datWeekend], FUN = mean, na.rm = TRUE)
plot( unique(dat$interval[datWeekend]), numberOfStepsWeekend, type = "l", ylab = "Average Number of Steps", xlab = "Interval", main = "Average number of steps per interval, weekends")
```

![plot of chunk plot.daily.activity.weekday](figure/plot.daily.activity.weekday-1.png) 





