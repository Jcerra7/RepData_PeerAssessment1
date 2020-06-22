#### Reading in the data from desktop

#### 1. Code for reading in the dataset and/or processing the data

    library(ggplot2)
    activity <- read.csv("/Users/jcerra/Desktop/activity.csv")
    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    summary(activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

#### 2. Histogram of steps per day

    steps_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
    hist(steps_day$steps, xlab = "Steps per Day", main = "Steps taken per day", col = "blue")

![](/Users/jcerra/Desktop/test/figures/unnamed-chunk-2-1.png)

#### 3. Mean and Median num of steps per day

    meansteps <- mean(steps_day$steps)
    mediansteps <- median(steps_day$steps)
    meansteps

    ## [1] 10766.19

    mediansteps

    ## [1] 10765

#### 4. Time series plot of the avg num of steps taken

    avg<- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
    plot(names(avg), avg, xlab="5 minutes intervals", type="l", ylab="Average number of steps")

![](/Users/jcerra/Desktop/test/figures/unnamed-chunk-4-1.png)

#### 5. The 5-minute interval that, on average, contains the maximum number of steps.

    maxavg<- max(avg)
    maxinterval<- as.numeric(names(avg)[which(avg==max(avg))])
    maxavg

    ## [1] 206.1698

    maxinterval

    ## [1] 835

#### 6. Inputting Missing data

    totalna <- sum(is.na(activity$steps))
    imputedata <- activity
    imputedata$steps[which(is.na(activity$steps))]<- as.vector(avg[as.character(activity[which(is.na(activity$steps)),3])])
    #Create new data set that is a match to original but with imputed values

#### NAs are distributed equally over intervals however they occur on specific dates, therefore we can just take mean of the steps taken during these days.

#### 7. Histogram of the total number of steps taken each day after missing values are imputed

    stepseachday <- tapply(imputedata$steps, imputedata$date, sum, na.rm=TRUE)
    qplot(stepseachday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)

![](/Users/jcerra/Desktop/test/figures/unnamed-chunk-7-1.png)

    medianeach<- median(stepseachday)
    meaneach<- mean(stepseachday)
    medianeach

    ## [1] 10766.19

    meaneach

    ## [1] 10766.19

#### Here we see that imputing missing values did not change the mean or median value

#### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

    imputedata$dayType<- ifelse(as.POSIXlt(imputedata$date)$wday %in% c(0,6), "weekends","weekdays")
    aggregateData<- aggregate(steps ~ interval + dayType, data=imputedata, mean)
    ggplot(aggregateData, aes(color = "", interval, steps)) + 
        geom_line() +
        facet_grid(dayType ~ .) +
        xlab("5 Minute Interval") + 
        ylab("Average number of steps")

![](/Users/jcerra/Desktop/test/figures/unnamed-chunk-8-1.png)
