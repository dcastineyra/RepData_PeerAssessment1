1.  Loading the data and taking out the NA values

<!-- -->

    setwd("~/Data Science/Rep Research Assig1")
    datos<-read.table("activity.csv",sep = ",",header = TRUE)
    activity<-subset(datos,!is.na(datos$steps))
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

1.  Histogram of the total number of steps taken each day

<!-- -->

    ## Creating a data frame with the total steps by day
    day<-unique(activity$date)
    day<-as.Date(day)
    total.steps<-as.vector(tapply(activity$steps,activity$date,sum))
    total.steps<-subset(total.steps,!is.na(total.steps))
    ## Plotting the histogram
    plot(day,total.steps,type="h",xlab="Day",ylab="Total Steps",main="Total Steps by Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

1.  Mean and median number of steps taken each day

<!-- -->

    ## Average and Median by day
    Average.Steps = as.vector(tapply(activity$steps,activity$date,mean))
    Average.Steps<-subset(Average.Steps,!is.na(Average.Steps))
    Median.Steps = as.vector(tapply(activity$steps,activity$date,median))
    Median.Steps<-subset(Median.Steps,!is.na(Median.Steps))
    mm_by_day<-data.frame(Day=day,Average.Steps = Average.Steps,Median.Steps = Median.Steps)

This table shows the Average and Median of Steps per Day

    mm_by_day

    ##           Day Average.Steps Median.Steps
    ## 1  2012-10-02     0.4375000            0
    ## 2  2012-10-03    39.4166667            0
    ## 3  2012-10-04    42.0694444            0
    ## 4  2012-10-05    46.1597222            0
    ## 5  2012-10-06    53.5416667            0
    ## 6  2012-10-07    38.2465278            0
    ## 7  2012-10-09    44.4826389            0
    ## 8  2012-10-10    34.3750000            0
    ## 9  2012-10-11    35.7777778            0
    ## 10 2012-10-12    60.3541667            0
    ## 11 2012-10-13    43.1458333            0
    ## 12 2012-10-14    52.4236111            0
    ## 13 2012-10-15    35.2048611            0
    ## 14 2012-10-16    52.3750000            0
    ## 15 2012-10-17    46.7083333            0
    ## 16 2012-10-18    34.9166667            0
    ## 17 2012-10-19    41.0729167            0
    ## 18 2012-10-20    36.0937500            0
    ## 19 2012-10-21    30.6284722            0
    ## 20 2012-10-22    46.7361111            0
    ## 21 2012-10-23    30.9652778            0
    ## 22 2012-10-24    29.0104167            0
    ## 23 2012-10-25     8.6527778            0
    ## 24 2012-10-26    23.5347222            0
    ## 25 2012-10-27    35.1354167            0
    ## 26 2012-10-28    39.7847222            0
    ## 27 2012-10-29    17.4236111            0
    ## 28 2012-10-30    34.0937500            0
    ## 29 2012-10-31    53.5208333            0
    ## 30 2012-11-02    36.8055556            0
    ## 31 2012-11-03    36.7048611            0
    ## 32 2012-11-05    36.2465278            0
    ## 33 2012-11-06    28.9375000            0
    ## 34 2012-11-07    44.7326389            0
    ## 35 2012-11-08    11.1770833            0
    ## 36 2012-11-11    43.7777778            0
    ## 37 2012-11-12    37.3784722            0
    ## 38 2012-11-13    25.4722222            0
    ## 39 2012-11-15     0.1423611            0
    ## 40 2012-11-16    18.8923611            0
    ## 41 2012-11-17    49.7881944            0
    ## 42 2012-11-18    52.4652778            0
    ## 43 2012-11-19    30.6979167            0
    ## 44 2012-11-20    15.5277778            0
    ## 45 2012-11-21    44.3993056            0
    ## 46 2012-11-22    70.9270833            0
    ## 47 2012-11-23    73.5902778            0
    ## 48 2012-11-24    50.2708333            0
    ## 49 2012-11-25    41.0902778            0
    ## 50 2012-11-26    38.7569444            0
    ## 51 2012-11-27    47.3819444            0
    ## 52 2012-11-28    35.3576389            0
    ## 53 2012-11-29    24.4687500            0

1.  Time series plot of the average numbers of steps taken

<!-- -->

    ## Time Series of Average Steps
    Average.perinterval<-as.vector(tapply(activity$steps,activity$interval,mean))
    interval<-unique(activity$interval)
    plot(interval,Average.perinterval,type="l",xlab = "Interval",ylab = "Average Steps",main = "Average Steps by Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

1.  The 5-minute interval that, on average, containis the maximum number
    of steps

<!-- -->

    ## Interval with the maximum average steps
    max.interval<-interval[which.max(Average.perinterval)]

Maximum is 835

1.  Code to describe and show a strategy for imputing missing data To
    fill the NA values we use the average of each interval

<!-- -->

    ## Number of missing values
    mv<-subset(datos,is.na(datos$steps)==TRUE)
    nmv<-length(mv$steps)
    ## Filling missing values with average of the interval
    Average.perinterval2<-c(Average.perinterval,Average.perinterval,Average.perinterval,Average.perinterval,Average.perinterval,Average.perinterval,Average.perinterval,Average.perinterval)
    mv<-mutate(mv,steps=Average.perinterval2)
    ##new data
    activity2<-merge(activity,mv,all=TRUE)
    day2<-unique(activity2$date)
    day2<-as.Date(day2)
    total.steps2<-as.vector(tapply(activity2$steps,activity2$date,sum))

1.  Histogram of the total number of steps taken each day after missing
    values are imputed

<!-- -->

    ## Plotting the histogram
    plot(day2,total.steps2,type="h",xlab="Day",ylab="Total Steps",main="Total Steps by Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    ## Average and Median by day
    Average.Steps2 = as.vector(tapply(activity2$steps,activity2$date,mean))
    Median.Steps2 = as.vector(tapply(activity2$steps,activity2$date,median))
    mm_by_day2<-data.frame(Day=day2,Average.Steps = Average.Steps2,Median.Steps = Median.Steps2)

1.  Panel plot comparting the average number of steps taken per 5-minute
    interval across weekdays and weekends

<!-- -->

    activity3<-mutate(activity2,day=weekdays(as.Date(activity2$date)))
    weekday<-subset(activity3,activity3$day=="lunes" | activity3$day=="martes" | activity3$day == "miércoles" | activity3$day=="jueves")
    weekend<-subset(activity3,activity3$day== "viernes" | activity3$day=="sábado" | activity3$day=="domingo")
    Average.weekday<-as.vector(tapply(weekday$steps,weekday$interval,mean))
    Average.weekend<-as.vector(tapply(weekend$steps,weekend$interval,mean))
    interval1<-unique(activity3$interval)
    par(mfrow=c(2,1))
    plot(interval1,Average.weekday,type="l",xlab = "Interval",ylab = "Average Weekday",main = "Average Steps by Interval")
    plot(interval1,Average.weekend,type="l",xlab = "Interval",ylab = "Average Weekend",main = "Average Steps by Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)
