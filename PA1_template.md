---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
  
Read in and clean the data

```r
activitydata<-read.csv(projectfile,colClasses = c("integer","character","integer"))


activitydata$date<-as.Date(activitydata$date)
activitydata<-mutate(activitydata,weekday=wday(date))
activitydata<-arrange(activitydata,interval)
```
## What is mean total number of steps taken per day?

```r
daysums<-tapply(activitydata$steps,activitydata$date,mean,na.rm=T)
hist(daysums)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
stepaverage<-mean(daysums,na.rm = T)
stepmedian<-median(daysums,na.rm = T)
print(stepaverage)
```

```
## [1] 37.3826
```

```r
print(stepmedian)
```

```
## [1] 37.37847
```
Mean of steps taken each day is 37.3825996, and median is 37.3784722.  

## What is the average daily activity pattern?

```r
groupedactivitydata<-group_by(activitydata,interval)
groupedactivitydata<-mutate(groupedactivitydata,intervalaverage=mean(steps,na.rm = T))
with(groupedactivitydata,plot(interval,intervalaverage,xlab="Time",type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dailymax<-max(groupedactivitydata$intervalaverage)
maxrows<-which(groupedactivitydata$intervalaverage==dailymax)
maxinterval<-unique(groupedactivitydata$interval[maxrows])
print(maxinterval)
```

```
## [1] 835
```

## Imputing missing values
  
  
Using the "mice" impute function from the "mice package

```r
imputed_Data <- mice(groupedactivitydata, m=1, maxit = 10, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data)
```

```r
imputeddaysums<-tapply(completeData$steps,completeData$date,mean)
hist(imputeddaysums)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
print(summary(imputeddaysums))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1424 30.6285 36.7049 36.9001 44.7326 73.5903
```
Comparing to the previous histogram, median, and mean, we see that imputing the NAs does not make a significant difference.  

## Are there differences in activity patterns between weekdays and weekends?

```r
completeData<-mutate(completeData,weekdaytype=factor(completeData$weekday,                                                    levels=1:7,labels=c("weekend","weekday","weekday","weekday"                                               ,"weekday","weekday","weekend")))
completeData<-group_by(completeData,weekdaytype,interval)
completeData<-mutate(completeData,intervalaverage=mean(steps,na.rm = T))
xyplot(intervalaverage~interval|weekdaytype,completeData,type="l",layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
  
  
  
As we can see, weekdays have a strong spike in activity around 0830, whereas on weekends the activity is more
evenly distributed across the waking hours.
