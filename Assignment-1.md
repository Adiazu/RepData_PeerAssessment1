
title: "Reproducible Research: Peer Assessment 1"
author: "Arnulfo Díaz Ulate"
output: 
  html_document:
    keep_md: true




<br><br>

### 1. Loading and preprocessing the data:

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity_data.zip")
unzip("activity_data.zip")
dt<- read.csv("activity.csv", stringsAsFactors = F)
```

Install ggplot2, and sqldf library 


```
## Loading required package: gsubfn
```

```
## Loading required package: proto
```

```
## Loading required package: RSQLite
```

Remove the missing values

```r
missing_dt <- dt[is.na(dt$steps),]
dim(missing_dt)
```

```
## [1] 2304    3
```

```r
dt1 <- dt[!is.na(dt$steps),]
```

<br><br>

### 2. What is mean total number of steps taken per day?

#### Histogram:

```r
StepsPerDay <- sqldf("Select date,sum(Steps) as TotalSteps from dt1 group by date")
hist(StepsPerDay$TotalSteps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
```

![](https://github.com/Adiazu/RepData_PeerAssessment1/blob/master/instructions_fig/Histogram%201.png)<!-- -->

#### mean and median:

**Info** of steps taken each day:

```r
summary(StepsPerDay)
```

```
##      date             TotalSteps   
##  Length:53          Min.   :   41  
##  Class :character   1st Qu.: 8841  
##  Mode  :character   Median :10765  
##                     Mean   :10766  
##                     3rd Qu.:13294  
##                     Max.   :21194
```
<br><br>

### 3. What is the average daily activity pattern?

#### Time series plot:

```r
AvgStepsbyInterval <- sqldf("Select interval,avg(Steps) as AvgSteps from dt1 group by interval")
plot(AvgStepsbyInterval$interval,AvgStepsbyInterval$AvgSteps, type = "l", main = "Time series plot of the \n average number of steps taken", xlab = "interval", ylab = "Mean steps")
```

![](https://github.com/Adiazu/RepData_PeerAssessment1/blob/master/instructions_fig/Time%20series.png)<!-- -->

#### The 5-minute interval contains the max number of steps:

```r
MaxInterval<- sqldf("Select interval,max(AvgSteps) from AvgStepsbyInterval")
MaxInterval
```

```
##   interval max(AvgSteps)
## 1      835      206.1698
```
<br><br>

### 4. Imputing missing values:

In this case **mean of the intervals** is used

```r
MeanSteps<-sqldf("Select avg(AvgSteps) from AvgStepsbyInterval")
missing_dt$steps <-MeanSteps$`avg(AvgSteps)`
```

assign avg to all NA in a new dataset:

```r
new_dt <- sqldf("Select * from dt1
                union all
                Select * from missing_dt")
```

#### Histogram after missing values are imputed:

```r
New_StepsPerDay <- sqldf("Select date,sum(Steps) as TotalSteps from new_dt group by date")

hist(New_StepsPerDay$TotalSteps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
```

![](https://github.com/Adiazu/RepData_PeerAssessment1/blob/master/instructions_fig/Histogram%202.png)<!-- -->

<br><br>

### 5. Differences in activity patterns between weekdays and weekends:

Panel plot:

```r
summary(New_StepsPerDay)
```

```
##      date             TotalSteps   
##  Length:61          Min.   :   41  
##  Class :character   1st Qu.: 9819  
##  Mode  :character   Median :10766  
##                     Mean   :10766  
##                     3rd Qu.:12811  
##                     Max.   :21194
```

```r
summary(StepsPerDay)
```

```
##      date             TotalSteps   
##  Length:53          Min.   :   41  
##  Class :character   1st Qu.: 8841  
##  Mode  :character   Median :10765  
##                     Mean   :10766  
##                     3rd Qu.:13294  
##                     Max.   :21194
```

#### New Factor of weekdays and weekends

```r
new_dt$days <- weekdays(as.Date(new_dt$date))
weekend <- c("Sunday","Saturday")
weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
new_dt$days[new_dt$days %in% weekend] <- "Weekend"
new_dt$days[new_dt$days %in% weekday] <- "weekday"
```

#### Difference between weekdays and weekends

```r
mean_number_steps <- aggregate(steps~ interval+days, new_dt, mean)
g <- qplot(interval, steps, data = mean_number_steps, facets = days~.)
g + geom_line(size = 1) + ylab("Mean steps") + ggtitle("Average number of steps taken, \n averaged across all weekday days or weekend days ")
```

![](https://github.com/Adiazu/RepData_PeerAssessment1/blob/master/instructions_fig/Weekdays%20and%20Weekends.png)<!-- -->
