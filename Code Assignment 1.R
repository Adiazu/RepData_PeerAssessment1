# Loading the data

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity_data.zip")
unzip("activity_data.zip")
dt<- read.csv("activity.csv", stringsAsFactors = F)
# Checking the data
dim(dt)
head(dt)
tail(dt)


# checking the missing values
missing_dt <- dt[is.na(dt$steps),]
dim(missing_dt)

# Removing the NAs
dt1 <- dt[!is.na(dt$steps),]

# Calculate the total number of steps taken per day
library(sqldf)
StepsPerDay <- sqldf("Select date,sum(Steps) as TotalSteps from dt1 group by date")


hist(StepsPerDay$TotalSteps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")

summary(StepsPerDay)

# Calculate the avg step per day in each interval
AvgStepsbyInterval <- sqldf("Select interval,avg(Steps) as AvgSteps from dt1 group by interval")
plot(AvgStepsbyInterval$interval,AvgStepsbyInterval$AvgSteps, type = "l", main = "Time series plot of the \n average number of steps taken", xlab = "interval", ylab = "Mean steps")

MaxInterval<- sqldf("Select interval,max(AvgSteps) from AvgStepsbyInterval")
MaxInterval

# total number of missing values in the dataset
length(missing_dt$steps)


MeanSteps<-sqldf("Select avg(AvgSteps) from AvgStepsbyInterval")
missing_dt$steps <-MeanSteps$`avg(AvgSteps)`


#Create a new dataset that is equal to the original dataset but with the missing data filled in.
new_dt <- sqldf("Select * from dt1
                union all
                Select * from missing_dt")


#Make a histogram of the total number of steps taken each day
New_StepsPerDay <- sqldf("Select date,sum(Steps) as TotalSteps from new_dt group by date")

hist(New_StepsPerDay$TotalSteps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")


summary(New_StepsPerDay)
summary(StepsPerDay)


##Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
new_dt$days <- weekdays(as.Date(new_dt$date))
weekend <- c("Sunday","Saturday")
weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
new_dt$days[new_dt$days %in% weekend] <- "Weekend"
new_dt$days[new_dt$days %in% weekday] <- "weekday"


library(ggplot2)
mean_number_steps <- aggregate(steps~ interval+days, new_dt, mean)
g <- qplot(interval, steps, data = mean_number_steps, facets = days~.)
g + geom_line(size = 1) + ylab("Mean steps") + ggtitle("Average number of steps taken, \n averaged across all weekday days or weekend days ")