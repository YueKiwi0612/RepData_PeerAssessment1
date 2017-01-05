#1. Loading and preprocessing the data
#set working directory
setwd("/Users/Eva/Documents/Coursera Courses/ReproductibleResearch")
#load the data
rawdata <- read.csv("activity.csv", stringsAsFactors = FALSE)
head(rawdata)
#transform the date data
rawdata$date <- as.POSIXct(rawdata$date, format="%Y-%m-%d")

#2. What is the mean total number of steps taken per day?
#remove NAs
rawdata.noNA <- na.omit(rawdata)
StepsPerDay <- aggregate(steps ~ date, rawdata.noNA, FUN = sum)
#histogram
hist(StepsPerDay$steps, col="blue", main="Total Number of Steps Taken Per Day",xlab="Total Number of Steps")
#calculate the mean and median
TotalStepsMean <- mean(StepsPerDay$steps)
TotalStepsMean
TotalStepsMed <- median(StepsPerDay$steps)
TotalStepsMed

#3. What is the average daily activity pattern?
StepsPerInterval <- aggregate(steps ~ interval, rawdata.noNA, mean)
plot(StepsPerInterval$interval,type = "l", StepsPerInterval$steps, main="Average Number of
     Steps in Each Interval",xlab="5-Minute Intervals",ylab="Average Steps Taken")
#find out the interval for the maximum average steps
index <- which.max(StepsPerInterval$steps)
StepsPerInterval[index,]

#4. Imputing missing values
#count NAs
sum(is.na(rawdata$steps))
rawdata2 <- rawdata
for (i in which(is.na(rawdata2$steps))){
        #get the 5-minute interval corresponding to the NA, and then the average steps for that particular interval
        int <- rawdata2$interval[i]
        avgStepsInt <- StepsPerInterval[which(StepsPerInterval$interval == int),]$steps
        #assign the mean steps for NA
        rawdata2$steps[i] <- avgStepsInt
        
}
#compute total number of steps in each day with filled NA values
StepsPerDayFilled <- aggregate(steps ~ date, rawdata2,sum)
hist(StepsPerDayFilled$steps, col="red",main="Total Number of Steps Taken Each Day (Filled NAs)",
     xlab="Total Number of Steps")
TotalStepsMean2 <- mean(StepsPerDayFilled$steps)
TotalStepsMean2
TotalStepsMed2 <- median(StepsPerDayFilled$steps)
TotalStepsMed2

#5. Are there differences in activity patterns between weekdays and weekends?
#create a new variable based on day type
rawdata2$daytype <- factor(ifelse(weekdays(rawdata2$date) %in% c("Saturday","Sunday"),
                       "weekend","weekday"))
#time series plots of the average steps taken in each intervals which averaged across all weekdays and weekends
avgStepsPerIntervalBydaytype <- aggregate(steps ~ interval + daytype, rawdata2, mean)
library(lattice)
xyplot(steps ~ interval|daytype, data=avgStepsPerIntervalBydaytype
       ,type="l",layout=c(1,2),main="Average Steps in 5-Minute Intervals During Weekends vs Weekdays",
       xlab="5-Minute Intervals",ylab="Average Steps Taken")

