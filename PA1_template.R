#reading the file to R.
fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile ="C:/Users/123/Desktop/data/activity.zip")
unzip("C:/Users/123/Desktop/data/activity.zip",exdir = "C:/Users/123/Desktop/data")
activity<-read.csv("C:/Users/123/Desktop/data/activity.csv")
head(activity)

#Histogram of the total number of steps taken each day.
##calculating total steps taken each day
##creating a png file named hist_of_steps
png(filename ="C:/Users/123/Desktop/PA1-template-figures/hist_of_steps.png",width = 480,height = 480,units = "px" )
step_sum<-tapply(activity$steps,activity$date,sum)
##plotting and annotating the histogram
hist(step_sum,breaks = 10,col="blue",xlim = c(0,25000),ylim =c(0,20),main="Total Number of Steps Taken Each Day")
#closing the device
dev.off()

#Mean and median number of steps taken each day
mean<-mean(step_sum,na.rm = TRUE)
median<-median(step_sum,na.rm = TRUE)

#Time series plot of the average number of steps taken
##creating a png file
png(filename ="C:/Users/123/Desktop/PA1-template-figures/time_series_plot_of_average_steps.png",width = 480,height = 480,units = "px" )
##transform the variable "interval" to a factor variable.
activity<-transform(activity,interval=factor(interval))
#calculating the average steps taken each day
stepsPerInterval<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
##plotting and annotatin the histogram
plot(names(stepsPerInterval),stepsPerInterval,type = "l",main = "Average Steps Per Interval",xlab = "5-minute Interval",ylab = "Average Steps")
#closing the device
dev.off()

#The maximum 5-minute interval that contains the maximum number of steps.
maximum<-names(which.max(stepsPerInterval))

#Imputing missing values
##calculating the total number of missing values
missing_values<-nrow(activity[is.na(activity),])

#filling all the missing values and create a new dataset with all the missing values filled in
##create a new dataset that is the copy of the original dataset.
newActivity<-activity
##filling all the missing values with the average steps taken each day
newActivity[is.na(newActivity$steps),"steps"]<-mean(newActivity$steps,na.rm=TRUE)

#Histogram of the total number of steps taken each day for the new dataset.
#creating a png file
png(filename ="C:/Users/123/Desktop/PA1-template-figures/hist_imputing_missing_values.png",width = 480,height = 480,units = "px" )
new_step_sum<-tapply(newActivity$steps,newActivity$date,sum)
hist(new_step_sum,col = "orange",breaks=10,main="Total Steps Taken Each Day",xlab = "Steps",xlim=c(0,25000),ylim=c(0,30))
#closing the device
dev.off()

#mean and median total number of steps taken per day for the new dataset.
newMean<-mean(new_step_sum,na.rm=TRUE)
newMedian<-median(new_step_sum,na.rm=TRUE)

#Create a new factor variable in the dataset with two levels:¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.
Sys.setlocale("LC_ALL","English")
#Transform the variable "date" to the class of "POSIXct" for further calculation
newActivity$date<-as.POSIXct(newActivity$date)
#Create a new variable named "day".
newActivity$day<-"Weekdays"
#The variable day will indicate whether the given date is weekday or weekend.
newActivity$day[weekdays(newActivity$date) %in% c("Saturday", "Sunday")] <- "Weekend"
#Subsetting the data frame on weekends
Weekend<-subset(newActivity,day=="Weekend")
#calculating the average steps taken on weekends per intervals
WeekendMean<-tapply(Weekend$steps,Weekend$interval,mean)
#Subsetting the data frame on weekdays.
Weekdays<-subset(newActivity,day=="Weekdays")
#calculating the average steps taken on weekdays per intervals
WeekdaysMean<-tapply(Weekdays$steps,Weekdays$interval,mean)
#Making the time series plots on weekdays and weekends.
#creating a png file
png(filename ="C:/Users/123/Desktop/PA1-template-figures/time_series_plot_on_weekdays_and_weekends.png",width = 480,height = 480,units = "px" )
par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(names(WeekendMean),WeekendMean,type="l",ylim=c(0,200),xlab = "5-minute-Interval",ylab = "Average Steps",main = "Average Steps on Weekend")
plot(names(WeekdaysMean),WeekdaysMean,type = "l",ylim=c(0,200),xlab = "5-minute-Interval",ylab = "Average Steps",main = "Average Steps on Weekdays")
#closing the device
dev.off()
