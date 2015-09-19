
raw_data <- read.csv("activity.csv")

#removing the missing records
good <- complete.cases(raw_data)
proc_data <- raw_data[good,]



# Total Number of steps per day
TotalStepPerDay <- aggregate(proc_data[,1],list("Date" = proc_data$date),sum)
names(TotalStepPerDay) <- c("Date","TotalSteps")

# A histogram showing total number of steps per day
hist(TotalStepPerDay$TotalSteps, xlab = "Total Steps Per Day", main ="Total Number of Steps Per Day")
#dev.off()

# mean and median of the total number of steps per day
MeanTotalSteps <- mean(TotalStepPerDay$TotalSteps)
MedianTotalSteps <- median(TotalStepPerDay$TotalSteps)

The mean total number of steps per day is ```{R} print(MeanTotalSteps)```
The median total number of steps per day is ```{R} print(MedianTotalSteps)```

######


