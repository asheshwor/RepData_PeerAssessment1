#Loading and preprocessing the data

act <- read.table("./data/activity.csv", sep=",", header=TRUE, na.strings= "NA",
                  colClasses=c("integer", "Date", "integer"))
head(act)
str(act)
nrow(act)
hist(act[,1])
#What is mean total number of steps taken per day?

require(plyr)
act.complete <- act[complete.cases(act),]
head(act.complete)

#add day column
tail(act.complete)
act.complete$day <- as.numeric(strftime(as.Date(act.complete$date), format='%j'))

#smmary of steps/day

stepsPerDay <- ddply(act, c("date"), function(xdf) {
  sum <- sum(xdf$steps, na.rm=TRUE)
  mean <- mean(xdf$steps, na.rm=TRUE)
  median <- median (xdf$steps, na.rm=TRUE)
  return(data.frame(cbind(sum, mean, median)))})
head(stepsPerDay)
#mean
meanSteps <- mean(stepsPerDay$sum)
#median
medianSteps <- median(stepsPerDay$sum)

with(stepsPerDay, plot(date, sum, main="Total number of steps per day", ylab="Steps",
                       xlab="Date"))
with(stepsPerDay, lines(date, sum))
#with(stepsPerDay, abline(h=meanSteps, col="blue"))
#with(stepsPerDay, abline(h=medianSteps, col="red"))
hist(stepsPerDay$sum)

require(ggplot2)
hplot <- ggplot(stepsPerDay, aes(x=sum)) +
  geom_histogram(binwidth=1000, colour="black", fill="lightblue") +
  #geom_density(alpha=.2, colour="red3") +
  geom_vline(data=stepsPerDay, aes(xintercept=mean(sum, na.rm=T)), size=1, linetype=2, colour="blue") +
  #geom_vline(data=stepsPerDay, aes(xintercept=median(sum, na.rm=T)), size=1, linetype=2, colour="red") +
  ggtitle("Distribution of steps per day")

#What is the average daily activity pattern?
stepsPerInterval <- ddply(act, c("interval"), function(xdf) {
  sum <- sum(xdf$steps, na.rm=TRUE)
  mean <- mean(xdf$steps, na.rm=TRUE)
  median <- median (xdf$steps, na.rm=TRUE)
  return(data.frame(cbind(sum, mean, median)))})

with(stepsPerInterval, plot(interval, sum, main="average daily activity pattern", ylab="Steps",
                       xlab="Date"))
with(stepsPerInterval, lines(interval, sum))
#max stpes in which interval
stepsPerInterval[stepsPerInterval$sum == max(stepsPerInterval$sum),]

#Imputing missing values
sum(is.na(act$steps))

xdf <- act
xdf$steps2 <- xdf$steps

getMeanSteps <- function(xinterval) {
  value <- NULL
  for (i in 1:length(xinterval))
  {
    value <- c(value, stepsPerInterval[stepsPerInterval$interval == xinterval[i], c(2)])
  }
  return(value)
}
rm(xdf)
#getMeanSteps(c(2345, 0,5,10, 2220))
imputeStepCount <- function(xdf) {
  nax <- which(is.na(xdf$steps))
  xdf$steps2[nax] <- getMeanSteps(xdf$interval[nax])
  #xdf$steps2[nax] <- stepsPerInterval[stepsPerInterval$interval == nax, c(2)]
  return(xdf$steps2)
}
#act2 <- ddply(act, transform, steps2 = imputeStepCount(steps))
act$steps2 <- imputeStepCount(act)
tail(act)
act$steps2 <- as.integer(act$steps2)
stepsPerDay2 <- ddply(act, c("date"), function(xdf) {
  sum <- sum(xdf$steps2, na.rm=TRUE)
  mean <- mean(xdf$steps2, na.rm=TRUE)
  median <- median (xdf$steps2, na.rm=TRUE)
  return(data.frame(cbind(sum, mean, median)))})

hplot <- ggplot(stepsPerDay2, aes(x=sum)) +
  geom_histogram(binwidth=1000, colour="black", fill="lightblue") +
  #geom_density(alpha=.2, colour="red3") +
  geom_vline(data=stepsPerDay2, aes(xintercept=mean(sum, na.rm=T)), size=1, linetype=2, colour="blue") +
  #geom_vline(data=stepsPerDay, aes(xintercept=median(sum, na.rm=T)), size=1, linetype=2, colour="red") +
  ggtitle("Distribution of steps per day")

stepsPerInterval2 <- ddply(act, c("interval"), function(xdf) {
  sum <- sum(xdf$steps2, na.rm=TRUE)
  mean <- mean(xdf$steps2, na.rm=TRUE)
  median <- median (xdf$steps2, na.rm=TRUE)
  return(data.frame(cbind(sum, mean, median)))})

with(stepsPerInterval2, plot(interval, sum, main="average daily activity pattern", ylab="Steps",
                            xlab="Date"))
with(stepsPerInterval2, lines(interval, sum))
with(stepsPerInterval, lines(interval, sum, col="blue"))

act2$weekday <- weekdays(act2$date)
tail(act2,3000)
is.weekend <- function(xdate) {
  if (weekdays(xdate) == "Saturday" | weekdays(xdate) == "Sunday") {return("Weekend")} else
  {return("Weekday")}
}
#is.weekend(as.Date("2012-10-01"))
act2$isweekend <- sapply(act2$date, is.weekend)
