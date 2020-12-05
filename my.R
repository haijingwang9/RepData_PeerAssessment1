library(dplyr)
library(ggplot2)

#######################################################################
## unpack data, read data, transform date
#######################################################################
## unzip("activity.zip")
activity <- read.csv("activity.csv", header=TRUE)
activity <- mutate(activity, date = as.Date(date, format="%Y-%m-%d"))

#######################################################################
## daily steps sum, mean daily steps
#######################################################################
activity %>% group_by(date) %>% summarize(dailysteps = sum(steps, na.rm=TRUE)) -> dailysteps

p1 <- qplot(dailysteps$dailysteps, geom="histogram", bins=12, 
      main="Histogram for Daily steps", xlab = "# of Steps per day", 
      ylab="Number of Days", fill = I("Steelblue"), col=I("red"))
print(p1)
mean <- mean(dailysteps$dailysteps)
median <- median(dailysteps$dailysteps)

#######################################################################
## Average Daily Activity Pattern
#######################################################################
activity %>% group_by(interval) %>% summarize(steps= mean(steps, na.rm=TRUE)) -> intervalsteps
p2 <- ggplot(intervalsteps, aes(x = interval, y=steps)) + geom_line(color="blue")
print(p2)

#######################################################################
## Impute missing values
#######################################################################
## total number of records with missing value
sum(is.na(activity$steps))
## percentage of records with missing value
sum(is.na(activity$steps)) / nrow(activity)

## fill in missing value with mean of 5 minute interval steps
## Make a copy of original data frame
filled <- data.frame(activity)
whichna <- which(is.na(filled$steps))
for( val in whichna){
  itv <- filled[val, 3]
  filled[val, 1] <- intervalsteps[intervalsteps$interval==itv, 2]
}

## histogram for imputed daily steps
filled %>% group_by(date) %>% summarize( stepsPerDay = sum(steps, na.rm=TRUE)) -> imputed
p3 <- qplot(imputed$stepsPerDay, geom="histogram", bins=12, 
      main="Histogram for Daily steps Imputed", xlab = "# of Steps per day", 
      ylab="Number of Days", fill = I("Orange"), col=I("black"))
print(p3)
imputedmean <- mean(imputed$stepsPerDay)
imputedMedian <- median(imputed$stepsPerDay)

#######################################################################
## Difference in activity pattern between weekday and weekend
#######################################################################
weekends <- c("Saturday", "Sunday")
twopattern <- mutate(filled, ifweekend = ( weekdays(date) %in% weekends))
twopattern %>% group_by(interval, ifweekend) %>% summarize(steps= mean(steps, na.rm=TRUE)) -> newstep

p4 <- ggplot(newstep, aes(x = interval, y=steps), group=ifweekend)
p4 <- p4 + geom_line(aes(color=ifweekend)) + geom_point(aes(color=ifweekend))
p4 <- p4 + facet_grid(ifweekend ~ .)
print(p4)

