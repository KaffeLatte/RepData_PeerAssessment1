# Reproducible Research Assignment 1
# KaffeLatte@github

# Load needed libraries
library(ggplot2)
library(dplyr)
library(zoo)

# Set working directory
setwd("~/Coursera/DSspec/RR/RepData_PeerAssessment1")

# Note I don't download things in this script as we are anyways cloning the repo with the files in it
# Unzip stuff if not already done
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}

# Load data into R, and clean away NA
d <- read.csv("activity.csv")
data <- d[complete.cases(d),]

totalNrOfSteps <- data %>%
  group_by(date) %>%
  summarize(totalSteps = sum(steps)) 

totalNrOfSteps <- data.frame(totalNrOfSteps)
# Calculate mean and median

meanNrOfSteps <- data %>%
  group_by(date) %>%
  summarize(meanSteps = mean(steps))

medianNrOfSteps <-data %>%
  group_by(date) %>%
  summarize(medianSteps = median(steps))

# Plot histogram over the number of steps
g <- ggplot(totalNrOfSteps, aes(x = totalSteps))
g + geom_histogram()

# Calculate mean and median
meanNrOfSteps <- mean(data$step, na.rm = TRUE)
medianNrOfSteps <- median(data$step, na.rm = TRUE)

# Time series stuff
timeSerie <- data %>%
  group_by(interval) %>%
  summarize(IntervalMeanSteps = mean(sum(steps)))

g <- ggplot(data=timeSerie, aes(x = interval, y = IntervalMeanSteps))
g + geom_line() + 
  xlab("5-min interval") +
  ylab("Mean of steps taken")

maxStepInterval <- which.max(timeSerie$IntervalMeanSteps)

nrNA <- length(which(is.na(d)))

# strategy for filling in missing values: use mean of that day, if not all NA then use 0
imputedData <- d
imputedData[imputedData$date == "2012-10-01",]$steps <- 0
imputedData[imputedData$date == "2012-10-08",]$steps <- 0
imputedData[imputedData$date == "2012-11-01",]$steps <- 0
imputedData[imputedData$date == "2012-11-04",]$steps <- 0
imputedData[imputedData$date == "2012-11-09",]$steps <- 0
imputedData[imputedData$date == "2012-11-10",]$steps <- 0
imputedData[imputedData$date == "2012-11-14",]$steps <- 0
imputedData[imputedData$date == "2012-11-30",]$steps <- 0
imputedData <- imputedData %>%
                    group_by(date) %>%
                    mutate(steps = replace(steps, is.na(steps), mean(steps, na.rm=TRUE)))


totalImputed <- imputedData %>%
  group_by(date) %>%
  summarize(totalSteps = sum(steps)) 

# Calculate mean and median

imputedMean <- imputedData %>%
  group_by(date) %>%
  summarize(meanSteps = mean(steps))

imputedMedian <- imputedData %>%
  group_by(date) %>%
  summarize(medianSteps = median(steps))

# Plot histogram over the number of steps
g <- ggplot(totalImputed, aes(x = totalSteps))
g + geom_histogram()

imputedData$date <- as.Date(imputedData$date)
imputedData <- imputedData %>%
  mutate(dayType = ifelse(weekdays(date) %in% c("lördag", "söndag"), "weekend", "weekday"))

newTimeSerie <- imputedData %>%
  group_by(interval, dayType) %>%
  summarize(IntervalMeanSteps = mean(sum(steps)))


g <- ggplot(data=newTimeSerie, aes(x = interval, y = IntervalMeanSteps))
g + geom_line() + facet_grid(.~dayType) +
  xlab("5-min interval") +
  ylab("Mean of steps taken")


