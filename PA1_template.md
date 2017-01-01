# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
'''{r}
library('dplyr')
activity <- read.csv("C:/Users/Christian/datasciencecoursera/Class4/activity.csv")
good <- complete.cases(activity)
activity_NA <- activity[!good, ]
activity <- activity[good, ]
head(activity)
'''

## What is mean total number of steps taken per day?


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
