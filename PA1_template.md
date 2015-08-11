---
title: "Reproducible Research: Peer Assessment 1"
author: "Tomas Mawyin"
output: 
  html_document:
    keep_md: yes
---

This assignment will work with data from a personal activity monitoring
device. The data contains 17568 observations and 3 variables that include:  

- the number of steps a person takes every 5 minutes (**steps**) 
- the date in which the data was collected (**date**)
- the interval identifier (**interval**)  

Each of the following sections will contain information of how the data is
processed and it also contains the R code to reproduce the results.

## Loading and preprocessing the data
The first step in processing the data is to load it and transform any variables
to an appropriate format. First, the *dplyr* package is loaded for convenience.

```r
library(dplyr)
```

Since the data is in a *.zip* format, the easiest way to load it is to unzip it
and then read the csv file. Also, it is important to transform the date variable
to an appropriate format. Finally, the data is converted to a more efficient format.


```r
unzip("activity.zip")
data <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
# Modifying the date format
data$date <- as.Date(data$date)
# Changing dataframe to tbl_df format
data <- tbl_df(data)
str(data)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The results above, show the structure of the loaded data set.

## What is mean total number of steps taken per day?

In this section, the following steps will be performed:  
1. Calculate the total number of steps per day  
2. Display the total numbers of steps per day in a histogram  
3. Report the mean and median of the number of steps per day  

To calculate the total number of steps per day, the *dplyr* package is very handy.
In addition, the **NA** values will be omitted for this part.


```r
steps.daily <-  data  %>% 
                na.omit(data) %>% 
                group_by(date) %>% 
                summarise(dailyStep = sum(steps, na.rm=TRUE))
# Checking the results
steps.daily
```

```
## Source: local data frame [53 x 2]
## 
##          date dailyStep
## 1  2012-10-02       126
## 2  2012-10-03     11352
## 3  2012-10-04     12116
## 4  2012-10-05     13294
## 5  2012-10-06     15420
## 6  2012-10-07     11015
## 7  2012-10-09     12811
## 8  2012-10-10      9900
## 9  2012-10-11     10304
## 10 2012-10-12     17382
## ..        ...       ...
```

To carry out step 2, the *ggplot2* package will be of use.


```r
library(ggplot2)
```

The histogram is generated as follows:


```r
ggplot(data = steps.daily, aes(dailyStep)) + 
    geom_histogram(breaks = seq(0, 21200, by=1000), color = "black", fill="blue",alpha = .75) +
    labs(title = "Histogram of daily steps") +
    labs(x = "Number of daily steps", y= "Frequency") +
    theme_classic(base_family = "Arial")
```

![plot of chunk histogram](figure/histogram-1.png) 

Note that the above histogram is set up with bin sizes of 1000 and
using a classic theme. Generating the mean and median of the data is simple
given that the *steps.daily* already contains the required data.


```r
mean(steps.daily$dailyStep)
```

```
## [1] 10766.19
```

```r
median(steps.daily$dailyStep)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

To understand this question, a time series plot of the average daily activity
is required. Based on the plot, it will be possible to determine which interval
value corresponds to the highest patter.  

Prior to plotting the data, it is necessary to obtain the average number of steps
given each of the intervals. This is done in the following code:


```r
avg.daily <- data  %>%
    # Clearing all NAs
    na.omit(data) %>% 
    # Grouping data based on the interval column
    group_by(interval) %>% 
    # Averaging all steps based on the given interval group
    mutate(meanSteps = mean(steps, na.rm = TRUE))
```

Now, using the *ggplot2* library, the time series can be obtained as follows:


```r
ggplot(data = avg.daily, aes(interval, meanSteps)) + 
    # Blue lines, why not!
    geom_line(color = "blue") +
    labs(title = "Average steps by interval") +
    labs(x = "Intervals", y = "Average Steps") +
    theme_classic(base_family = "Arial")
```

![plot of chunk timeseries](figure/timeseries-1.png) 

The above plot shows that the maximum average happens anywhere between the 700
and 900 interval. To obtain the exact interval:


```r
avg.daily[which.max(avg.daily$meanSteps),"interval"]
```

```
## Source: local data frame [1 x 1]
## Groups: interval
## 
##   interval
## 1      835
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?