# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Reading in data

```r
data<-read.csv(unz("activity.zip","activity.csv"))
```
Creating a dataset without na's

```r
comp_data_bool<-complete.cases(data)
comp_data<-data[comp_data_bool,]
```

## What is mean total number of steps taken per day?
Calculating totals per day...

```r
days<-dimnames(table(comp_data$date))[[1]]
totals<-c()
for (d in days){
  tempdf<-comp_data[comp_data$date==d,]
  s<-sum(tempdf$steps)
  totals<-c(totals,s)
}
#...and mean!
tot_mean=mean(totals)
```
### graphing total steps vs. days

```r
barplot(totals)
#adding a line that represents the mean per day steps
abline(h=tot_mean,col="red")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## What is the average daily activity pattern?
Calculating average steps per time interval

```r
intervals<-dimnames(table(comp_data$interval))[[1]]
avg_steps<-c()
for (i in intervals){
  tempdf<-comp_data[comp_data$interval==i,]
  s<-mean(tempdf$steps)
  avg_steps<-c(avg_steps,s)
}
```
Time-series plot of average steps taken per interval

```r
plot(intervals, avg_steps,type="l")
max_avg<-max(avg_steps)
max_int<-match(max_avg,avg_steps)
points(intervals[max_int],max_avg,col="red", pch=21)
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
paste("The interval that has the largest average number of steps is",intervals[max_int],"and is indicated by the red dot at",max_avg,sep=' ')
```

```
## [1] "The interval that has the largest average number of steps is 835 and is indicated by the red dot at 206.169811320755"
```
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?