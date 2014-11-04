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



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
