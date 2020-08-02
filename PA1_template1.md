\#\#Upload to R

``` r
library(markdown)
library(knitr)
opts_chunk$set(echo=TRUE)
setwd("C:/Users/juanc/OneDrive/Escritorio")
activity<-read.csv("./activity.csv")
```

\#\#Histogram, total numbers of steps

``` r
totalSteps<-tapply(activity$steps,activity$date,sum)
stepsDates<-names(totalSteps)
totalSteps<-as.data.frame(totalSteps)
library(ggplot2)
ggplot(totalSteps, aes(totalSteps))+geom_histogram()+labs(title = "Total steps") + labs(x = "Steps")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#dev.copy(png, file = "HistogramNA.png", width=480, height=480) 
#dev.off() 
```

\#\#mean and median

``` r
m<-mean(totalSteps$totalSteps, na.rm=TRUE)
med<-median(totalSteps$totalSteps, na.rm=TRUE)
```

*Mean=1.076618910^{4} *Median=10765

\#\#Time series, average of number of steps taken per interval

``` r
new<-activity
new<-new[-c(2)]
library(reshape2)
meltdata<-melt(new, id=c( "interval"))
Final<-dcast(meltdata,interval~variable, mean,na.rm=TRUE)

ggplot(Final, aes(y=Final$steps ,x=Final$interval, group=1))+geom_line()+geom_point()+labs(title = "Time Series Steps") + labs(x = "Interval")+labs(y="Steps")
```

    ## Warning: Use of `Final$interval` is discouraged. Use `interval` instead.

    ## Warning: Use of `Final$steps` is discouraged. Use `steps` instead.

    ## Warning: Use of `Final$interval` is discouraged. Use `interval` instead.

    ## Warning: Use of `Final$steps` is discouraged. Use `steps` instead.

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
#dev.copy(png, file = "TimeSeriesNA.png", width=480, height=480) 
#dev.off() 
```

\#\#The 5-minute interval that, on average that contains the maximum
number of steps

``` r
maximum<-activity$interval[which(activity$steps==max(activity$steps, na.rm=TRUE))]
```

\*Maximum steps interval=615

\#\#Number of NA’s

``` r
summary(activity$steps)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    0.00    0.00   37.38   12.00  806.00    2304

\#\#Impute missing values

``` r
## Impute the NA values with the mean of the 5 minute interval
nonNA<-activity
c<-which(is.na(nonNA$steps)==TRUE)
nonNA$steps[c]<-mean(activity$steps, na.rm=TRUE) 
```

\#\#Histogram with imputed NA

``` r
STEPS<-tapply(nonNA$steps,nonNA$date,sum)
STEPS<-as.data.frame(STEPS)
ggplot(STEPS, aes(STEPS))+geom_histogram()+labs(title = "Total steps") + labs(x = "Steps")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
#dev.copy(png, file = "HistogramImputed.png", width=480, height=480) 
#dev.off() 
```

\#\#mean and median with imputed NA

``` r
nonNAmean<-mean(STEPS[,1])
nonNAmedian<-median(STEPS[,1])
```

*Mean=1.076618910^{4} *Median=1.076618910^{4}

\#\#Weekdays and Weekend

``` r
nonNA$date<-as.Date(nonNA$date)
wd<-weekdays(nonNA$date)
nonNA<-cbind(nonNA,wd)
d<-which(nonNA$wd=="sábado" | nonNA$wd=="domingo")
e<-which(nonNA$wd=="lunes" | nonNA$wd=="martes" | nonNA$wd=="miércoles" | nonNA$wd=="jueves" | nonNA$wd=="viernes")

nonNA$wd[d]<-"weekend"
nonNA$wd[e]<-"weekdays"

nonNA<-transform(nonNA, wd=factor(wd))

new2<-nonNA
new2<-new2[-c(2)]
library(reshape2)
meltdata2<-melt(new2, id=c( "interval", "wd"))
Final2<-dcast(meltdata2,wd+interval~variable, mean,na.rm=TRUE)

ggplot(Final2, aes(x=Final2$interval ,y=Final2$steps, group=1))+geom_line()+geom_point()+facet_grid(. ~ wd)+labs(title = "Time Series Steps") + labs(x = "5 minute interval")+labs(y="Steps")
```

    ## Warning: Use of `Final2$interval` is discouraged. Use `interval` instead.

    ## Warning: Use of `Final2$steps` is discouraged. Use `steps` instead.

    ## Warning: Use of `Final2$interval` is discouraged. Use `interval` instead.

    ## Warning: Use of `Final2$steps` is discouraged. Use `steps` instead.

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
#dev.copy(png, file = "TimeSeriesImputed.png", width=480, height=480) 
#dev.off() 
```
