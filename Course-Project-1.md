---
title: "Project 1 Reproducible Research"
author: "Emiliano Olmedo"
date: "9/9/2020"
output: 
  html_document:

   keep_md: true
---


##Important Note English is not mi native language, i hope i didn't have to many gramatical errors


We are requested to pre process the data to do a histogram of steps taken each day, AMdata(Activity Monitor..) has all the data set, with no care of NA existence, this DataSet will be use at some other point but first we use [complete.cases] command to extract just complete cases.



```r
AMdata<-read.csv('activity.csv',stringsAsFactors = FALSE,sep=",", colClasses = c('numeric',NA,"numeric")) 
ProcesingData<-AMdata[complete.cases(AMdata),] #important command to delete NA
AMdata2<-as_tibble(ProcesingData)
AMdata2$date<-ymd(AMdata2$date)
```
With aggregate function we Sum the data usign the date as a key (AMdata2 is the same data set of complete cases but using dplyr library and lubridate)


```r
SumSteps<-aggregate(AMdata2$steps, by=list(AMdata2$date), sum)
```



```r
ggplot(SumSteps, aes(x=Group.1, y=x)) + geom_bar(stat="identity") +ylab("Frequency") + xlab("Year and Month") +
  theme_bw() 
```

![](./figure/question1-1.png)<!-- -->

In question 3 we have to get the mean and median number of steps taken each day, to do it so, we have to notice that each day is divided by intervals, because of this we need to subset the data set using each day as a filter, the variable [partial] is the result of such process, after that [resul] variable creates two column variables called [Avg] and [Med], finally we bind each iteration inside [s] Data frame.

in the previous step we loose the day column, to correct this we bind the data frames [l(UniqueDates)] and [s]  in the data frame within [Timeseries]
IMPORTANT NOTE: because we use Uniquedates in the for loop we know that the order of the rows in [s] and [l] is the same, something that is important when we bind them, otherwise a diferent approach must be use.

```r
UniqueDates<-unique(AMdata2$date)
s <- data.frame()
for (val in 1:length(UniqueDates)) {
  partial <- subset(AMdata2, AMdata2$date == UniqueDates[[val]])
  resul <-summarise(partial,
              Avg = mean(partial$steps),
              Med = median(partial$steps))
  s <- rbind(s, resul)
}

l<-data.frame(UniqueDates)
Timeseries<-cbind(l,s)

names(Timeseries)[1]<-'Fecha'
names(Timeseries)[2]<-'AvgSteps'    #renaming variables
names(Timeseries)[3]<-'Median'
Timeseries$Fecha<-ymd(Timeseries$Fecha)

x0<-xtable(Timeseries)
x<-xtable(summary(Timeseries))
```

Th3 former code was used thinking in the point 4-'Time series plot of the average number of steps taken'


```r
print(x0, type='html')
```

<!-- html table generated in R 4.0.1 by xtable 1.8-4 package -->
<!-- Fri Sep 11 17:19:00 2020 -->
<table border=1>
<tr> <th>  </th> <th> Fecha </th> <th> AvgSteps </th> <th> Median </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 15615.00 </td> <td align="right"> 0.44 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right"> 15616.00 </td> <td align="right"> 39.42 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right"> 15617.00 </td> <td align="right"> 42.07 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right"> 15618.00 </td> <td align="right"> 46.16 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right"> 15619.00 </td> <td align="right"> 53.54 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right"> 15620.00 </td> <td align="right"> 38.25 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right"> 15622.00 </td> <td align="right"> 44.48 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td align="right"> 15623.00 </td> <td align="right"> 34.38 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td align="right"> 15624.00 </td> <td align="right"> 35.78 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td align="right"> 15625.00 </td> <td align="right"> 60.35 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td align="right"> 15626.00 </td> <td align="right"> 43.15 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td align="right"> 15627.00 </td> <td align="right"> 52.42 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td align="right"> 15628.00 </td> <td align="right"> 35.20 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td align="right"> 15629.00 </td> <td align="right"> 52.38 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td align="right"> 15630.00 </td> <td align="right"> 46.71 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td align="right"> 15631.00 </td> <td align="right"> 34.92 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td align="right"> 15632.00 </td> <td align="right"> 41.07 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td align="right"> 15633.00 </td> <td align="right"> 36.09 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td align="right"> 15634.00 </td> <td align="right"> 30.63 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td align="right"> 15635.00 </td> <td align="right"> 46.74 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 21 </td> <td align="right"> 15636.00 </td> <td align="right"> 30.97 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 22 </td> <td align="right"> 15637.00 </td> <td align="right"> 29.01 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 23 </td> <td align="right"> 15638.00 </td> <td align="right"> 8.65 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 24 </td> <td align="right"> 15639.00 </td> <td align="right"> 23.53 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 25 </td> <td align="right"> 15640.00 </td> <td align="right"> 35.14 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 26 </td> <td align="right"> 15641.00 </td> <td align="right"> 39.78 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 27 </td> <td align="right"> 15642.00 </td> <td align="right"> 17.42 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 28 </td> <td align="right"> 15643.00 </td> <td align="right"> 34.09 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 29 </td> <td align="right"> 15644.00 </td> <td align="right"> 53.52 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 30 </td> <td align="right"> 15646.00 </td> <td align="right"> 36.81 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 31 </td> <td align="right"> 15647.00 </td> <td align="right"> 36.70 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 32 </td> <td align="right"> 15649.00 </td> <td align="right"> 36.25 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 33 </td> <td align="right"> 15650.00 </td> <td align="right"> 28.94 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 34 </td> <td align="right"> 15651.00 </td> <td align="right"> 44.73 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 35 </td> <td align="right"> 15652.00 </td> <td align="right"> 11.18 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 36 </td> <td align="right"> 15655.00 </td> <td align="right"> 43.78 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 37 </td> <td align="right"> 15656.00 </td> <td align="right"> 37.38 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 38 </td> <td align="right"> 15657.00 </td> <td align="right"> 25.47 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 39 </td> <td align="right"> 15659.00 </td> <td align="right"> 0.14 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 40 </td> <td align="right"> 15660.00 </td> <td align="right"> 18.89 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 41 </td> <td align="right"> 15661.00 </td> <td align="right"> 49.79 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 42 </td> <td align="right"> 15662.00 </td> <td align="right"> 52.47 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 43 </td> <td align="right"> 15663.00 </td> <td align="right"> 30.70 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 44 </td> <td align="right"> 15664.00 </td> <td align="right"> 15.53 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 45 </td> <td align="right"> 15665.00 </td> <td align="right"> 44.40 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 46 </td> <td align="right"> 15666.00 </td> <td align="right"> 70.93 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 47 </td> <td align="right"> 15667.00 </td> <td align="right"> 73.59 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 48 </td> <td align="right"> 15668.00 </td> <td align="right"> 50.27 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 49 </td> <td align="right"> 15669.00 </td> <td align="right"> 41.09 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 50 </td> <td align="right"> 15670.00 </td> <td align="right"> 38.76 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 51 </td> <td align="right"> 15671.00 </td> <td align="right"> 47.38 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 52 </td> <td align="right"> 15672.00 </td> <td align="right"> 35.36 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 53 </td> <td align="right"> 15673.00 </td> <td align="right"> 24.47 </td> <td align="right"> 0.00 </td> </tr>
   </table>

```r
print(x, type='html')
```

<!-- html table generated in R 4.0.1 by xtable 1.8-4 package -->
<!-- Fri Sep 11 17:19:00 2020 -->
<table border=1>
<tr> <th>  </th> <th>     Fecha </th> <th>    AvgSteps </th> <th>     Median </th>  </tr>
  <tr> <td align="right"> X </td> <td> Min.   :2012-10-02   </td> <td> Min.   : 0.1424   </td> <td> Min.   :0   </td> </tr>
  <tr> <td align="right"> X.1 </td> <td> 1st Qu.:2012-10-16   </td> <td> 1st Qu.:30.6979   </td> <td> 1st Qu.:0   </td> </tr>
  <tr> <td align="right"> X.2 </td> <td> Median :2012-10-29   </td> <td> Median :37.3785   </td> <td> Median :0   </td> </tr>
  <tr> <td align="right"> X.3 </td> <td> Mean   :2012-10-30   </td> <td> Mean   :37.3826   </td> <td> Mean   :0   </td> </tr>
  <tr> <td align="right"> X.4 </td> <td> 3rd Qu.:2012-11-16   </td> <td> 3rd Qu.:46.1597   </td> <td> 3rd Qu.:0   </td> </tr>
  <tr> <td align="right"> X.5 </td> <td> Max.   :2012-11-29   </td> <td> Max.   :73.5903   </td> <td> Max.   :0   </td> </tr>
   </table>

```r
p<-ggplot(Timeseries,aes(x=Fecha, y=AvgSteps))+geom_line(size=2,linetype=1,col = "blue")+geom_point(size=3)
print(p)
```

![](./figure/question4-1.png)<!-- -->

First we find each intervals, something that is done in [uniqueinterval] knowing the interval we use a for loop looking forward to sum all the steps in a specific interval, finally we bind the interval with it corresponding sum [conjunto] and calculate the mean of each interval, at this point the question we want to answer is 5-'The 5-minute interval that, on average, contains the maximum number of steps', this is answered in [colmax].


```r
uniqueinterval<-unique(AMdata2$interval)
s2<-data.frame()
mm<-unlist(list(uniqueinterval))

for(val2 in mm){

  temp<-filter(AMdata2,interval==val2)
  suma<-sum(temp$steps)
  s2<-rbind(s2,suma)  #S2 is the sum of steps by each interval 
}

conjunto<-cbind(unique(AMdata2$interval),s2) #adding interval label to their sum of steps
names(conjunto)[2]<-'SumaSteps'
denominator<-length(UniqueDates)
conjunto<-mutate(conjunto,avg=SumaSteps/denominator)
colmax <- sapply(conjunto, max, na.rm = TRUE)
x1<-kable(colmax)
print(x1)
```

                                     x
-------------------------  -----------
unique(AMdata2$interval)     2355.0000
SumaSteps                   10927.0000
avg                           206.1698

In the Data set MeanSteps we calculate the mean of the interval using all the days, this approach is questionable because we are assuming that each day we perform the same activities a more subtle approche will be distinguish the data using weekdays and weekends or using only all mondays, all tuesdays etc to do the mean, in any case we took just the mean of the interval. NaSteps is the variable of those cases with NA values so within our for loop we use the interval i and when this variable coincides with the interval of MeanSteps we append that value into the data frame NaSteps. 
6-Code to describe and show a strategy for imputing missing data


```r
MeanSteps<-aggregate(AMdata2$steps, by=list(AMdata2$interval), mean)
NaSteps<-AMdata[!complete.cases(AMdata),]
for(i in NaSteps$interval){
  if(i == MeanSteps$Group.1 ) NaSteps$steps <- MeanSteps$x
}
```

we've got to bind by rows [NaSteps] (!complete.cases(AMdata)) with [ProcessingData] (complete.cases(AMdata)) this two data sets are disjoint due to [!] so there is no way we replace or duplicate an existing value.

Using the complete data set we plot 7-'Histogram of the total number of steps taken each day after missing values are imputed'


```r
Fullcases<-rbind(NaSteps,ProcesingData)
Fullcases2<-tbl_df(Fullcases)
Fullcases2$date<-ymd(Fullcases2$date)
FullSumSteps<-aggregate(Fullcases2$steps, by=list(Fullcases2$date), sum)
ggplot(FullSumSteps, aes(x=Group.1, y=x)) + geom_bar(stat="identity") +ylab("Frequency") + xlab("Year and Month") +
  theme_bw() 
```

![](./figure/question7-1.png)<!-- -->


Finally we separate the data depending if it´s weekday or weekend we check each row of fullcases and evaluate if it's inside [c('sábado','domingo')=c('saturday','sunday')] and depending the outcome append tho one or another, the last part of the code is just the sum of steps taken each day, we add a categorical column because i´ll use lattice plotting system.



```r
dtweekdays<-data.frame()
dtweekends<-data.frame()
`%notin%` <- Negate(`%in%`)
for (i in 1:nrow(Fullcases)) if (weekdays(as.Date.character(Fullcases[i,2])) %notin% c('sábado','domingo')){
  dtweekdays<-rbind(dtweekdays,Fullcases[i,])
  
} else {
  dtweekends<-rbind(dtweekends,Fullcases[i,])
}

MeanWeekDay<-aggregate(dtweekdays$steps, by=list(dtweekdays$interval), sum)
MeanWeekEnds<-aggregate(dtweekends$steps, by=list(dtweekends$interval), sum)
MeanWeekDay<-mutate(MeanWeekDay,Laboral='Laboral')
MeanWeekEnds<-mutate(MeanWeekEnds,Laboral='No Laboral')
temp5<-rbind(MeanWeekDay,MeanWeekEnds)

# Each group in a separate mini plot
xyplot(x ~ Group.1 | Laboral, data = temp5, col="red",type='l',lwd=2)
```

![](./figure/question8-1.png)<!-- -->


I 'am an amateur  using R so if you have any recommendations please feel free to sendme an email, i'll gladly check your comments

Atte Emiliano Olmedo
emiliano_fisica@ciencias.unam.mx

