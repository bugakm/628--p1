##Seasonal Analysis
library(dplyr)
library(timsac)
library(dynlm)
library(stats)
library(forecase)
library(bfast)
library(ggplot2)


#0. Cleaning
dataclean<-function(chr){
  data <-read.csv(chr,skip=3,colClasses = c("character",rep("NULL",2),rep("integer",5),rep("NULL",3)))
  data<-data %>%
    filter(Value != -999) %>%
    group_by(Month) %>%
    mutate(mean_Value_Mon = mean(Value)) %>%
    group_by(Month,Day) %>%
    mutate(mean_Value_Day = mean(Value)) %>%
    ungroup()
}
dailyclean<-function(chr){
  data <-read.csv(chr,skip=3,colClasses = c("character",rep("NULL",2),rep("integer",5),rep("NULL",3)))
  data<-data %>%
    filter(Value != -999) %>%
    group_by(Month) %>%
    mutate(mean_Value_Mon = mean(Value)) %>%
    group_by(Month,Day) %>%
    summarize(mean_Value_Day = mean(Value)) %>%
    ungroup()
}
###perform first cleaning
bj2015<-dataclean("bj2015.csv")
bj2014<-dataclean("bj2014.csv")
bj2013<-dataclean("bj2013.csv")
bj2012<-dataclean("bj2012.csv")
bj2011<-dataclean("bj2011.csv")
bj<-rbind(bj2011,bj2012,bj2013,bj2014,bj2015)
save(bj,file="bj.rdata")
dbj2015<-dailyclean("bj2015.csv")
dbj2014<-dailyclean("bj2014.csv")
dbj2013<-dailyclean("bj2013.csv")
dbj2012<-dailyclean("bj2012.csv")
dbj2011<-dailyclean("bj2011.csv")
dbj<-rbind(dbj2011,dbj2012,dbj2013,dbj2014,dbj2015)
save(dbj,file="dbj.rdata")



#1. Calculate Daily Index and Polluted Days

##Daily Index: Mean Value
load("dbj.rdata")

pollute<-function(data){
  data<-data %>%
    ##Polluted Days: Daily Index >= 150
    mutate(Polluted = as.numeric(mean_Value_Day >= 150)) %>%
    ##Heavily Polluted Days: Daily Index >= 300
    mutate(Hea_Polluted = as.numeric(mean_Value_Day >= 300))
}

dbj2011<-pollute(dbj2011)
dbj2011$year<-as.numeric(2011)
dbj2012<-pollute(dbj2012)
dbj2012$year<-as.numeric(2012)
dbj2013<-pollute(dbj2013)
dbj2013$year<-as.numeric(2013)
dbj2014<-pollute(dbj2014)
dbj2014$year<-as.numeric(2014)
dbj2015<-pollute(dbj2015)
dbj2015$year<-as.numeric(2015)
dbj<-rbind(dbj2012,dbj2013,dbj2014,dbj2015)

#2. Calculate Monthly Polluted Days and Monthly Average Index

monthpol<-function(data){
  data<-data %>%
    group_by(Year, Month) %>%
    summarize(aggPollute = sum(Polluted), aggHeavy = sum(Hea_Polluted))
}

mbj<-monthpol(dbj)
mbj<-data.frame(year=mbj$Year, month=mbj$Month, aggPollute=mbj$aggPollute, aggHeavy=mbj$aggHeavy)
mmean = monthmean(data=mbj, resp='aggPollute', adjmonth=FALSE)
mmean
plot(mmean)

##Plotting monthly data in circle diagrams
plotCircular(area1 = mmean$mean,
             dp = 1, lines = TRUE,
             labels = month.abb,
             scale = 0.7)

#3. Time Series Plot / monthly data
mbj$yrmon<-round(mbj$year+(1/12)*(mbj$month-1),3)

plot(mbj$yrmon, mbj$aggPollute, type = 'o',
     pch = 19,
     ylab = 'Number of polluted days (PM2.5>=150) per month', xlab = 'Time')

#4. STL Decomposition / weekly data
