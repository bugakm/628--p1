##Seasonal Analysis
##Goal: To understand the tendency and urgency of the smog problem by analysing seasonal data
##Data Source: StateAir Data (www.stateair.net)

library(dplyr)
library(timsac)
library(dynlm)
library(season)
library(stats)
library(forecast)
library(bfast)
library(ggplot2)

setwd("/Users/KJ/OneDrive/Projects/628Air/data/")

#0. Cleaning

##Setup clean functions

##Original Data Cleaning from Groupwork
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

##Adding Daily Index: Mean Value
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

##Adding Pollution Dummies

###from the "Air Quality Guide for PM2.5" provided by stateair.net
### 0-50 Good
### 51-100 Moderate
### 101-150 Unhealthy for Sensitive Groups
### 151-200 Unhealthy
### 201-300 Very Unhealthy
### 301-500 Hazardous
### >500 Beyond Index
###We only use two classification standard, both related with the effect on people not just sensitive groups
###We define polluted level as index>=150, as it is (unhealthy) for all groups of people
###We also define heavily polluted as index>=300, as the cautionary statement stated that everyone should (avoid all outdoor exertion)

pollute<-function(data){
  data<-data %>%
    ##Polluted Days: Daily Index >= 150
    mutate(Polluted = as.numeric(mean_Value_Day >= 150)) %>%
    ##Heavily Polluted Days: Daily Index >= 300
    mutate(Hea_Polluted = as.numeric(mean_Value_Day >= 300))
}


##Read data and perform first cleaning

reRead <- 1
###Read data and save it as RData to save time next time:
if(reRead==1){
  
  ####read yearly data and clean
  bj2015<-dataclean("bj2015.csv")
  bj2014<-dataclean("bj2014.csv")
  bj2013<-dataclean("bj2013.csv")
  bj2012<-dataclean("bj2012.csv")
  bj2011<-dataclean("bj2011.csv")
  bj<-rbind(bj2011,bj2012,bj2013,bj2014,bj2015)
  save(bj,file="bj.rdata")
  
  ####create another data with daily index
  
  dbj2015<-dailyclean("bj2015.csv")
  dbj2015<-pollute(dbj2015)
  dbj2015$Year<-as.numeric(2015)
  
  dbj2014<-dailyclean("bj2014.csv")
  dbj2014<-pollute(dbj2014)
  dbj2014$Year<-as.numeric(2014)
  
  dbj2013<-dailyclean("bj2013.csv")
  dbj2013<-pollute(dbj2013)
  dbj2013$Year<-as.numeric(2013)
  
  dbj2012<-dailyclean("bj2012.csv")
  dbj2012<-pollute(dbj2012)
  dbj2012$Year<-as.numeric(2012)
  
  dbj2011<-dailyclean("bj2011.csv")
  dbj2011<-pollute(dbj2011)
  dbj2011$Year<-as.numeric(2011)
  
  dbj<-rbind(dbj2011,dbj2012,dbj2013,dbj2014,dbj2015)
  ####adding dummies
  
  save(dbj,file="dbj.rdata")
  
}else{
  
  load("bj.rdata")
  load("dbj.rdata")
  
} 


#1. Calculating and Plotting Monthly Polluted Days and Monthly Average Index

##Monthly Polluted Days and Monthly Average Index
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

##Circle Plot of Average Number of Polluted Days Each Month
plotCircular(area1 = mmean$mean,
             dp = 1, lines = TRUE,
             labels = month.abb,
             scale = 0.7)


#2. Time Series Plot / monthly data

mbj$yrmon<-round(mbj$year+(1/12)*(mbj$month-1),3)

plot(mbj$yrmon, mbj$aggPollute, type = 'o',
     pch = 19,
     ylab = 'Number of polluted days (PM2.5>=150) per month', xlab = 'Time')

#3. STL Decomposition

#Daily Value Time Series Decompose
mbj_v<-summarise(group_by(dbj,Year,Month),mean=mean(mean_Value_Day))
d=mbj_v$mean
t=ts(d,frequency = 12,start=c(2011,1))
g<- decompose(t)
plot(g)

fit1 <- stl(d, t.window=15, s.window="periodic", robust=TRUE)
plot(fit1)

#Polluted Days Time Series Decompose
d2=mbj$aggPollute
t2=ts(d2,frequency = 12,start=c(2011,1))
g2<- decompose(t2)
plot(g2)

#Heavily Polluted Days Time Series Decompose
d3=mbj$aggHeavy
t3=ts(d3,frequency = 12,start=c(2011,1))
g3<- decompose(t3)
plot(g3)

#4. Replicate the peocedure for weekly data


