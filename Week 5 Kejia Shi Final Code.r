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
library(mosaic)


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
###We only use two classification standards, both related with the effect on people not just sensitive groups
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
  
  dbj<-rbind(dbj2012,dbj2013,dbj2014,dbj2015)
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
mmeanh = monthmean(data=mbj, resp='aggHeavy', adjmonth=FALSE)
mmeanh
plot(mmeanh)

##Circle Plot of Average Number of Polluted Days Each Month
plotCircular(area1 = mmean$mean,
             dp = 1, lines = TRUE,
             labels = month.abb,
             scale = 0.7,
             pieces.col="yellow",
             main="Monthly Average Polluted Days\n(Index>=150, Bejing)")

plotCircular(area1 = mmeanh$mean,
             dp = 1, lines = TRUE,
             labels = month.abb,
             scale = 0.35,
             pieces.col="red",
             main="Monthly Average Heavily Polluted Days\n(Index>=300, Bejing)")

#2. STL Decomposition

#Daily Value Time Series Decompose
mbj_v<-summarise(group_by(dbj,Year,Month),mean=mean(mean_Value_Day))
d=mbj_v$mean
t=ts(d,frequency = 12,start=c(2012,1))
g<- decompose(t)
plot(g)
#title("Daily Index Decomposition\n(Bejing)")

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

#3. Time Series Plot / monthly data

mbj$yrmon<-round(mbj$year+(1/12)*(mbj$month-1),3)
plot(mbj$yrmon, mbj$aggPollute, type = 'o',
     pch = 19,
     ylab = 'Index', xlab = 'Time', main='Number of polluted days (PM2.5>=150) per month (Beijing)')

mbj_v$yrmon<-round(mbj_v$Year+(1/12)*(mbj_v$Month-1),3)
plot(mbj_v$yrmon, mbj_v$mean, type = 'o',
     pch = 19,
     ylab = 'Index', xlab = 'Time', main='Average daily index per month (Beijing)')


#4. Replicate the peocedure for weekly data

#5. Replicate for Shanghai and Chongqin

##Shanghai

reRead2 <- 1
if(reRead2==1){
  
  sh2015<-dataclean("sh2015.csv")
  sh2014<-dataclean("sh2014.csv")
  sh2013<-dataclean("sh2013.csv")
  sh2012<-dataclean("sh2012.csv")
  sh<-rbind(sh2012,sh2013,sh2014,sh2015)
  save(sh,file="sh.rdata")
  
  dsh2015<-dailyclean("sh2015.csv")
  dsh2015<-pollute(dsh2015)
  dsh2015$Year<-as.numeric(2015)
  
  dsh2014<-dailyclean("sh2014.csv")
  dsh2014<-pollute(dsh2014)
  dsh2014$Year<-as.numeric(2014)
  
  dsh2013<-dailyclean("sh2013.csv")
  dsh2013<-pollute(dsh2013)
  dsh2013$Year<-as.numeric(2013)
  
  dsh2012<-dailyclean("sh2012.csv")
  dsh2012<-pollute(dsh2012)
  dsh2012$Year<-as.numeric(2012)
  
  dsh<-rbind(dsh2012,dsh2013,dsh2014,dsh2015)
  
  save(dsh,file="dsh.rdata")
  
}else{
  
  load("sh.rdata")
  load("dsh.rdata")
  
} 

msh<-monthpol(dsh)
msh<-data.frame(year=msh$Year, month=msh$Month, aggPollute=msh$aggPollute, aggHeavy=msh$aggHeavy)
mmean_sh = monthmean(data=msh, resp='aggPollute', adjmonth=FALSE)
mmean_sh
plot(mmean_sh)
mmeanh_sh = monthmean(data=msh, resp='aggHeavy', adjmonth=FALSE)
mmeanh_sh
plot(mmeanh_sh)

plotCircular(area1 = mmean_sh$mean,
             dp = 1, lines = TRUE,
             labels = month.abb,
             scale = 0.7,
             pieces.col="yellow",
             main="Monthly Average Polluted Days\n(Index>=150, Shanghai)")

plotCircular(area1 = mmeanh_sh$mean,
             dp = 1, lines = TRUE,
             labels = month.abb,
             scale = 0.35,
             pieces.col="red",
             main="Monthly Average Heavily Polluted Days\n(Index>=300, Shanghai)")

msh_v<-summarise(group_by(dsh,Year,Month),mean=mean(mean_Value_Day))
d_sh=msh_v$mean
t_sh=ts(d_sh,frequency = 12,start=c(2012,1))
g_sh<- decompose(t_sh)
plot(g_sh)
#'Daily Index Decomposition\n(Shanghai)')

d2_sh=msh$aggPollute
t2_sh=ts(d2_sh,frequency = 12,start=c(2012,1))
g2_sh<- decompose(t2_sh)
plot(g2_sh)

d3_sh=msh$aggHeavy
t3_sh=ts(d3_sh,frequency = 12,start=c(2012,1))
g3_sh<- decompose(t3_sh)
plot(g3_sh)

msh$yrmon<-round(msh$year+(1/12)*(msh$month-1),3)
plot(msh$yrmon, msh$aggPollute, type = 'o',
     pch = 19,
     ylab = 'Index', xlab = 'Time',main='Number of polluted days (PM2.5>=150) per month (Shanghai)')

msh_v$yrmon<-round(msh_v$Year+(1/12)*(msh_v$Month-1),3)
plot(msh_v$yrmon, msh_v$mean, type = 'o',
     pch = 19,
     ylab = 'Index', xlab = 'Time',main='Average daily index per month (Shanghai)')

##Chengdu

reRead3 <- 1
if(reRead3==1){
  
  cd2015<-dataclean("cd2015.csv")
  cd2014<-dataclean("cd2014.csv")
  cd2013<-dataclean("cd2013.csv")
  cd2012<-dataclean("cd2012.csv")
  cd<-rbind(cd2012,cd2013,cd2014,cd2015)
  save(cd,file="cd.rdata")
  
  dcd2015<-dailyclean("cd2015.csv")
  dcd2015<-pollute(dcd2015)
  dcd2015$Year<-as.numeric(2015)
  
  dcd2014<-dailyclean("cd2014.csv")
  dcd2014<-pollute(dcd2014)
  dcd2014$Year<-as.numeric(2014)
  
  dcd2013<-dailyclean("cd2013.csv")
  dcd2013<-pollute(dcd2013)
  dcd2013$Year<-as.numeric(2013)
  
  dcd2012<-dailyclean("cd2012.csv")
  dcd2012<-pollute(dcd2012)
  dcd2012$Year<-as.numeric(2012)
  
  dcd<-rbind(dcd2012,dcd2013,dcd2014,dcd2015)
  
  save(dcd,file="dcd.rdata")
  
}else{
  
  load("cd.rdata")
  load("dcd.rdata")
  
} 

mcd<-monthpol(dcd)
mcd<-data.frame(year=mcd$Year, month=mcd$Month, aggPollute=mcd$aggPollute, aggHeavy=mcd$aggHeavy)
mmean_cd = monthmean(data=mcd, resp='aggPollute', adjmonth=FALSE)
mmean_cd
plot(mmean_cd)
mmeanh_cd = monthmean(data=mcd, resp='aggHeavy', adjmonth=FALSE)
mmeanh_cd
plot(mmeanh_cd)

plotCircular(area1 = mmean_cd$mean,
             dp = 1, lines = TRUE,
             labels = month.abb,
             scale = 0.7,
             pieces.col="yellow",
             main="Monthly Average Polluted Days\n(Index>=150, Chengdu)")

plotCircular(area1 = mmeanh_cd$mean,
             dp = 1, lines = TRUE,
             labels = month.abb,
             scale = 0.35,
             pieces.col="red",
             main="Monthly Average Heavily Polluted Days\n(Index>=300, Chengdu)")

mcd_v<-summarise(group_by(dcd,Year,Month),mean=mean(mean_Value_Day))
d_cd=mcd_v$mean
t_cd=ts(d_cd,frequency = 12,start=c(2012,1))
g_cd<- decompose(t_cd)
plot(g_cd)
#'Daily Index Decomposition\n(Chengdu)')

d2_cd=mcd$aggPollute
t2_cd=ts(d2_cd,frequency = 12,start=c(2012,1))
g2_cd<- decompose(t2_cd)
plot(g2_cd)

d3_cd=mcd$aggHeavy
t3_cd=ts(d3_sh,frequency = 12,start=c(2012,1))
g3_cd<- decompose(t3_cd)
plot(g3_cd)

mcd$yrmon<-round(mcd$year+(1/12)*(mcd$month-1),3)
plot(mcd$yrmon, mcd$aggPollute, type = 'o',
     pch = 19,
     ylab = 'Index', xlab = 'Time',main='Number of polluted days (PM2.5>=150) per month (Chengdu)')

mcd_v$yrmon<-round(mcd_v$Year+(1/12)*(mcd_v$Month-1),3)
plot(mcd_v$yrmon, mcd_v$mean, type = 'o',
     pch = 19,
     ylab = 'Index', xlab = 'Time',main='Average daily index per month (Chengdu)')


#6. Putting plots together

##Fix Chengdu data
vector1 <- c(2012, 1, NA, NA, 2012.000)
vector2 <- c(2012, 2, NA, NA, 2012.083)
vector3 <- c(2012, 3, NA, NA, 2012.167)
vector4 <- c(2012, 4, NA, NA, 2012.250)
mcd<-rbind(vector1, vector2, vector3, vector4, mcd)
rm(vector1, vector2, vector3, vector4)

vector5 <- c(2012, 1, NA, 2012.000)
vector6 <- c(2012, 2, NA, 2012.083)
vector7 <- c(2012, 3, NA, 2012.167)
vector8 <- c(2012, 4, NA, 2012.250)
mcd_v<-rbind(vector5, vector6, vector7, vector8, mcd_v)
rm(vector5, vector6, vector7, vector8)

##Combine the three cities' monthly data
monthlyData <- data.frame(year=mbj$year, month=mbj$month, yrmon=mbj$yrmon, 
                          bjMean=mbj_v$mean, shMean=msh_v$mean, cdMean=mcd_v$mean, 
                          bjPollu=mbj$aggPollute, shPollu=msh$aggPollute, cdPollu=mcd$aggPollute,
                          bjHeavy=mbj$aggHeavy, shHeavy=msh$aggHeavy, cdHeavy=mcd$aggHeavy)

p <- ggplot() + theme(plot.title = element_text(size=15, face="bold"), legend.title=element_blank()) +
  geom_line(data=monthlyData, aes(x=yrmon, y=bjMean, color="Beijing")) +
  geom_line(data=monthlyData, aes(x=yrmon, y=shMean, color="Shanghai"))  +
  geom_line(data=monthlyData, aes(x=yrmon, y=cdMean, color="Chengdu"))  +
  xlab("Time") +
  ylab("Index") +
  ggtitle("2012-2015 Average Daily Index Per Month\n")
p

#7. Distribution of the average air quality by season - density plot
##xlab=airquality ylab=probability
##Decompose into seasons
###3/21,6/22,9/23,12/22

season<-function(df){
  df<-mutate(df, Season = derivedFactor(
                  "Winter" = ((Month==12&Day>=22)|Month==1|Month==2|(Month==3&Day<21)),
                  "Spring" = ((Month==3&Day>=21)|Month==4|Month==5|(Month==6&Day<22)),
                  "Summer" = ((Month==6&Day>=22)|Month==7|Month==8|(Month==9&Day<23)),
                  "Fall" = ((Month==9&Day>=23)|Month==10|Month==11|(Month==12&Day<22)),
                  .method = "first",
                  .default = NA))
}

dbj<-season(dbj)
dsh<-season(dsh)
dcd<-season(dcd)

ggplot(dbj, aes(x = mean_Value_Day, fill = Season, colour = Season)) +
  geom_density(alpha=0.1) + theme(plot.title = element_text(size=15, face="bold")) +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50)) +
  ggtitle ("PM2.5 Index Distribution by Season\n(Beijing)\n") +
  xlab("Average Daily Index") +  ylab ("Probability")

ggplot(dsh, aes(x = mean_Value_Day, fill = Season, colour = Season)) +
  geom_density(alpha=0.1) + theme(plot.title = element_text(size=15, face="bold")) +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50)) +
  ggtitle ("PM2.5 Index Distribution by Season\n(Shanghai)\n") +
  xlab("Average Daily Index") +  ylab ("Probability")

ggplot(dcd, aes(x = mean_Value_Day, fill = Season, colour = Season)) +
  geom_density(alpha=0.1) + theme(plot.title = element_text(size=15, face="bold")) +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50)) +
  ggtitle ("PM2.5 Index Distribution by Season\n(Chengdu)\n") +
  xlab("Average Daily Index") +  ylab ("Probability")

#8. Yearly Index Distribution
##add natural year
###3/21,6/22,9/23,12/22

naturalYear<-function(df){
  df<-mutate(df, Nat_Year = derivedFactor(
    "2012-2013" = ((Year==2012&((Month==6&Day>=22)|Month>=7))|(Year==2013&((Month==6&Day<22)|Month<=5))),
    "2013-2014" = ((Year==2013&((Month==6&Day>=22)|Month>=7))|(Year==2014&((Month==6&Day<22)|Month<=5))),
    "2014-2015" = ((Year==2014&((Month==6&Day>=22)|Month>=7))|(Year==2015&((Month==6&Day<22)|Month<=5))),
    .method = "first",
    .default = NA))
}

dbj<-naturalYear(dbj)
dsh<-naturalYear(dsh)
dcd<-naturalYear(dcd)

ggplot(dbj, aes(x = mean_Value_Day, fill = Nat_Year, colour = Nat_Year)) +
  geom_density(alpha=0.1) + theme(plot.title = element_text(size=15, face="bold")) +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50)) +
  ggtitle ("PM2.5 Index Distribution by Year (Beijing)\n") +
  xlab("Average Daily Index") + ylab ("Probability")

ggplot(dsh, aes(x = mean_Value_Day, fill = Nat_Year, colour = Nat_Year)) +
  geom_density(alpha=0.1) + theme(plot.title = element_text(size=15, face="bold")) +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50)) +
  ggtitle ("PM2.5 Index Distribution by Year (Shanghai)\n") +
  xlab("Average Daily Index") + ylab ("Probability")

ggplot(dcd, aes(x = mean_Value_Day, fill = Nat_Year, colour = Nat_Year)) +
  geom_density(alpha=0.1) + theme(plot.title = element_text(size=15, face="bold")) +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50)) +
  ggtitle ("PM2.5 Index Distribution by Year (Chengdu)\n") +
  xlab("Average Daily Index") + ylab ("Probability")

#9. By city
dbj$city<-"Beijing"
dsh$city<-"Shanghai"
dcd$city<-"Chengdu"
dailyCity<-rbind(dbj,dsh,dcd)

ggplot(dailyCity, aes(x=subset(mean_Value_Day, Season=="Fall"||Season=="Winter"), fill = city, colour = city)) +
  geom_density(alpha=0.1) + theme(plot.title = element_text(size=15, face="bold")) +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50)) +
  ggtitle ("PM2.5 Index Distribution in Fall and Winter\n") +
  xlab("Average Daily Index") + ylab ("Probability")

ggplot(dailyCity, aes(x=subset(mean_Value_Day, Season=="Fall"||Season=="Winter"), fill = city, colour = city)) +
  geom_density(alpha=0.1) + theme(plot.title = element_text(size=15, face="bold")) +
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50)) +
  ggtitle ("2012-2015 PM2.5 Index Distribution in Fall and Winter\n") +
  xlab("Average Daily Index") + ylab ("Probability")

