################Group 4: Kai Sun, Xinzhu Zhang, Wenyi Li, Binghong Xie
library(dplyr)
library(ggplot2)
library(tidyverse)
library(forecast)
library(rio)
library(lubridate)
library(Holidays)
# library(prophet)
# library(tibbletime)
################################################################################
############################ Data Preparation ##################################
################################################################################

# Load bike trip data

url = c("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/11/MetroBikeShare_2016_Q3_trips-2.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/09/metro-bike-share-trips-2016-q4.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/11/la_metro_gbfs_trips_Q1_2017-2.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2017/07/la_metro_gbfs_trips_Q2_2017.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2016/10/metro-bike-share-trips-2017-q3.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/02/metro-bike-share-trips-2017-q4-v2.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/04/metro-bike-share-trips-2018-q1.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/08/metro-bike-share-trips-2018-q2.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/10/metro-bike-share-trips-2018-q3.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/01/metro-bike-share-trips-2018-q4.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/04/metro-bike-share-trips-2019-q1.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/08/metro-bike-share-trips-2019-q2.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/10/metro-bike-share-trips-2019-q3-1.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/01/metro-bike-share-trips-2019-q4.csv.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/04/metro-bike-share-trips-2020-q1.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/03/metro-trips-2020-q2-v2.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/10/metro-trips-2020-q3.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/01/metro-trips-2020-q4.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/04/metro-trips-2021-q1-1.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/07/metro-trips-2021-q2.zip",
        "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/10/metro-trips-2021-q3.zip"
        )

# check each quarter data
tail(import(url[21], which = 2),2)
length(url)

# Look at first quarter
trip = import(url[1])
str(trip)

# change data type
trip$start_time = mdy_hm(trip$start_time)
trip$end_time = mdy_hm(trip$end_time)
str(trip)

# save 1st quarter's column names in a character vector
FirstQuarterNames = names(trip)
FirstQuarterNames 

dim(trip)

# Import all quarters
for (i in 2:(length(url))){
  print(i)
  # import data and specify which file to import
  if (i %in% c(20,21)){
    quarter_i = import(url[i], which = 2)
  }else{
    quarter_i = import(url[i], which = 1)
  }
  # Keep the first 14 columns and rename columnes
  quarter_i = quarter_i[,1:14]
  colnames(quarter_i) = FirstQuarterNames
  # change data type
  if (i %in% c(2,4,6,7,8,9,10,11,12,14)){
    quarter_i$start_time = ymd_hms(quarter_i$start_time)
    quarter_i$end_time = ymd_hms(quarter_i$end_time)
  }else{
    quarter_i$start_time = mdy_hm(quarter_i$start_time)
    quarter_i$end_time = mdy_hm(quarter_i$end_time)    
  }
  trip = rbind(trip,quarter_i)
  print(dim(trip))
}

head(trip)
tail(trip)
str(trip)

# Load bike station data
station_url="https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/10/metro-trips-2021-q3.zip"
station=import(station_url,which=1)
colnames(station) = c("StationId", "StationName", "GoLiveDate", "Region", "Status")
head(station)

# Join the two table
df = merge(x = trip, y = station, by.x = "start_station_id", by.y = "StationId",
           all.x = TRUE)
head(df)
tail(df)
dim(df)

df$Hour = format(as.POSIXct(df$start_time), format = "%H")
df$Date = as.Date(df$start_time,format = "%Y/%m/%d %I:%M:%S")
head(df)
dim(df)

# Calculate how many trips for each hour each date
by_hour = df %>% group_by(Date,Hour) %>% summarise(NumTrips = n())
by_hour %>%  summarise(TotalTrips = sum(NumTrips)) %>% 
  summarise(TotalTrips = sum(TotalTrips))
head(by_hour)
tail(by_hour)
dim(by_hour)
# Create an empty table with all days and all hours
date1 = "2016/7/7"
date2 = "2021/9/30"
dif <- as.numeric(abs(as.Date(date1) - as.Date(date2)))
dif
dates <- c()
for (i in 0:dif){
  date = (as.Date(date1) + i)
  dates = append(dates,rep(date,24))
}
# 1912 days between two dates
data = data.frame(Date = dates, Hour = rep(1:24,1912))
head(data)
tail(data)

# Join the empty table with by_hour table
data = merge(x = data, y = by_hour, by = c("Date","Hour"),all.x = TRUE)
data$NumTrips = data$NumTrips %>% replace_na(0)
data$Time = 1:(dim(data)[1])
# add holiday dummy
holidays=holidays(years=2016:2021,type='USFed')
holidays
data$Holiday=ifelse(data$Date%in% holidays,1,0)
head(data)
tail(data)

# Shorten the time period to better observe trend
data %>% slice((24*31):(24*45)) %>% ggplot(aes(x=Time,y=NumTrips)) + geom_line() + geom_point()+theme_bw()
data %>% slice(120:168) %>% ggplot(aes(x=Time,y=NumTrips)) + geom_line() + geom_point()+theme_bw()

# We observed daily seasonality and weekly seasonality

data %>% slice(26000:27000) %>% ggplot(aes(x=Time,y=NumTrips)) + geom_line() + geom_point()+theme_bw()
tail(data)
dim(data)

data %>% ggplot(aes(x=Time,y=NumTrips)) + geom_line()+geom_point()+theme_bw()
# Data now is ready for prediction
################################################################################
# set up the model comparison table
RMSE_table = data.frame(matrix(ncol = 5, nrow = 0))
colnames(RMSE_table) = c("Model","Senario 1",
                         "Senario 2", 
                         "Senario 3",
                         "Average RMSE")
RMSE_table

###############################################################################
#############################  decomposition ####################################
###############################################################################

# hourly data has daily, weekly and annual seasonality
y = msts(data$NumTrips, seasonal.periods = c(24,24*7,24*365.25))
y

#M = auto.arima(y, seasonal = F, xreg = fourier(y, K=c(i,j,k)), lambda = "auto")

# seasonality = data.frame(matrix(ncol = 4, nrow = 0))
# colnames(seasonality) = c("i","j","k","AICC")
# n=1
# for (i in 11:11){
#   for (j in 13:13){
#     for (k in 1:10){
#       AICC = auto.arima(y,seasonal = F, xreg = fourier(y, K= c(i,j,k)),
#                         lambda = "auto")$aicc
#       seasonality[n,] = c(i,j,k, AICC)
#       print(c(i,j,k))
#       n = n+1
#     }
#   }
# }
# plot(seasonality$AICC)
             
# We choose i=11, j=13 and k = 1 to balance the accuracy and the 
# complexity of the model

############################### Senario 1 #####################################
# Three Scenarios: 
# Scenario 1: 
# Train: 2016Q3 - 2019Q3
# Test: 2019Q4

#Scenario 2:
# Train: 2016Q3 - 2020Q3
# Test: 2020Q4

# Scenario 3:
# Train: 2016Q3 - 2021Q2
# Test: 2021Q3
{
data1=data[data$Date >= as.Date("2016-9-1",format=c("%Y-%m-%d")) &
             data$Date <= as.Date("2019-12-31",format=c("%Y-%m-%d")),]

tail(data1)
y1 = msts(data1$NumTrips, seasonal.periods = c(24,24*7,24*365.25))
y1.train=window(y1,end=c(1,sum(data1$Date <as.Date("2019-9-1"))))
length(y.train)
y1.test=window(y1,start=c(1,sum(data1$Date <as.Date("2019-9-1"))+1))
length(y1.test)

# y1 = msts(data1$NumTrips, seasonal.periods = c(24,24*7,24*365.25),end = 2.25)
# length(y1)
# y1.train = window(y1,end = 2)
# y1.test = window(y1, start = 2)
# # y1_train = msts(data1_train$NumTrips, seasonal.periods = c(24,24*7,24*365.25),end=1)
# # y1_test = msts(data1_test$NumTrips, seasonal.periods = c(24,24*7,24*365.25),start = 1)
# 
# length(y1)/2
# length(y1.train)
# tail(y1.train)
# length(y1.test)
# tail(y1.test)

# Multiple seasonal decomposition
M1 = mstl(y1.train)
M1

MF = forecast(M1,method='arima',h=length(y1.test),level = 95)
MF

autoplot(y1.train,size=1) + autolayer(MF$fitted, series="Fitted values",size=1.25) + 
  autolayer(MF$mean, series=" Forecast on the \n testing set",size=1.25) + 
  theme_bw()


accuracy(MF, y1.test)
RMSE_table[1,"Model"] = "decomposition"
RMSE_table[1,"Senario 1"] = accuracy(MF, y1.test)[2,"RMSE"]
RMSE_table
}
############################### Senario 2 #####################################
{
data2=data[data$Date >= as.Date("2016-9-1",format=c("%Y-%m-%d")) &
             data$Date <= as.Date("2020-12-31",format=c("%Y-%m-%d")),]

tail(data2)
y2 = msts(data2$NumTrips, seasonal.periods = c(24,24*7,24*365.25))
y2.train=window(y2,end=c(1,sum(data2$Date <as.Date("2020-9-1"))))
length(y2.train)
y2.test=window(y2,start=c(1,sum(data2$Date <as.Date("2020-9-1"))+1))
length(y2.test)

M1_2 = mstl(y2.train)
M1_2

MF1_2 = forecast(M1_2,method='arima',h=length(y2.test),level = 95)
MF1_2

autoplot(y2,size=1.25) + autolayer(MF1_2$fitted, series="Fitted values",size=1.25) + 
  autolayer(MF1_2$mean, series=" Forecast on the \n testing set",size=1.25) + 
  theme_bw()


accuracy(MF1_2, y2.test)
RMSE_table[1,"Senario 2"] = accuracy(MF1_2, y2.test)[2,"RMSE"]
RMSE_table
}
############################### Senario 3 #####################################
{
data3=data[data$Date >= as.Date("2016-9-1",format=c("%Y-%m-%d")) &
             data$Date <= as.Date("2021-9-30",format=c("%Y-%m-%d")),]


y3 = msts(data3$NumTrips, seasonal.periods = c(24,24*7,24*365.25))
y3.train=window(y3,end=c(1,sum(data3$Date <as.Date("2021-6-1"))))
length(y3.train)
y3.test=window(y3,start=c(1,sum(data3$Date <as.Date("2021-6-1"))+1))
length(y3.test)


M1_3 = mstl(y3.train)
M1_3

MF1_3 = forecast(M1_3,method='arima',h=length(y3.test),level = 95)
MF1_3

autoplot(y3,size=1.25) + autolayer(MF1_3$fitted, series="Fitted values",size=1.25) + 
  autolayer(MF1_3$mean, series=" Forecast on the \n testing set",size=1.25) + 
  theme_bw()


accuracy(MF1_3, y3.test)
RMSE_table[1,"Senario 3"] = accuracy(MF1_3, y3.test)[2,"RMSE"]
RMSE_table[1,"Average RMSE"] = (RMSE_table[1,"Senario 1"] +
                                RMSE_table[1,"Senario 2"] +
                                  RMSE_table[1,"Senario 3"])/3
RMSE_table
 }

###############################################################################
#############################  Naive model ####################################
###############################################################################
############################### Senario 1 #####################################
{
 M2 = snaive (y1.train, h=length(y1.test), level = 95)
  ?snaive
  M2
  
  autoplot(y1,size=1.25) + autolayer(M2$fitted, series="Fitted values",size=1.25) + 
    autolayer(M2$mean, series=" Forecast on the \n testing set",size=1.25) + 
    theme_bw()
  tsdisplay(M2$residuals)
  
  accuracy(M2, y1.test)
  RMSE_table[2,"Model"] = "snaive"
  RMSE_table[2,"Senario 1"] = accuracy(M2, y1.test)[2,"RMSE"]
  RMSE_table
}
############################### Senario 2 #####################################
{
  M2_2 = snaive (y2.train, h=length(y2.test), level = 95)
  M2_2
  
  autoplot(y2,size=1.25) + autolayer(M2_2$fitted, series="Fitted values",size=1.25) + 
    autolayer(M2_2$mean, series=" Forecast on the \n testing set",size=1.25) + 
    theme_bw()
  tsdisplay(M2_2$residuals)
  
  accuracy(M2_2, y2.test)
  RMSE_table[2,"Senario 2"] = accuracy(M2_2, y2.test)[2,"RMSE"]
  RMSE_table
}
############################### Senario 3 #####################################
{M2_3 = snaive (y3.train, h=length(y3.test), level = 95)
M2_3

autoplot(y3,size=1.25) + autolayer(M2_3$fitted, series="Fitted values",size=1.25) + 
  autolayer(M2_3$mean, series=" Forecast on the \n testing set",size=1.25) + 
  theme_bw()
tsdisplay(M2_3$residuals)

accuracy(M2_3, y3.test)
RMSE_table[2,"Senario 3"] = accuracy(M2_3, y3.test)[2,"RMSE"]

RMSE_table[2,"Average RMSE"] = (RMSE_table[2,"Senario 1"] +
                                  RMSE_table[2,"Senario 2"] +
                                  RMSE_table[2,"Senario 3"])/3
RMSE_table
}

###############################################################################
###############################  Ets model ####################################
###############################################################################
############################### Senario 1 #####################################
{
  M3 = stlf(y1.train, s.window = "periodic",method = 'ets',
            allow.multiplicative.trend = TRUE, h=length(y2.test), lambda = "auto")
  M3
  
  MF = forecast(M3,h=length(y1.test))
  MF
  
  autoplot(y1,size=1.25) + autolayer(MF$fitted, series="Fitted values",size=1.25) + 
    autolayer(MF$mean, series=" Forecast on the \n testing set",size=1.25) + 
    theme_bw()
  tsdisplay(M3$residuals)
  
  accuracy(MF, y1.test)
  RMSE_table[3,"Model"] = "ets"
  RMSE_table[3,"Senario 1"] = accuracy(MF, y1.test)[2,"RMSE"]
  RMSE_table
}
############################### Senario 2 #####################################
{  
  M3_2 = stlf(y2.train, s.window = "periodic",method = 'ets',
              allow.multiplicative.trend = TRUE, h=length(y2.test), lambda = "auto")
  M3_2

autoplot(y2,size=1.25) + autolayer(M3_2$fitted, series="Fitted values",size=1.25) + 
  autolayer(M3_2$mean, series=" Forecast on the \n testing set",size=1.25) + 
  theme_bw()
tsdisplay(M3_2$residuals)

accuracy(M3_2, y2.test)
RMSE_table[3,"Senario 2"] = accuracy(M3_2, y2.test)[2,"RMSE"]
RMSE_table
}
############################### Senario 3 #####################################
{  
  M3_3 = stlf(y3.train, s.window = "periodic",method = 'ets',
              allow.multiplicative.trend = TRUE, h=length(y3.test), 
              lambda = "auto")
  M3_3
  
  autoplot(y3,size=1.25) + autolayer(M3_3$fitted, series="Fitted values",size=1.25) + 
    autolayer(M3_3$mean, series=" Forecast on the \n testing set",size=1.25) + 
    theme_bw()
  tsdisplay(M3_3$residuals)
  
  accuracy(M3_3, y3.test)
  RMSE_table[3,"Senario 3"] = accuracy(M3_3, y3.test)[2,"RMSE"]
  
  RMSE_table[3,"Average RMSE"] = (RMSE_table[3,"Senario 1"] +
                                    RMSE_table[3,"Senario 2"] +
                                    RMSE_table[3,"Senario 3"])/3
  RMSE_table
}

###############################################################################
###########################  Nueral Network ###################################
###############################################################################
############################### Senario 1 #####################################
{
  M4 = nnetar(y1.train, MaxNWts=1083)
  M4
  
  MF = forecast(M4,h=length(y1.test))
  MF
  
  autoplot(y1,size=1.25) + autolayer(MF$fitted, series="Fitted values",size=1.25) + 
    autolayer(MF$mean, series=" Forecast on the \n testing set",size=1.25) + 
    theme_bw()
  tsdisplay(M4$residuals)
  
  accuracy(MF, y1.test)
  RMSE_table[4,"Model"] = "nn"
  RMSE_table[4,"Senario 1"] = accuracy(MF, y1.test)[2,"RMSE"]
  RMSE_table
}
############################### Senario 2 #####################################
{
  M4_2 = nnetar(y2.train, MaxNWts=1153)
  M4_2
  
  MF = forecast(M4_2,h=length(y2.test))
  MF
  
  autoplot(y2,size=1.25) + autolayer(MF$fitted, series="Fitted values",size=1.25) + 
    autolayer(MF$mean, series=" Forecast on the \n testing set") + 
    theme_bw()
  tsdisplay(M4_2$residuals)
  
  accuracy(MF, y2.test)
  RMSE_table[4,"Senario 2"] = accuracy(MF, y2.test)[2,"RMSE"]
  RMSE_table
  }
############################### Senario 3 #####################################
{ 
  M4_3 = nnetar(y3.train, MaxNWts = 100000)
  ?nnetar
  M4_3

  MF = forecast(M4_3,h=length(y3.test))
  MF

  autoplot(y3,size=1.25) + autolayer(MF$fitted, series="Fitted values",size=1.25) + 
    autolayer(MF$mean, series=" Forecast on the \n testing set",size=1.25) + 
    theme_bw()
  tsdisplay(M4_3$residuals)

  accuracy(MF, y3.test)
  RMSE_table[4,"Senario 3"] = accuracy(MF, y3.test)[2,"RMSE"]
  RMSE_table[4,"Average RMSE"] = (RMSE_table[4,"Senario 1"] +
                                    RMSE_table[4,"Senario 2"] +
                                    RMSE_table[4,"Senario 3"])/3
  RMSE_table
}

###############################################################################
#################################  LM  ########################################
###############################################################################
data=data[data$Date >= as.Date("2016-9-1",format=c("%Y-%m-%d")),]
data$DayOfWeek = wday(data$Date,label=T,abbr=F)
data$Hour = factor(data$Hour)
data$Actual = data$NumTrips

# construct day of the year variable
a=c()
for (year in 2016:2021){
  if (year == 2016){
    for (i in 245:366) {
      b = rep(i,24)
      a = c(a,b)
    }
  }
  else if (year == 2020){
    for (i in 1:366) {
      if (i==366){
        b = rep(365,24)
      }
      else{
        b = rep(i,24)
      }
      a = c(a,b)
    }
  }
  else if (year == 2021){
    for (i in 1:273) {
      b = rep(i,24)
      a = c(a,b)
    }
  }
  else {
    for (i in 1:365) {
      b = rep(i,24)
      a = c(a,b)
    }
  }
}
data$Day = factor(a)
data$Week = factor(ceiling(as.integer(data$Day)/7))
# feb 2020 till now
data$covid=1
data$covid[1:1247*24]=0
head(data)
str(data)
tail(data)
dim(data)
length(unique(data$Date))
# add weather data
setwd("D:/USC/3rd semester/DSO522 time series/final project")
weather=read.csv('LA.csv',header=TRUE)
head(weather)
str(weather_new)
dim(data)
weather[is.na(weather)]=0
weather=weather %>% rename(Date=DATE)
weather$Date=as.Date(weather$Date,format = "%Y/%m/%d")
weather$Date=format(as.Date(weather$Date),"%Y-%m-%d")
weather=weather[weather$Date >= as.Date("2016-9-1",format=c("%Y-%m-%d")),]

weather=aggregate(weather,by=list(weather$Date),FUN=mean)
weather=weather[,-which(names(weather) %in% c('Date'))]


weather_new = weather[rep(seq_len(nrow(weather)), each = 24), ]
weather_new$Date=ymd(weather_new$Date)
data=cbind(data,weather_new)


# Senarios:

# Senario 1 
df1 = data %>% slice(1:(24*(365+365+365+122)))
tail(df1)
dim(df1)
df1$NumTrips[(24*(365+365+365)+1):(dim(df1)[1])] = NA
df1$TrainTest = "Test"
df1$TrainTest[1:(24*(365+365+365))] = "Train"
head(df1)
tail(df1,122*24)

# Senario 2
df2 = data %>% slice(1:(24*(365+365+365+366+122)))
dim(df2)
df2$NumTrips[(24*(365+365+365+366)+1):(dim(df2)[1])] = NA
df2$TrainTest = "Test"
df2$TrainTest[1:(24*(365+365+365+366))] = "Train"
head(df2)
tail(df2,122*24)

# Senario 3
df3 = data %>% slice(1:(24*(365+365+365+366+365+30)))
tail(df3)
df3$NumTrips[(24*(365+365+365+366+274)+1):(dim(df3)[1])] = NA
df3$TrainTest = "Test"
df3$TrainTest[1:(24*(365+365+365+366+274))] = "Train"
tail(df1)
df1[24*(365+365+365)+1,"M5_1"]=predict(M5, newdata = df1[24*(365+365+365)+1,])
df1[24*(365+365+365)+1,"M5_1"]
predict(M5, newdata = df1[24*(365+365+365)+1,])
?predict
############################### Senario 1 #####################################
{
  M5 = lm(NumTrips ~ DayOfWeek + Hour + Day + Week+ Week*Hour+PRCP+TAVG+AWND, data = df1)
  M5
  
  # store fitted values in data
  df1$M5_1 = NA
  df1[!is.na(df1$NumTrips),"M5_1"] = M5$fitted.values
  
  # Predict 2019 Q4
  for (i in (24*(365+365+365)+1):(dim(df1)[1])) {
    df1[i,"M5_1"] = predict(M5, newdata = df1[i,])
  }
  
  df1 %>% ggplot(aes(x=Time,y=Actual)) + geom_line(size=1.25) +
    geom_line(aes(x=Time,y=M5_1,color=TrainTest),size=1.25) +
    theme_bw()
  
  RMSE1_1 = df1 %>% filter(Date > "2019-09-01") %>% 
    {sqrt(mean((.$Actual - .$M5_1)^2,na.rm = TRUE))}
  RMSE_table[5,"Model"] = "LM_1"
  RMSE_table[5,"Senario 1"] = RMSE1_1
  RMSE_table
}
RMSE1_1
############################### Senario 2 #####################################
{
  M5_2 = lm(NumTrips ~ DayOfWeek + Hour + Day + Week+ Week*Hour+PRCP+TAVG+AWND, 
          data = df2)
  M5_2
  
  # store fitted values in data
  df2$M5_2 = NA
  df2[!is.na(df2$NumTrips),"M5_2"] = M5_2$fitted.values
  
  # Predict 2008 Q4 - 2009 Q3 data
  for (i in (24*(365+365+365+366)+1):(dim(df2)[1])) {
    df2[i,"M5_2"] = predict(M5_2, newdata = df2[i,])
  }
  
  df2 %>% ggplot(aes(x=Time,y=Actual)) + geom_line(size=1.25) +
    geom_line(aes(x=Time,y=M5_2,color=TrainTest),size=1.25) +
    theme_bw()
  
  RMSE1_2 = df2 %>% filter(Date > "2020-09-30") %>% 
    {sqrt(mean((.$Actual - .$M5_2)^2,na.rm = TRUE))}
  RMSE_table[5,"Senario 2"] = RMSE1_2
  RMSE_table
}

############################### Senario 3 #####################################
{ 
  M5_3 = lm(NumTrips ~ DayOfWeek + Hour + Day + Week*Hour+PRCP+TAVG+AWND, 
            data = df3)
  M5_3
  
  # store fitted values in data
  df3$M5_3 = NA
  df3[!is.na(df3$NumTrips),"M5_3"] = M5_3$fitted.values
  
  # Predict 2008 Q4 - 2009 Q3 data
  for (i in (24*(365+365+365+366+274)+1):(dim(df3)[1])) {
    df3[i,"M5_3"] = predict(M5_3, newdata = df3[i,])
  }

  df3 %>% ggplot(aes(x=Time,y=Actual)) + geom_line(size=1.25) +
    geom_line(aes(x=Time,y=M5_3,color=TrainTest),size=1.25) +
    theme_bw()
  
  RMSE1_3 = df3 %>% filter(Date > "2021-06-01") %>% 
    {sqrt(mean((.$Actual - .$M5_3)^2,na.rm = TRUE))}
  RMSE_table[5,"Senario 3"] = RMSE1_3
  RMSE_table[5,"Average RMSE"] = (RMSE_table[5,"Senario 1"] +
                                    RMSE_table[5,"Senario 2"] +
                                    RMSE_table[5,"Senario 3"])/3
  RMSE_table
}

############################## Champion #########################################
# According to the RMSE Table and the visualizations
# Decomposition is the champion model
# Use decompostition to predict 2021 Q4 Hourly demand:
z = msts(data$NumTrips, seasonal.periods = c(24,24*7,24*365.25))

M = mstl(z)
FM = forecast(M, method="arima", h=24*93)
autoplot(FM) + autolayer(FM$fitted) + theme_bw()
autoplot(M)
sum(FM$mean)
sum(FM$mean)/93
sum(FM$mean)/93/24
















