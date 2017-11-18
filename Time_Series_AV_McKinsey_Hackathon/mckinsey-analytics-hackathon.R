require(forecast)
require(ggplot2)
require(dplyr)
require(dummies)
require(randomForest)
train_raw = read.csv('train_aWnotuB.csv')
test_raw = read.csv('test_BdBKkAj.csv')
glimpse(train_raw)

Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}

Missing_data_Check(train_raw)
Missing_data_Check(test_raw)

Prediction_hrs = 24*(as.Date("2017-10-31 23:00:00")-as.Date("2017-07-01 00:00:00"))

train_data = train_raw
train_data$Day =as.factor(weekdays(as.Date(train_data$DateTime)))
train_data$Month =as.factor(months(as.Date(train_data$DateTime)))
train_data$Hour =as.factor(format(as.POSIXct(train_data$DateTime, 
                                             format="%Y-%m-%d %H:%M:%S"), 
                                  format="%H"))
train_data$Year =as.factor(format(as.POSIXct(train_data$DateTime, 
                                             format="%Y-%m-%d %H:%M:%S"), 
                                  format="%Y"))

train_data_J1 = subset(train_data, train_data$Junction == '1')
train_data_J2 = subset(train_data, train_data$Junction == '2')
train_data_J3 = subset(train_data, train_data$Junction == '3')
train_data_J4 = subset(train_data, train_data$Junction == '4')

firstHr = 24*(as.Date("2017-06-30 23:00:00")-as.Date("2015-11-01 00:00:00"))
#ts_train_J1 = ts(train_data_J1$Vehicles, start(2015,firstHr), frequency = 24*365)

time_vector= c('00','01','02','03','04','05',
               '06','07','08','09','10','11',
               '12','13','14','15','16','17',
               '18','19','20','21','22','23')
forecast = list()
i = 1
for(time in time_vector){
  temp = ts(train_data_J1[train_data_J1$Hour== time,'Vehicles'],
            start(2015,firstHr), frequency = 365)
  print(temp)
  ARIMA_model = auto.arima(temp[1:608])
  ARIMA_model
  y_predict = forecast::forecast(ARIMA_model,h=123)
  forecast[[i]] = round(y_predict$mean)
  i = i +1
}

day_names = paste0(rep('day',123),1:123)
for(j in 1:123){
  temp = data.frame(numeric())
  for(i in 1:24){
    temp =rbind(temp,forecast[[i]][j]) 
  }
  assign(day_names[j], temp)
}

forecast_df_J1 = cbind(day1,day2,day3,day4,day5,day6,day7,day8,day9,day10,
                       day11,day12,day13,day14,day15,day16,day17,day18,day19,
                       day20,day21,day22,day23,day24,day25,day26,day27,day28,
                       day29,day30,day31,day32,day33,day34,day35,day36,day37,
                       day38,day39,day40,day41,day42,day43,day44,day45,day46,
                       day47,day48,day49,day50,day51,day52,day53,day54,day55,
                       day56,day57,day58,day59,day60,day61,day62,day63,day64,
                       day65,day66,day67,day68,day69,day70,day71,day72,day73,
                       day74,day75,day76,day77,day78,day79,day80,day81,day82,
                       day83,day84,day85,day86,day87,day88,day89,day90,day91,
                       day92,day93,day94,day95,day96,day97,day98,day99,day100,
                       day101,day102,day103,day104,day105,day106,day107,day108,
                       day109,day110,day111,day112,day113,day114,day115,day116,
                       day117,day118,day119,day120,day121,day122,day123)
colnames(forecast_df_J1) = c(1:123)
#View(forecast_df_J1)

rm(list = ls(pattern = "day")) 
rm(list=c('forecast'))

#J2
forecast = list()
i = 1
for(time in time_vector){
  temp = ts(train_data_J2[train_data_J2$Hour== time,'Vehicles'],
            start(2015,firstHr), frequency = 365)
  print(temp)
  ARIMA_model = auto.arima(temp[1:608])
  ARIMA_model
  y_predict = forecast::forecast(ARIMA_model,h=123)
  forecast[[i]] = round(y_predict$mean)
  i = i +1
}

day_names = paste0(rep('day',123),1:123)
for(j in 1:123){
  temp = data.frame(numeric())
  for(i in 1:24){
    temp =rbind(temp,forecast[[i]][j]) 
  }
  assign(day_names[j], temp)
}

forecast_df_J2 = cbind(day1,day2,day3,day4,day5,day6,day7,day8,day9,day10,
                       day11,day12,day13,day14,day15,day16,day17,day18,day19,
                       day20,day21,day22,day23,day24,day25,day26,day27,day28,
                       day29,day30,day31,day32,day33,day34,day35,day36,day37,
                       day38,day39,day40,day41,day42,day43,day44,day45,day46,
                       day47,day48,day49,day50,day51,day52,day53,day54,day55,
                       day56,day57,day58,day59,day60,day61,day62,day63,day64,
                       day65,day66,day67,day68,day69,day70,day71,day72,day73,
                       day74,day75,day76,day77,day78,day79,day80,day81,day82,
                       day83,day84,day85,day86,day87,day88,day89,day90,day91,
                       day92,day93,day94,day95,day96,day97,day98,day99,day100,
                       day101,day102,day103,day104,day105,day106,day107,day108,
                       day109,day110,day111,day112,day113,day114,day115,day116,
                       day117,day118,day119,day120,day121,day122,day123)
colnames(forecast_df_J2) = c(1:123)
#View(forecast_df_J2)

rm(list = ls(pattern = "day")) 
rm(list=c('forecast'))

#J3
forecast = list()
i = 1
for(time in time_vector){
  temp = ts(train_data_J3[train_data_J3$Hour== time,'Vehicles'],
            start(2015,firstHr), frequency = 365)
  print(temp)
  ARIMA_model = auto.arima(temp[1:608])
  ARIMA_model
  y_predict = forecast::forecast(ARIMA_model,h=123)
  forecast[[i]] = round(y_predict$mean)
  i = i +1
}

day_names = paste0(rep('day',123),1:123)
for(j in 1:123){
  temp = data.frame(numeric())
  for(i in 1:24){
    temp =rbind(temp,forecast[[i]][j]) 
  }
  assign(day_names[j], temp)
}

forecast_df_J3 = cbind(day1,day2,day3,day4,day5,day6,day7,day8,day9,day10,
                       day11,day12,day13,day14,day15,day16,day17,day18,day19,
                       day20,day21,day22,day23,day24,day25,day26,day27,day28,
                       day29,day30,day31,day32,day33,day34,day35,day36,day37,
                       day38,day39,day40,day41,day42,day43,day44,day45,day46,
                       day47,day48,day49,day50,day51,day52,day53,day54,day55,
                       day56,day57,day58,day59,day60,day61,day62,day63,day64,
                       day65,day66,day67,day68,day69,day70,day71,day72,day73,
                       day74,day75,day76,day77,day78,day79,day80,day81,day82,
                       day83,day84,day85,day86,day87,day88,day89,day90,day91,
                       day92,day93,day94,day95,day96,day97,day98,day99,day100,
                       day101,day102,day103,day104,day105,day106,day107,day108,
                       day109,day110,day111,day112,day113,day114,day115,day116,
                       day117,day118,day119,day120,day121,day122,day123)
colnames(forecast_df_J3) = c(1:123)
#View(forecast_df_J3)

rm(list = ls(pattern = "day")) 
rm(list=c('forecast'))

#J4
firstHr_J4 = 24*(as.Date("2017-06-30 23:00:00")-as.Date("2017-01-01 00:00:00"))
forecast = list()
i = 1
for(time in time_vector){
  temp = ts(train_data_J4[train_data_J4$Hour== time,'Vehicles'],
            start(2017,firstHr_J4), frequency = 365)
  print(temp)
  ARIMA_model = auto.arima(temp[1:181])
  ARIMA_model
  y_predict = forecast::forecast(ARIMA_model,h=123)
  forecast[[i]] = round(y_predict$mean)
  i = i +1
}

day_names = paste0(rep('day',123),1:123)
for(j in 1:123){
  temp = data.frame(numeric())
  for(i in 1:24){
    temp =rbind(temp,forecast[[i]][j]) 
  }
  assign(day_names[j], temp)
}

forecast_df_J4 = cbind(day1,day2,day3,day4,day5,day6,day7,day8,day9,day10,
                       day11,day12,day13,day14,day15,day16,day17,day18,day19,
                       day20,day21,day22,day23,day24,day25,day26,day27,day28,
                       day29,day30,day31,day32,day33,day34,day35,day36,day37,
                       day38,day39,day40,day41,day42,day43,day44,day45,day46,
                       day47,day48,day49,day50,day51,day52,day53,day54,day55,
                       day56,day57,day58,day59,day60,day61,day62,day63,day64,
                       day65,day66,day67,day68,day69,day70,day71,day72,day73,
                       day74,day75,day76,day77,day78,day79,day80,day81,day82,
                       day83,day84,day85,day86,day87,day88,day89,day90,day91,
                       day92,day93,day94,day95,day96,day97,day98,day99,day100,
                       day101,day102,day103,day104,day105,day106,day107,day108,
                       day109,day110,day111,day112,day113,day114,day115,day116,
                       day117,day118,day119,day120,day121,day122,day123)
colnames(forecast_df_J4) = c(1:123)
#View(forecast_df_J4)

rm(list = ls(pattern = "day")) 
rm(forecast)

test_dat = test_raw
test_dat$Day =as.factor(weekdays(as.Date(test_dat$DateTime)))
test_dat$Month =as.factor(months(as.Date(test_dat$DateTime)))
test_dat$Hour =as.factor(format(as.POSIXct(test_dat$DateTime, 
                                             format="%Y-%m-%d %H:%M:%S"), 
                                  format="%H"))
test_dat$Year =as.factor(format(as.POSIXct(test_dat$DateTime, 
                                             format="%Y-%m-%d %H:%M:%S"), 
                                  format="%Y"))
test_dat$Date =as.factor(format(as.POSIXct(test_dat$DateTime, 
                                           format="%Y-%m-%d %H:%M:%S"), 
                                format="%Y-%m-%d"))


test_dat_J1 = subset(test_dat, test_dat$Junction == '1')
test_dat_J2 = subset(test_dat, test_dat$Junction == '2')
test_dat_J3 = subset(test_dat, test_dat$Junction == '3')
test_dat_J4 = subset(test_dat, test_dat$Junction == '4')

require(reshape2)


temp = data.frame(
  D1 = c(1,2,3,4),
  D2 = c(5,6,7,8),
  D3 = c(9,10,11,12),
  D4 = c(13,14,15,16),
  Hr = c(0,1,2,3)
)
temp_melt = melt(temp, id = c('Hr'))

forecast_df_J1$Hr = c(1:24)
colnames(forecast_df_J1) = c(paste0('D',c(1:123)),'Hr')
forecast_df_J2$Hr = c(1:24)
colnames(forecast_df_J2) = c(paste0('D',c(1:123)),'Hr')
forecast_df_J3$Hr = c(1:24)
colnames(forecast_df_J3) = c(paste0('D',c(1:123)),'Hr')
forecast_df_J4$Hr = c(1:24)
colnames(forecast_df_J4) = c(paste0('D',c(1:123)),'Hr')

forecast_df_J1_test = melt(forecast_df_J1, id=c('Hr'))
forecast_df_J2_test = melt(forecast_df_J2, id=c('Hr'))
forecast_df_J3_test = melt(forecast_df_J3, id=c('Hr'))
forecast_df_J4_test = melt(forecast_df_J4, id=c('Hr'))

F_dataes = unique(test_dat_J1$Date)
date_code = data.frame(Date = F_dataes,
                       D_Code = c(paste0('D',c(1:123))))

test_dat_J1$hr_code = as.numeric(test_dat_J1$Hour)
test_dat_J2$hr_code = as.numeric(test_dat_J2$Hour)
test_dat_J3$hr_code = as.numeric(test_dat_J3$Hour)
test_dat_J4$hr_code = as.numeric(test_dat_J4$Hour)

test_dat_J1 = test_dat_J1 %>%
                left_join(date_code, by ='Date')
test_dat_J2 = test_dat_J2 %>%
                left_join(date_code, by ='Date')
test_dat_J3 = test_dat_J3 %>%
                left_join(date_code, by ='Date')
test_dat_J4 = test_dat_J4 %>%
                left_join(date_code, by ='Date')
 

test_dat_J1 = test_dat_J1 %>%
                left_join(forecast_df_J1_test, by = c('hr_code' = 'Hr',
                                                      'D_Code' = 'variable'))
test_dat_J2 = test_dat_J2 %>%
                left_join(forecast_df_J2_test, by = c('hr_code' = 'Hr',
                                                      'D_Code' = 'variable'))
test_dat_J3 = test_dat_J3 %>%
                left_join(forecast_df_J3_test, by = c('hr_code' = 'Hr',
                                                      'D_Code' = 'variable'))
test_dat_J4 = test_dat_J4 %>%
                left_join(forecast_df_J4_test, by = c('hr_code' = 'Hr',
                                                      'D_Code' = 'variable'))
test_dat_J1$Vehicles =  test_dat_J1$value
test_dat_J2$Vehicles =  test_dat_J2$value
test_dat_J3$Vehicles =  test_dat_J3$value
test_dat_J4$Vehicles =  test_dat_J4$value

final_solution = rbind(test_dat_J1,test_dat_J2,test_dat_J3,test_dat_J4)

Solution = final_solution[,c(3,12)]
write.csv(Solution, 'Solution.csv', row.names = F)

