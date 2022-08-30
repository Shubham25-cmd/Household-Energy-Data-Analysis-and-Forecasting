library(stringr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(urca)
library(tseries)
library(fpp2)
library(forecast)

#PRE-PROCESSING DATA

#reading the data
data<-read.csv("/Users/shubhammishra/Desktop/VII Semester/ECO619- Time Series Analysis and Forecasting/Assignment/Data/archive/D202.csv")

#sub-setting rows
data$COST<-as.numeric(gsub("$", "",as.character(data$COST), fixed = T))

data$year <- gsub("/", "",as.character(data$DATE), fixed = T)
data$year <- str_sub(data$year,-4)
data<-subset(data, year == '2017')

#sub-setting column
data$time<-as.character(paste(data$DATE, data$START.TIME))
data <- data[!names(data) %in% c("year", "TYPE", "DATE", "START.TIME", "END.TIME", "UNITS", "NOTES")]
colnames(data)<-c("use","cost","time")
data <- data %>% select("time",everything())

#data reduction row-wise - 1 minuite data 
Nth.ddatate<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
i <- 4
dummy<-data
while(i >= 2){
  dummy<-Nth.ddatate(dummy, i)
  i <- i - 1
}
i <- 4
data<-data %>%
  group_by(time = gl(ceiling(nrow(data)/i), i, nrow(data))) %>%
  summarise_each(funs(sum))
data$time<-dummy$time

#keeping only the imp object in the environment
rm(list = setdiff(ls(), c("data")))

#summary
summary(data)
str(data)


#.....................................................................................


#WORKING DATA FOR ANALYSIS
df<-data


#time-series plot for the data
ggplot()+
  geom_line(data=df, aes(x = time, y = use, group=1)) + ylab('Usage')+xlab('Time index') + theme_clean()


#apply time series function, with monthly(8760/12) freq
count_ma<-ts(df$use,frequency = 8760/12)
#periodic decomposition
decomp<-stl(count_ma,"periodic")
#seasonally adjust the decomposition
deseasonal_cnt<-seasadj(decomp)  #Returns seasonally adjusted data constructed by removing the seasonal component
#plotting 
plot(decomp) #seasonality visible


#testing for stationary
plot(df$use, type='l')   #visual check for stationary variance
adf.test(count_ma, alternative = 'stationary')   #adf test shows stationary
#auto correlations 
Acf(count_ma, main='', lag.max = 30)  #corr btw the time-series and its lag
Pacf(count_ma, main='', lag.max = 30)  #corr btw the time-series and its lag that explained by previous lag


#taking into seasonal difference, and rechecking
#re-testing for stationary
ss <- 1  #difference counter
count_d1 <- diff(deseasonal_cnt, differences = ss)
plot(count_d1)  #visual check
adf.test(count_d1, alternative = "stationary")  #adf test
#re-testing auto correlations
Acf(count_d1, main='', lag.max = 30)  #corr btw the time-series and its lag
Pacf(count_d1, main='', lag.max = 30)  #corr btw the time-series and its lag that explained by previous lag


#.....................................................................................


#MODEL FITTING
#fitting ARIMA model
fit1 <- auto.arima(deseasonal_cnt, seasonal = FALSE)  #auto mode gives the most like version for arima
fit1   #gives ARIMA(0,1,1)
tsdisplay(residuals(fit1), lag.max = 30, main = 'Model Residuals from ARIMA(0,1,1)')  #checking the residuals

#trying other fittings
fit2 <- arima(deseasonal_cnt, order=c(2,0,24)) # from previous plot - experimental values
fit2  #is the AIC lower?
tsdisplay(residuals(fit2), lag.max = 30, main = 'Model Residuals from ARIMA(2,0,24)')


#FORECASTING
#predicting new future values after 8760 obs
autoplot(forecast(fit1)) 

#predict from data point 8000 to 8760 over the test data
hold<-window(ts(deseasonal_cnt),start(8000))
mod_fit<- arima(ts(deseasonal_cnt[-c(8000:8760)]), order = c(2,0,24))
ff<-forecast(mod_fit, h=759)
plot(ff, main="") #plotting the forecast
lines(ts(deseasonal_cnt)) #actual value over the forecast values


#bringing back seasonality and again forecasting
#again fitting arima model with seasonality
sfit1 <- auto.arima(deseasonal_cnt, seasonal = TRUE)
tsdisplay(residuals(sfit1), lag.max = 30, main = 'Model Residuals from auto ARIMA with seasonality')
#forecasting with seasonality 
sff<-forecast(sfit1, h=760)
plot(sff, main="Auto Arima with seasonality")
lines(ts(deseasonal_cnt))

#need to find the good models


#.....................................................................................


#ACCURACY TESTING with selected models


#compiling different fit models
#fitting auto arima with seasonality
sfit1 <- auto.arima(deseasonal_cnt, seasonal = TRUE)
tsdisplay(residuals(sfit1), lag.max = 30, main = 'Model Residuals from auto ARIMA with seasonality')
#fitting auto arima withot seasonality
fit1 <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit1), lag.max = 30, main = 'Model Residuals from ARIMA(0,1,1)')
#fitting arima(2,0,24)
fit2 <- arima(deseasonal_cnt, order=c(2,0,24))
tsdisplay(residuals(fit2), lag.max = 30, main = 'Model Residuals from ARIMA(2,0,24)')
#fitting arima(1,1,1)
fit3 <- arima(deseasonal_cnt, order=c(1,1,1))
tsdisplay(residuals(fit2), lag.max = 30, main = 'Model Residuals from ARIMA(1,1,1)')
#fitting ets model
fit10 <- ets(df$use)  #ETS model of the original data
plot(fit10)
tsdisplay(residuals(fit10), lag.max = 30, main = 'ETS model')


#visual accuracy, forecasting different fit models together
par(mfrow=c(2,3))
#auto arima with seasonality
sff1<-forecast(sfit1, h=760)
plot(sff1)
lines(ts(deseasonal_cnt))
#auto arima without seasonality
ff1<-forecast(fit1, h=760)
plot(ff1)
lines(ts(deseasonal_cnt))
#ARIMA(2,0,24)
ff2<-forecast(fit2, h=760)
plot(ff2)
lines(ts(deseasonal_cnt))
#ARIMA(1,1,1)
ff3<-forecast(fit3, h=760)
plot(ff3)
lines(ts(deseasonal_cnt))
#ETS model
ff10<-forecast(fit10, h=760)
plot(ff10)
lines(ts(deseasonal_cnt))


#accuracy checking --> lower the mape value better the accuracy
accuracy(sfit1)  #auto arima with seasonality
accuracy(fit1)   #auto arima without seasonality
accuracy(fit2)   #arima(2,0,24)
accuracy(fit3)   #arima(1,1,1)
accuracy(fit10)  #ets model


