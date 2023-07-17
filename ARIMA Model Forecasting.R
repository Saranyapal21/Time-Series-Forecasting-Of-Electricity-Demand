#Exploratory Data Analysis Of Time Seires Data
library(fpp2)


a = c(97.40715802,	100.0037793,	100.4063893,	103.8358548,	114.4815194,	
      124.1584755,	126.3968338,	136.1689781,	136.0898643,	142.1397322,
      152.4150364,	158.7655619,	166.4877721,	184.2481223,	194.5562834,	
      209.0442303,	221.291445,	             241.1021277,	258.0419364,	272.9466472,	
      291.6625282,	305.0131285,	320.9149714,	341.3540525,	358.6295603,	
      359.4810859,	374.9474116,	385.1269668,	391.1196337,	392.5101757,	
      392.3609036,	408.7769006,	428.2132845,	448.9720155,	465.1766773,	
      506.2225086,	538.7865959,	558.4267535,	595.6056558,	637.1258442,	
      692.779357,	718.2959462,	758.1106752,	797.349232)


series = ts(a, start = 1971, end = 2014)
summary(series)
class(series)

plot.ts(series, ylab="Kwh", main="Electricity Consumption Per Capita In India")
#The time series has a strong trend component


#So, we use irst difference technique to remove the trend...

#Trend Estimation And Removal
plot.ts(diff(series),ylab = "Kwh", main = "Differenced Electricty Consumption Data")
summary(diff(series))   

plot.ts(diff(series, lag = 2), main="Data Differenced At Lag 2")
summary(diff(series, lag = 2))
checkresiduals(diff(series, lag=2))
checkresiduals(diff(series, lag=3))

#Seasonality Investigation
##Data is not seasonal; so we can't make any of the plots...

ggseasonplot(differenced.data)   

seasonplot(differenced.data)
ggsubseriesplot(differenced.data)    #When multiple year's data is there, it won't give error...







#Forecasting with ARIMA models

#Before forecasting, we look at the acf and pacf plots:-
par(mfrow=c(2,1))   #Actual Data ACF And PACF
acf(series, main="Actual Data ACF")
pacf(series, main="Actual Data PACF")

acf(diff(series), main="ACF of First Order Differencing")
pacf(diff(series), main="PACF of First Order Differencing ")

acf(diff(series, lag = 2), main="ACF of Second Order Differencing")
pacf(diff(series, lag = 2), main="PACF of Second Order Differencing")

par(mfrow = c(1,1))




#Now we are back with forecasting....
arima.model = auto.arima(series)
arima.model

summary(arima.model)
#It is an ARIMA(0,2,1) model with an AIC of 302.84
#ma1 = -0.536


summary(arima.model)
checkresiduals(arima.model)


arima.forecast = forecast(arima.model)
autoplot(arima.forecast)
summary(arima.model)









library(forecast)
library(fpp2)

#We now use different types of method for forecasting

#Method 1: Seasonal Naive Method
fit1 = snaive(series)    #Residual sd: 20.7878 
summary(fit1)
checkresiduals(fit1)

#Forecasting based on the seasonal naive model
prediction1 = forecast(fit1)
autoplot(prediction1)
summary(prediction1)




#Method2: Exponential Smoothing Method
?ets
?ses

fit2 = ses(series, alpha = 0.999)                #Residuals SD =
fit2$model
autoplot(fit2)    #same as if you use forecast and then autoplot

summary(fit2)   
checkresiduals(fit2)

#Forecasting based on the Eponential Smoothing Method
prediction2 = forecast(fit2)
autoplot(prediction2)
accuracy(prediction2)


fit3 = holt(series)
autoplot(fit3)
summary(fit3)

