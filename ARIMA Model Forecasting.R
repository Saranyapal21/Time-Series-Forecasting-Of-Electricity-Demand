#Exploratory Analysis Of Time Series Data

#We will use West Bengal State Electricity Consumption Data of 2019

#data = read.csv("C:/Users/Hrik/Desktop/long_data_.csv")
data = long_data_
names(data)
nrow(data)
head(data)
tail(data)


data$Date = as.Date(data$Dates, format = "%d-%m-%y")
inds = seq(as.Date("02-01-2019"), as.Date("31-12-2019"), by = "day")
series = ts(data$Usage,
            start = c(2019, as.numeric(format(inds[1], "%j"))),
            frequency = 365)


plot.ts(series, ylab="Electricity Consumption", main="Time Series")

#Trend Calcualtion
linear.model = lm(data$Usage ~ data$index)   #Intercept=137.32   Slope=0.0088
coef(linear.model)                           #Almost No trend for a particular year
abline(linear.model)                         #As expected....

fix(data)


#Seasonality Estimation





