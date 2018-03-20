#####  Predicting Stock Price-Performance comparison of ARIMA models with supervised learning algorithms   #####

# Download data from Yahoo finance using quantmod package

library(quantmod)

RBC=getSymbols('RY.TO', from='2011-01-01', to='2017-12-31', auto.assign = FALSE)


# Select closing price as a measure for stock price.  

closingP = RBC[,4]


# checking data for any missing values. 

sum(is.na(closingP))

closingP=na.omit(closingP)

# here, we can see that we have only 1 missing value and hence we can simply delete it. otherwise, we have to impute all the missing values with median.


# For time series analysis we will transform dataframe into time series dataset 

tsclosingP=ts(closingP, start = c(2011, 1), end=c(2017,1), frequency = 12)


# Plotting the data [Years on x-axis and Closing Price on Y-axis]  

plot(tsclosingP, xlab='Years', ylab = 'Closing Price')

# It shows upward trend, seems to be non-stationary


# stationarity check using Augmented Dickey-Fuller (ADF) test

install.packages("tseries")

library(tseries)

adf.test(tsclosingP, alternative = "stationary")

#here With a p-value >0.05, we cannot reject the null hypothesis of non-stationarity in our series.


# make the data stationary by differencing with 1,2,3...etc. until p-value is <0.05

tsclosingP_diff1 = diff(tsclosingP, differences = 1)

plot.ts(tsclosingP_diff1, ylab='Differenced closing prices')

adf.test(tsclosingP_diff1, alternative="stationary")


# Autocorrelations for choosing the order parameters for ARIMA model 

library(forecast)

Acf(tsclosingP, main='ACF for Differenced Series') #identifying q value-indices exceeding 95% significance boundary(blue dotted lines)

Pacf(tsclosingP, main='PACF for Differenced Series') #identifying p value-indices exceeding 95% significance boundary(blue dotted lines)


#### Modeling with ARIMA and Evaluating its prediction accuracy#### 

fitArima= auto.arima(tsclosingP, seasonal=FALSE)

accuracy(fitArima)


# Forecasting with ARIMA

library(forecast)

forecastA=forecast(fitArima)

plot(forecastA)


#### Modeling with Neural Networks  and Evaluating its accuracy #####

fitNn <- nnetar(tsclosingP)

accuracy(fitNn)


# Forecasting with Neural Networks

forecastN=forecast(fitNn)

plot(forecastN)

