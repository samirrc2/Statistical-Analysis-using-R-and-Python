library(quantmod);
library(Quandl)
library(tseries);
library(ggplot2)
library(timeSeries);
library(forecast);
library(xts);

data <- Quandl("NSE/OIL",type='xts')
data<- na.omit(data)
tail(data)
N <- nrow(data) #get number of rows 
N
price<- data$Last[N:1] 
price 

# Compute the log returns for the Bitcoin
log_return = diff(log(price),lag=1)
sum(is.na(log_return))#check number of NA in df
log_return = log_return[!is.na(log_return)]
# Plot log returns 
plot(log_return,type='l', main='log returns plot')
summary(log_return)

# Conduct ADF test on log returns series
print(adf.test(log_return)) #The p-value of 0.01 from the ADF test tells us that the series is stationary

# Split the dataset in two parts - training and testing
breakpoint = floor(nrow(log_return)*(2.9/3)) # to round the number to nearest low number 
breakpoint

# Apply the ACF and PACF functions
par(mfrow = c(1,1)) # matrix of plots 
acf.log_return = acf(log_return[c(1:breakpoint),], main='ACF Plot', lag.max=100)
pacf.log_return = pacf(log_return[c(1:breakpoint),], main='PACF Plot', lag.max=100)

# check p,d,q values with auto arima
fit <- auto.arima(log_return)
fit

# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2014-11-25","%Y-%m-%d")) # as.Date for converting strings to dates

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())
b=breakpoint

# For loop for forecasting
for (b in breakpoint:(nrow(log_return)-1)) {
  
  #test train split
  log_return_train = log_return[1:b, ] # [ rows, columns] 
  log_return_test = log_return[(b+1):nrow(log_return), ]
  
  # Summary of the ARIMA model using the determined (p,d,q) parameters
  fit = arima(log_return_train, order = c(0, 0, 0),include.mean=TRUE)
  #summary(fit)
 
   # plotting a acf plot of the residuals
  acf(fit$residuals,main="Residuals plot")
  
  # Forecasting the log returns
  arima.forecast = forecast(fit, h = 1,level=95) #h no periods for forecasting, l is conf internval
  #summary(arima.forecast)
  
  # plotting the forecast
  par(mfrow=c(1,1))
  plot(arima.forecast, main = "ARIMA Forecast")
  
  # Creating a series of forecasted returns for the forecasted period
  forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  # Creating a series of actual returns for the forecasted period
  Actual_return = log_return[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return) # remove actual return from memory
  
  #print(price[(b+1),])
  #print(price[(b+2),])
  
}

# Adjust the length of the Actual return series
Actual_series = Actual_series[-1]

# Create a time series object of the forecasted series
forecasted_series = xts(forecasted_series,index(Actual_series))

# Create a plot of the two return series - Actual versus Forecasted
plot(Actual_series,type='l',main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series,lwd=1.5,col='red')
legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))

# Create a table for the accuracy of the forecast
comparsion = merge(Actual_series,forecasted_series)
comparsion$Accuracy = sign(comparsion$Actual_series)==sign(comparsion$Forecasted)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)

