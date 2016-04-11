setwd("~/Personal/TS")
install.packages("forecast")
library(tseries)
library(forecast)

data(AirPassengers)

#This tells you that the data series is in a time series format
class(AirPassengers)

#This is the start of the time series
start(AirPassengers)

#This is the end of the time series
end(AirPassengers)

frequency(AirPassengers)

summary(AirPassengers)

#1. visualise the series
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

#This will print the cycle across years.
cycle(AirPassengers)

#This will aggregate the cycles and display a year on year trend
plot(aggregate(AirPassengers,FUN=mean))

#Box plot across months will give us a sense on seasonal effect
boxplot(AirPassengers~cycle(AirPassengers))

#2. stationarize the series
#Augmented Dickey-Fuller Test
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

#3. Plot ACF/PACF and find optimal parameters
#ACF Plots
acf(log(AirPassengers))
#after regressing
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))
# (0,1,1) as (p,d,q) comes out to be the combination with least AIC and BIC
#three integer components (p, d, q) are the AR order, the degree of differencing, and the MA order.

#acf() FUNCTION produces the correlogram plot by default.   
#Correlogram for Air Passanger data 
AirPassengers.decom<-decompose(AirPassengers, "mult")
plot(ts(AirPassengers.decom$random[7:138]))
acf(AirPassengers.decom$random[7:138]) #Correlogram for random component of AirPassenger data period 1949-1960
#correlogram suggests a damped 'cosine' shape, 
#which is characteristic of an autoregressive model of order 2 or that the seasonal adjustment has not been entirely effective. 

#HOW TO CHECK FOR EFFECTIVENESS OF SEASONAL ADJUSTMENTS
#1. Look at the standard deviation of the original series 
sd(AirPassengers[7:138]) #=109
#2. Look at the standard deviation of the series after subtracting the trend 
sd(AirPassengers[7:138]-AirPassengers.decom$trend[7:138]) # only 41 
#3. Look at the standard deviation of the series after seasonal adjustment  
sd(AirPassengers.decom$random[7:138]) #only a very small 0.0334
#=>therefore one can say that the seasonal adjustment was very EFFECTIVE 

#4. Build Arima model
(findbest <- auto.arima(AirPassengers))
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

#5. Make predictions
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))
plot(forecast(findbest,h=20))

#6. Compute prediction intervals of 95% confidence level for each prediction
# calculate upper (U) and lower (L) prediction intervals
U <- pred$pred + 2*pred$se # se: standard error (quantile is 2 as mean=0)
L <- pred$pred - 2*pred$se
# plot observed and predicted values
ts.plot(AirPassengers, pred$pred, U, L, col=c(1, 2, 4, 4), lty=c(1, 1, 2, 2))
library(graphics)
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% prediction interval)"), 
       col=c(1, 2, 4),lty=c(1, 1, 2))
#residual analysis
res <- residuals(fit)  # get residuals from fit
# check acf and pacf of residuals
acf(res)
pacf(res)
qqnorm(residuals(fit))
qqline(residuals(fit))

#Test for stationarity
adf.test(fit$residuals, alternative ="stationary")


#holtzwinters
AP.hw<-HoltWinters(AirPassengers, seasonal="mult")
plot(AP.hw)
AP.hw$alpha; AP.hw$beta; AP.hw$gamma
AP.predict<-predict(AP.hw, n.ahead=4*12) # since montly data is predicted for 4 years
ts.plot(AirPassengers, AP.predict, lty=1:2)

