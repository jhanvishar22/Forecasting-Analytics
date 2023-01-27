library(fpp2)
library(tidyverse)
library(forecast)
install.packages("TSstudio")
library(tseries)
library(readxl)
library(TSstudio)

sovdata <-read_excel("C:/Users/22jha/OneDrive/Desktop/SouvenirSales.xls")
sovdata

sovdata$Date = as.Date(sovdata$Date)
head(sovdata)
sovdata.ts <- ts(sovdata[,2:2], start= c(1995,1), frequency = 12)
sovdata.ts
plot.ts(sovdata.ts)

class(sovdata.ts)
sum(is.na(sovdata.ts))
frequency(sovdata.ts)
cycle(sovdata.ts)
summary(sovdata.ts)

boxplot(sovdata.ts~cycle(sovdata.ts))

## decomposition of additive time series function

decompose(sovdata.ts)
plot(decompose(sovdata.ts))

## Data Split
sov_split<- ts_split(ts.obj = sovdata.ts, sample.out = 12)
sov_train<-sov_split$train
sov_test<-sov_split$test


## Question B

#Model A- Showcasing linear trend with additive seasonality

datamodA <- tslm(sov_train ~ trend+season, lambda = 1)
datamodA

#Model B-  exponential trend model with multiplicative seasonality 
datamodB <- tslm(sov_train~trend+season, lambda = 0)
datamodB

## Linear Model for solving Validation Set Errors

datamodA.pred <- forecast (datamodA, h=length(sov_test), level = 0)
datamodA.error <- sov_test - datamodA.pred$mean 
datamodA.error

accuracy(df_test, modelA.pred$mean)

## Exponential model for solving Validation Set Errors

datamodB.pred <- forecast(datamodB, h=length(sov_test), level =0)
datamodB.error <- sov_test - datamodB.pred$mean                
datamodB.error

accuracy(sov_test, datamodB.pred$mean)

## QuestionC) Which model is the best model considering RMSE as the metric? Could you have understood this from the line chart?
#Explain. Produce the plot showing the forecasts from both models along with actual data.
#In a separate plot, present the residuals from both models (consider only the validation set residuals).

#Linear model

plot.ts(sovdata.ts/1000,xlab="Time", ylab =  "Sales in Thousands", main = "Linear Sales Trend")
lines (datamodA$fitted.values/1000,lwd=2, col="green")

plot.ts(sovdata.ts/1000,xlab="Time", ylab =  "Sales in Thousands", main = "Exponential Sales Trend")
lines (datamodB$fitted.values/1000,lwd=2, col="green")


## validation set residuals test

datamodA.pred = forecast (datamodA , h= length(sov_test), level = 0)
plot(datamodA.pred)
accuracy(datamodA.pred) 

# calculating RMSE: 5365.199

datamodB.pred = forecast (datamodB , h= length(sov_test), level = 0)
plot(datamodB.pred)
accuracy(datamodB.pred) 

#calculating RMSE: 2865.154

datamodA.error <- sov_test - datamodA.pred$mean
datamodA.error



datamodB.error <- sov_test - datamodB.pred$mean
datamodB.error



par(mfrow=c(1,1))

# Residuals Plot

plot(datamodA.error, main = "Linear and Exponential Model of Residual",
     col = "green")
lines(datamodB.error, col="blue")
legend(2001.0, 55000, legend=c("Linear", "Exponential"), fill = c("green","blue"))

## Question D) Examine the additive model. Which month has the highest average sales during the year. 
## What does the estimated trend coefficient in the model A mean?

summary(datamodA)

# season 12 - December has recorded the highest average sales in the whole year.


## Question E) Examine the multiplicative model. What does the coefficient of October mean? 
## What does the estimated trend coefficient in the model B mean?

summary(datamodB)


## Question F) Use the best model type from part (c) to forecast the sales in January 2002. 
## Think carefully which data to use for model fitting in this case.

datamodF<-tslm(sovdata.ts~trend+season, lambda = 0)
datamodF

datamodF.pred = forecast (datamodF , h= length(sov_test), level = 0)
plot(datamodF.pred)
accuracy(datamodF.pred)

datamodF.pred


## Question G) Plot the ACF and PACF plot until lag 20 of the residuals obtained from training set of the best model chosen.
## Comment on these plots and think what AR(p) model could be a good choice?  

Acf(datamodB$residuals,lag.max = 20, main="ACF of Errors for model B")

Pacf(datamodB$residuals,lag.max = 20,main="PACF of Errors for model B")




## Question H) Fit an AR(p) model as you think appropriate from part (g) to the training set residuals and produce the regression coefficients. 
## Was your intuition at part (g) correct?

arima <- arima(sov_train, order = c(2,0,0))
arima

arimares <- arima(datamodB$residuals, order = c(2,0,0))
arimares
summary(arimares)

Acf(arima$residuals,lag.max = 20, main="ACF of Errors in Arima")

Pacf(arima$residuals,lag.max = 20,main="PACF of Errors in Arima")

# hence prooved - Yes the intuition made was correct.

ts.plot(sov_train)
arima_fit <- sov_train - residuals(arima)
lines(arima_fit, col ="magenta")


## Question I) Now, using the best regression model and AR(p) model, forecast the sales in January 2002. 
## Think carefully which data to use for model fitting in this case.

predict_arima <- predict(arima, n.ahead = 13)

predict_arima


