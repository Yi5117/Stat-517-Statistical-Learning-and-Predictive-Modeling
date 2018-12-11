library(dplyr)
library(forecast)
library(reshape)
library(TSclust)

tsclust.f <- function (mts){
  #Clusters time series from training data into n number of clusters.
  #This function is called in preparation for ARIMA.
  #The idea is that trend and seasonality is common across many departments as they are selling similar items.
  # Input: 
  # ts - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeks in training data) x (number of stores)
  # n.clusters - the number of clusters
  # Output:
  # train matrix of n clusters
  
  tsdist <- diss(mts, "ACF", p=0.05)
  #perform hierachical clustering to the dist object
  hc <- hclust(tsdist)
  #show the results
  plot(hc)
  
}

calc.mape <- function(timeseries,arima.fit){
  train.m <-ts(timeseries,start=1,end=120)
  test.m <-ts(timeseries,start=121,end=143)
  fc <-forecast(arima.fit,h=23)
  return(accuracy(fc,test.m)[2,5])
}

apply.forecast <- function(train,test,fname, ...){
  ### This function applies the selected forecast function on the training data and outputs a prediction for the dates specific from the test data
  #Input: Train data table, Test data table, function
  #Output: Prediction data table
  
  #Creating a forecast frame
  test.dates <- unique(test$Date)
  num.test.dates <- length(test.dates)
  all.stores <- unique(test$Store)
  num.stores <- length(all.stores)
  test.depts <- unique(test$Dept)
  forecast.frame <- data.frame(Date=rep(test.dates, num.stores),
                               Store=rep(all.stores, each=num.test.dates))
  
  #Creating a train frame
  pred <- test
  pred$Weekly_Sales <- 0
  train.dates <- unique(train$Date)
  num.train.dates <- length(train.dates)
  train.frame <- data.frame(Date=rep(train.dates, num.stores),
                            Store=rep(all.stores, each=num.train.dates))
  
  #Apply function to each department in a loop
  f <- get(fname)
  for(d in test.depts){
    print(paste('dept:', d))
    tr.d <- train.frame
    # This joins in Weekly_Sales but generates NA's. Resolve NA's 
    # in the model because they are resolved differently in different models.
    tr.d <- left_join(tr.d, train[train$Dept==d, c('Store','Date','Weekly_Sales')])
    tr.d <- cast(tr.d, Date ~ Store)    
    fc.d <- forecast.frame
    fc.d$Weekly_Sales <- 0
    fc.d <- cast(fc.d, Date ~ Store)
    result <- f(tr.d, fc.d, ...)
    # This has all Stores/Dates for this department
    result <- melt(result)
    pred.d.idx <- pred$Dept==d
    #These are the Store-Date pairs for the department
    pred.d <- pred[pred.d.idx, c('Store', 'Date')]
    pred.d <- left_join(pred.d, result)
    pred$Weekly_Sales[pred.d.idx] <- pred.d$value
  }
  pred
}

seasonal.naive <- function(train, test){
  # Computes seasonal naive forecasts
  #
  # Input:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  #
  # Output:
  #  the test(forecast) data frame with the forecasts filled in 
  h <- nrow(test)
  tr <- train[nrow(train) - (52:1) + 1,]
  tr[is.na(tr)] <- 0
  test[,2:ncol(test)]  <- tr[1:h,2:ncol(test)]
  test
}

tslm <- function(train, test){
  # Computes a forecast using linear regression and seasonal dummy variables
  #
  # Input:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # test - An all-zeros matrix of dimension:
  #       (number of weeeks in training data) x (number of stores)
  #       The forecasts are written in place of the zeros.
  #
  # Output:
  #  the test(forecast) data frame with the forecasts filled in 
  horizon <- nrow(test)
  train[is.na(train)] <- 0
  for(j in 2:ncol(train)){
    s <- ts(train[, j], frequency=52)
    model <- tslm(s ~ trend + season)
    fc <- forecast(model, h=horizon)
    test[, j] <- as.numeric(fc$mean)
  }
  test
}

arima.m <-function(train,test){
  horizon <- nrow(test)
  tr<-train
  for(j in 2:ncol(tr)){
    # fit arima model
    s <- ts(tr[, j], frequency=52)
    model <- Arima(s,order=c(1,0,1), seasonal = list(order = c(0,1,0), period = 52), include.mean = FALSE)
    fc <- forecast(model, h=horizon)
    test[, j] <- as.numeric(fc$mean)
  }
  test
}
