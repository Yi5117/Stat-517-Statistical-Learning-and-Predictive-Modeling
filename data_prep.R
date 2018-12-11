library(dplyr)
library(reshape2)

read.train <- function(){
  #Load train data
  cls <- c('factor','factor','Date','numeric','logical') #Classes for Store, Dept, Date, Weekly_Sales, isHoliday
  train<- read.csv(file='data/train.csv',colClasses = cls)
  train<-tbl_df(train)
}

read.test <- function(){
  #Load test data
  cls <- c('factor','factor','Date','logical') #Classes for Store, Dept, Date, isHoliday
  test<- read.csv(file='data/test.csv',colClasses = cls)
  test<- tbl_df(test)
}

reshape.by.stores <- function(train){
  #Reshape the train data into a matrix containing the weekly sales for each store
  #This is preparation required for time series clustering
  #Input: Train dataset which contain multiple rows x 4 column variables
  #Output: Matrix of 143 weekly sales observations x 45 stores
  store.matrix <- dcast(train,formula=Date~Store,value.var = "Weekly_Sales",fun.aggregate = sum)
  store.matrix <- tbl_df(store.matrix)
  return(store.matrix)
}
