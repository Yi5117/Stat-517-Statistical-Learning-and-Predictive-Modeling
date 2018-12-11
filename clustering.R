library(TSclust)

calculate.ts.dist <- function(store.matrix){
  #Calculates the dissimilarity matrix via the Autocorrelation method (ACF)
  #Input: A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  #Output:A matrix of dissimilarity computations of length 990
  
  tsdist <-t(select(store.matrix,-1)) #remove date column
  tsdist<-scale(tsdist) #standardising data points
  tsdist <- diss(tsdist, "ACF", p=0.05)
  return(tsdist)
}

optimal.pdq <- function(p,d,q,cluster){
  #Returns the optimal p, d and q coefficients for ARIMA modeling by minimizing AIC
  #Input: 3 terms representing the maximum coefficient for the function to loop through, and a time series
  #Output: A list of the optimal coeffients for p, d and q
  azfinal.aic <- Inf
  azfinal.order <- c(0,0,0)
  for (a in 1:q) for (b in 0:d) for (c in 1:p) {
    azcurrent.aic <- AIC(Arima(cluster, order=c(a, b, c),seasonal = list(order = c(0,1,0), period = 52), include.mean = FALSE))
    if (azcurrent.aic < azfinal.aic) {
      azfinal.aic <- azcurrent.aic
      azfinal.order <- c(a, b, c)
      azfinal.arima <- Arima(cluster, order=c(a, b, c),seasonal = list(order = c(0,1,0), period = 52), include.mean = FALSE)
    }
  }
  azfinal.order
}
