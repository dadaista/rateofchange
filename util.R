source("loadUtils.R")


options(stringsAsFactors = FALSE)

cache=list(init=TRUE)



load_<-function(asset,to.date=0){
  
  df <- loadAdjCloseEOD(asset,to.date=to.date)
  df
  
}




#' Rate of Change
#'
#' @param price a vetor of prices
#' @param lag  
#'
#' @return the rate of change vector
#' @export
#'
#' @examples
roc<-function(price,lag=10){
  
  N<-length(price)
  d<-price[(lag+1):N] - price[1:(N-lag)] 
  
  r<-d/price[1:(N-lag)]
  return(r)
}

rets <-function(ticker){
  x <- load(ticker)$Adj.Close
  getReturns(x)
} 







