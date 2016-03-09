
options(stringsAsFactors = FALSE)

cache=list(init=TRUE)


#' build a url for yahoo api
#'
#' @param asset, a ticker 
#'
#' @return a url
#' @export
#'
#' @examples
buildYahooUrl<-function(asset){
  
  aDate=strsplit(as.character(Sys.Date()),"-")[[1]]
  day=as.numeric(aDate[3])
  month=as.numeric(aDate[2])-1
  year=as.numeric(aDate[1])
  #u<- 'http://real-chart.finance.yahoo.com/table.csv?s=%5ENDX&a=09&b=11&c=2010&d=09&e=11&f=2015&g=d&ignore=.csv'
  baseu<-'http://real-chart.finance.yahoo.com/table.csv?s='

  
  u=sprintf("%s%s&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d&g=d&ignore=.csv",baseu,asset,month,day,year-1,month,day,year)
  print(u)
  return (u)
  
}

#' Title
#'
#' @param asset a symbol
#' @param n if non zero loads the most recent n days
#'
#' @return a dataframe
#' @export
#'
#' @examples
load<-function(asset,n=0,usecache=FALSE){
  
  if(usecache==TRUE){
    print("cache lookup...")
    if(asset %in% names(cache)){
      print("loading from cache...")
      return(cache[[asset]])
    }
  }
  #special case
  if(asset=="BTCUSD"){
    return(loadBTC())
  }
  
  df<-read.csv(buildYahooUrl(asset))
  #df$Adj.Close=(1:262)*10

  if (n>0)
    df <- df[1:n,]
  
  
    
  df <- df[nrow(df):1,]
  cache[[asset]] <<- df
  df

  
}

#' load Bitcoin prices from blockchain.info
#'
#' @return
#' @export
#'
#' @examples
loadBTC<-function(){
  
  print("loadinf from blockchain.info...")
  df<-read.csv('https://blockchain.info/charts/market-price?showDataPoints=false&timespan=180days&show_header=true&daysAverageString=1&scale=0&format=csv&address=')
  df[,1]<-as.character(as.Date(df[,1],format="%d/%m/%Y"))
  return(data.frame(list(Date=df[,1],Adj.Close=df[,2])))
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

getReturns <- function(x){
  len <- length(x)
  y <- x[2:len]
  (y-x[1:(len-1)]) / x[1:(len-1)]
}



#' Merges two securities data frames with different dates to get a single
#' dataframe with same dates and one column for each security 
#'
#' @param df a dataframe with columns Date and Adj.Close
#' @param data another data frame as loaded by load()
#' @param symbol the name of the symbol for data
#'
#' @return  merges dates in order to consider only a subset of rows
#' @export
#'
#' @examples
mergeSecurities <- function(df,data,symbol){
  

  
  dates1 <- df$Date
  dates2 <- data$Date
  dates = intersect(dates1,dates2)#keep only dates in common
  df <-     df[df$Date %in% dates,]
  data <-  data[data$Date %in% dates,]
  df[symbol]<-  data$Adj.Close
  df
}

#' compute returns in a single data frame
#'
#' @param d dataframe from mergeSecurities
#'
#' @return the daily returns for each security
#' @export
#'
#' @examples
computeReturns <- function(d){
  

  n <- nrow(d)
  d <- d[,-1]#remove date column
  (d[2:n, ] - d[1:(n-1),])/d[1:(n-1),]
  
}

beta <- function(eq,bm){
  cov(eq,bm)/var(bm)
}

alpha <- function(fund.return, market.return,beta){
  fund.return - beta*market.return
}

priceAtDate <- function(date,symbol){
  df <- load(symbol,usecache = TRUE)
  if(symbol!="BTCUSD")df <- df[,c(1,7)]
  
  df <- df[df$Date<=date,]
  price <-  df$Adj.Close[nrow(df)]
  price
}


pricesAtDate <- Vectorize(priceAtDate,"symbol")



