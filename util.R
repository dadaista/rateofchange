
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

