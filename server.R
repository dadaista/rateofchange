library(shiny)
source('util.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
 
  observe({
    popular <- input$popular
    print ("popular")
    print (popular)
    updateTextInput(session,"stock",value = popular)
  })
  
  getStockName<-reactive({
    print ("get name ...")
      
    stock<-input$stock
    
    
    name = stock
    
    print (name)
    return (name)
  })

  


  
  loadData<-reactive({
    print ("loadin...")
    stock=getStockName()
    data<-load(stock)

    
    a <- as.Date(data$Date)
    b <- data$Adj.Close
    return (list(x=a,y=b))
  })

  computeRoc<-reactive({
    print ("roc computing...")
    y=loadData()$y
    x=loadData()$x
    k=input$range
    N=length(x)
    lag=input$hold
    rc<-roc(y[(N-k):N],lag=lag)
    return (rc)
  })  
  
  
  output$figure <- renderPlot(
                            { x=loadData()$x
                              y=loadData()$y
                              index = (length(x)-input$range):length(x)
                              
                              par(mfrow=c(3,1),mar=c(2.5,2.5,1.5,1.5),oma=c(1,1,1,1)) 
                              plot(x[index],y[index],type='l',
                                   xlab="time",main="price")
                              
                              
                              rc<-computeRoc()
                              N=length(x)
                              k=input$range
                              plot(x[(N-k):(N-k+length(rc)-1)],
                                   rc,
                                   type='l',
                                   xlab="time",
                                   main="price rate of change")
                              abline(h=0,col="red")
                              
                              kd<-density(rc)
                              
                              plot(kd,main = "density prob. of return")
                              mu=mean(rc)
                              s=sd(rc)
                              abline(v=mu,col="blue")
                              text(mu,1,paste("mu:",round(mu,2)))
                              abline(v=mu+s,col="red")
                              abline(v=mu-s,col="red")
                              
                            })
  
  
    output$text<-renderText({
                              rc=computeRoc()
                              
                              P=ecdf(rc)
                              paste("mean:",round(mean(rc),2),
                                    "\nstd:",round(sd(rc),2),
                                    "\nprob of 2% loss:",P(-0.02),
                                    "\nprob of 2% earn:",1-P(0.02),
                                    "\nprob of 5% loss:",P(-0.05),
                                    "\nprob of 5% earn:",1-P(0.05))})

})
