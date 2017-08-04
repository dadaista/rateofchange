library(shiny)

# Define UI for application that draws a histogram
fluidPage(

  titlePanel("Price and Price Rate of Change"),
  sidebarPanel(
                
                textInput("stock", 
                          label = a(href="https://uk.finance.yahoo.com/q/hp?s=^FTSE","insert symbol as in yahoo finance"), 
                          value = "^FTSE"),          
                sliderInput("range", 
                            label = "Time period in days:",
                            min = 1, max = 300, value = 90),
                sliderInput("hold", 
                            label = "Hold period in days:",
                            min = 1, max = 90, value = 15),
                
                
                selectizeInput("popular",
                               label="Select popular symbols",
                               choices=list(ExxonMobile="XOM",
                                            Facebook="FB",
                                            Apple="AAPL",
                                            Google="GOOG",
                                            Microsoft="MSFT",
                                            Activision="ATVI"),
                               
                               selected = NULL,
                               multiple = FALSE,
                               options = NULL)
                
                
                ),
  

  mainPanel(plotOutput("figure"),
            verbatimTextOutput("text"))
  
)
#
