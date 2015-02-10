library("shiny")
library("networkD3")
library("dygraphs")

shinyUI(fluidPage(
  titlePanel("OCStock"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock belonging to S&P 100 index. 
        Information about prices will be collected from yahoo finance."),
    
      selectInput("symb", "Symbol", 
                  c("AAPL","ABBV","ABT","ACN","AIG","ALL","AMGN","AMZN","APA","APC","AXP","BA","BAC","BAX","BIIB","BK","BMY","BRK.B","C","CAT","CL","CMCSA","COF","COP","COST","CSCO","CVS","CVX","DD","DIS","DOW","DVN","EBAY","EMC","EMR","EXC","F","FB","FCX","FDX","FOXA","GD","GE","GILD","GM","GOOG","GS","HAL","HD","HON","HPQ","IBM","INTC","JNJ","JPM","KO","LLY","LMT","LOW","MA","MCD","MDLZ","MDT","MET","MMM","MO","MON","MRK","MS","MSFT","NKE","NOV","NSC","ORCL","OXY","PEP","PFE","PG","PM","QCOM","RTN","SBUX","SLB","SO","SPG","T","TGT","TWX","TXN","UNH","UNP","UPS","USB","UTX","V","VZ","WBA","WFC","WMT","XOM"), multiple = T, selected = ""),
    
      dateRangeInput("dates", 
        "Date range",
        start = "2013-01-01", 
        end = as.character(Sys.Date())),
   
      actionButton("get", "Get Stock"),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      checkboxInput("corr", "Order correlation matrix for stocks", 
        value = FALSE),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      sliderInput("treecol", "Number of clusters for the tree", min = 2, max=10, value = 2),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      checkboxInput("mst", 
        "plot minimum spanning tree", value = FALSE)
    ),
    
    mainPanel(
      #textOutput("text"),
      helpText(h2("Time Series", align = "center")),
      dygraphOutput("retplot"),
      helpText(h2("Correlation Matrix", align = "center")),
      plotOutput("corrplot"),
      helpText(h2("Hierarchical Tree", align = "center")),
      plotOutput("hclustplot"),
      helpText(h2("Minimum Spanning tree", align = "center")),
      forceNetworkOutput("mstplot")
      
    )
)))