# server.R

library("shiny")
library("reshape2")
library("ggplot2")
library("igraph")
library("networkD3")
library("xts")
library("dygraphs")
library("RColorBrewer")
source("helpers.R")


shinyServer(function(input, output, session) {
  
  ### changing values in selection widget when select all is chosen
  observe({
    if(is.null(input$symb)) return(NULL)
    if(any(input$symb%in%"Select all")) updateSelectInput(session, "symb", 
                                                   choices =  c("Select all","AAPL","ABBV","ABT","ACN","AIG","ALL","AMGN","AMZN","APA","APC","AXP","BA","BAC","BAX","BIIB","BK","BMY","BRK.B","C","CAT","CL","CMCSA","COF","COP","COST","CSCO","CVS","CVX","DD","DIS","DOW","DVN","EBAY","EMC","EMR","EXC","F","FB","FCX","FDX","FOXA","GD","GE","GILD","GM","GOOG","GS","HAL","HD","HON","HPQ","IBM","INTC","JNJ","JPM","KO","LLY","LMT","LOW","MA","MCD","MDLZ","MDT","MET","MMM","MO","MON","MRK","MS","MSFT","NKE","NOV","NSC","ORCL","OXY","PEP","PFE","PG","PM","QCOM","RTN","SBUX","SLB","SO","SPG","T","TGT","TWX","TXN","UNH","UNP",
                                                     "UPS","USB","UTX","V","VZ","WBA","WFC","WMT","XOM"),
                                                   selected = c("AAPL","ABT","ACN","AIG","ALL","AMGN","AMZN","APA","APC","AXP","BA","BAC","BAX","BIIB","BK","BMY","BRK.B","C","CAT","CL","CMCSA","COF","COP","COST","CSCO","CVS","CVX","DD","DIS","DOW","DVN","EBAY","EMC","EMR","EXC","F","FB","FCX","FDX","FOXA","GD","GE","GILD","GM","GS","HAL","HD","HON","HPQ","IBM","INTC","JNJ","JPM","KO","LLY","LMT","LOW","MA","MCD","MDLZ","MDT","MET","MMM","MO","MON","MRK","MS","MSFT","NKE","NOV","NSC","ORCL","OXY","PEP","PFE","PG","PM","QCOM","RTN","SBUX","SLB","SO","SPG","T","TGT","TWX","TXN","UNH","UNP",
                                                               "UPS","USB","UTX","V","VZ","WBA","WFC","WMT","XOM"))
  })

  dataInput <- reactive({
    if(input$get==0) return(NULL)
    isolate({getData(input$symb, from = input$dates[1], input$dates[2])
    })
  })
  
  
  tsInput <- reactive({
    # plot prices or log returns depending on the check box
    if(input$showPrices==T){
      ts <- dataInput()$prices
    } else {
      ts <- dataInput()$data;
    }
    if(input$hideTsLeg==T){
      return(
        dygraph(xts(ts[,2:ncol(ts)], order.by = as.Date(ts$Date, format="%Y-%m-%d")))%>%
        dyLegend(show = "onmouseover", width = 0)
      )
    }
    return(dygraph(xts(ts[,2:ncol(ts)], order.by = as.Date(ts$Date, format="%Y-%m-%d")))%>%
             dyLegend(show = "always", hideOnMouseOut = T, width=400))
  })
  

  hcInput <- reactive({
    tmp <- dataInput()$data;
    distmat <- as.dist(sqrt(2*(1-cor(tmp[,2:ncol(tmp)]))));
    return(hclust(distmat, method="single"));
    
  })
    
  corrInput <- reactive({
    if(input$get==0) return(NULL)
#    if(!input$corr){
#       df <- melt(dataInput())
#       df$Date <- as.Date(df$Date)
#       p <- ggplot(data=df, aes(x=Date, y=value, group=factor(variable), color=factor(variable))) +
#            geom_path() + xlab("") + ylab("log return") + 
#            scale_color_discrete(guide=guide_legend(title="stocks")) +
#         scale_x_date(breaks= date_breaks("months"), labels=date_format("%Y/%m")) + 
#         theme(axis.text.x=element_text(angle=90, size=(12 - 0.5*(nrow(df)/250))))
#       return(p)
#      return(NULL)
#    }
    
    
    tmp <- dataInput()$data

    cmat <- cor(tmp[,2:ncol(tmp)])
    diag(cmat) <- 100
    df <- reshape2::melt(cmat)
    if(input$corr){
      ######## ordering columns using hc
      hc <- hcInput()
      df <- data.frame(Var1=factor(df$Var1, levels = hc$labels[rev(hc$order)]),
                       Var2=factor(df$Var2, levels = hc$labels[rev(hc$order)]), 
                       value=df$value);
      
      p <- ggplot(data=df, aes(x=Var1, y=Var2, fill=value)) + 
        geom_raster(color="white") + xlab("") + ylab("") +
        scale_fill_gradient(low = "#0000FF", high ="#FF0000", 
                             space = "rgb", guide = "colourbar", limits=c(min(cmat), 1), na.value = "#DADAC8") +
        theme_classic(base_size = 15) + 
        theme(axis.text.x=element_text(size=input$corrLSize, angle=90, face="bold", vjust=0.5),
              axis.text.y=element_text(size=input$corrLSize, face="bold"))
      return(p)
    }
    p <- ggplot(data=df, aes(x=Var1, y=Var2)) + 
         geom_tile(aes(fill=value)) +  xlab("") + ylab("") +
         scale_fill_gradient(low = "#0000FF", high ="#FF0000", 
                        space = "rgb", guide = "colourbar", limits=c(min(cmat), 1), na.value = "#DADAC8") +
      theme_classic(base_size = 15) + 
      theme(axis.text.x=element_text(size=input$corrLSize, angle=90, face="bold", vjust=0.5),
          axis.text.y=element_text(size=input$corrLSize, face="bold"))
    return(p)
})


mstInput <- reactive({ 
  if(!input$mst) return(list(links=data.frame(`source`=1, target=1, value=0 ), nodes=data.frame(name=1, group=1)))
    tmp <- dataInput()$data
    adj <- sqrt(2*(1 - cor(tmp[,2:ncol(tmp)])));
    g <- graph.adjacency(adj, mode = "undirected", weighted="weight");
    mst <- minimum.spanning.tree(g);
    nodes <- get.data.frame(mst, what = "vertices");
    nodes$group <- 1;
    #links <- as.data.frame(get.edgelist(mst, names = F)) - 1;
    links <- as.data.frame(get.edgelist(mst));
  
    colnames(links) <- c("source", "target");
    links$value <- 20; 
    #return(list(links=links, nodes=nodes))
    return(links)
  
})

  output$retplot <- renderDygraph({
    if(input$get==0) return(NULL)
    tsInput();
  })

output$tsMergeWarning <- renderText({
  if(input$get==0) return(NULL)
  if(input$dates[1] == dataInput()$finDate[1]) return(NULL)
  #if(1==1) return(input$dates[1] == dataInput()$finDate[1])
  paste(dataInput()$minStock, " has records only starting from ", dataInput()$finDate[1], 
       ". Your input date range has been changed accordingly.", sep="")
})

output$tsNotFoundWarning <- renderText({
  if(input$get==0) return(NULL)
  if(length(dataInput()$not.found)>0){
    st <- paste(dataInput()$not.found, collapse = ",");
    return(paste("Selected stock(s) not present in the time period: ", st, ".", sep=""))
  }
  return(NULL)
})

  output$corrplot <- renderPlot({
    if(input$get == 0) return(NULL)
    corrInput()
  })
  
  output$hclustplot <- renderPlot({
    if(input$get == 0) return(NULL)
    A2Rplot(hcInput(), k=input$treecol, boxes=T, col.up="black",
            col.down = brewer.pal(input$treecol, "Paired"), main="Single Linkage", 
            show.labels = T, lty.up = 1)
    
  })
  
  #output$mstplot <- renderForceNetwork({
   output$mstplot <- renderSimpleNetwork({
    if(input$mst == 0) return(NULL)
#                           forceNetwork(Links = mstInput()$links, Nodes = mstInput()$nodes, 
#                           Source = "source", Target="target", Value = "value", 
#                           NodeID = "name", Group = "group", opacity = 1)
          simpleNetwork(mstInput(), fontSize = 12)
})


  
})