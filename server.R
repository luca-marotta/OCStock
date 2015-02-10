# server.R

library("shiny")
library("reshape2")
library("ggplot2")
library("igraph")
library("networkD3")
library("quantmod")
library("RCurl")
library("scales")
library("xts")
library("dygraphs")
library("RColorBrewer")
source("helpers.R")


shinyServer(function(input, output) {
  dataInput <- reactive({
    if(input$get==0) return(NULL)
    isolate({getData(input$symb, from = input$dates[1], input$dates[2])
    })
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
    
    
    tmp <- dataInput()
    cmat <- cor(tmp[,2:ncol(tmp)])
    diag(cmat) <- NA
    if(input$corr){
      ######## ordering columns using hc
      mergemat <- t(hcInput()$merge)
      cmat <- cmat[abs(mergemat[mergemat<0]), abs(mergemat[mergemat<0])]
      df <- reshape2::melt(cmat);
      p <- ggplot(data=df, aes(x=Var1, y=Var2)) + 
        geom_tile(aes(fill=value)) +
        scale_fill_gradient(low = "#0000FF", high ="#FF0000", 
                             space = "rgb", guide = "colourbar")
      return(p)
    }
    df <- melt(cmat)
    p <- ggplot(data=df, aes(x=Var1, y=Var2)) + 
         geom_tile(aes(fill=value))+
         scale_fill_gradient(low = "#0000FF", high ="#FF0000", 
                        space = "rgb", guide = "colourbar")
    return(p)
})

hcInput <- reactive({
  tmp <- dataInput();
  distmat <- as.dist(sqrt(2*(1-cor(tmp[,2:ncol(tmp)]))));
  return(hclust(distmat, method="single"));
  
})

mstInput <- reactive({ 
  if(!input$mst) return(list(links=data.frame(`source`=1, target=1, value=0 ), nodes=data.frame(name=1, group=1)))
    tmp <- dataInput()
    adj <- sqrt(2*(1 - cor(tmp[,2:ncol(tmp)])));
    g <- graph.adjacency(adj, mode = "undirected", weighted="weight");
    mst <- minimum.spanning.tree(g);
    nodes <- get.data.frame(mst, what = "vertices");
    nodes$group <- 1;
    links <- as.data.frame(get.edgelist(mst, names = F)) - 1;
    colnames(links) <- c("source", "target");
    links$value <- 20; 
    return(list(links=links, nodes=nodes))
  
})

  output$retplot <- renderDygraph({
    if(input$get==0) return(NULL)
    dygraph(xts(dataInput()[,2:ncol(dataInput())], order.by = as.Date(dataInput()$Date, format="%Y-%m-%d")))
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
  
  output$mstplot <- renderForceNetwork({
    if(input$mst == 0) return(NULL)
                          forceNetwork(Links = mstInput()$links, Nodes = mstInput()$nodes, 
                          Source = "source", Target="target", Value = "value", 
                          NodeID = "name", Group = "group", opacity = 1)
})


  
})