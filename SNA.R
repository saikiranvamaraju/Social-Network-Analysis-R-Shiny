
library("shiny")
library("visNetwork")
library("reshape")
library("igraph")
library("visNetwork")


mldata <- read.csv("D:/ABA with R/Project1/COCAINE_DEALING.csv")
mldata1 <- read.csv("D:/ABA with R/Project1/COCAINE_DEALING.csv",row.names = 1)

mdata <- melt(mldata, id = 1)
Submdata <- subset(mdata, mdata$value!=0)
Submdata

nodes <- data.frame(id = mldata[,1],label= mldata[,1])
edges <- data.frame(from = Submdata[,1], 
                    to = Submdata[,2])
matrix <- as.matrix(mldata1)
g <- graph_from_adjacency_matrix(matrix, mode = "undirected", diag = FALSE, weighted = TRUE)
g1 <- graph_from_adjacency_matrix(matrix, mode = "directed", diag = FALSE, weighted = TRUE)
d1 <- igraph::degree(g,mode="all")
b1 <- betweenness(g)
c<- paste(mldata[,1],d1,b1)

Indegree <- igraph::degree(g1,mode="in")
Outdegree <- igraph::degree(g1,mode="out")
a <- cbind(d1,Indegree,Outdegree,b1)
colnames(a) <- c("Degree", "InDegree","OutDgree","Betweenness")
#3 melting the data set
mdata <- melt(mldata)
names(mdata) <- c("calling","called","times")


Totalcalls = sum(mdata$times)
#5 Most calls recieved

MostCallsRecieved=aggregate(times~called,mdata,sum)
MostCallsRecieved
index1=which(MostCallsRecieved$times==max(MostCallsRecieved$times))
index1
b2=MostCallsRecieved$called[index1]
b2
#6 Most calls placed

MostCallsPlaced=aggregate(times~calling,mdata,sum)
MostCallsPlaced
index2=which(MostCallsPlaced$times==max(MostCallsPlaced$times))
index2
b3=MostCallsPlaced$calling[index2]


r <- data.frame(b2)
r
ui <- fluidPage(
  titlePanel("Sai Kiran Vamaraju_SNA"),
    mainPanel(
      uiOutput("tb")
  
    )
    
  )

server <- function(input,output){
  
  
  output$betweennesss <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will drop shadow
    nodes$title <- b1 # Text on click
    nodes$size <- b1/4 # Node size
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "orange"
    nodes$color.highlight.background <- "red"
    nodes$color.highlight.border <- "darkred"
    
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "black", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$Degree <- renderVisNetwork({
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will drop shadow
    nodes$title <- d1 # Text on click
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "orange"
    nodes$color.highlight.background <- "red"
    nodes$color.highlight.border <- "darkred"
    nodes$size <- d1*2 # Node size
    
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "red", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))})
  
  output$In <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will drop shadow
    nodes$title <- Indegree # Text on click
    nodes$size <- Indegree*10 # Node size
    nodes$borderWidth <- 2 # Node border width
    
    

    nodes$color.border <- "orange"
    nodes$color.highlight.background <- "red"
    nodes$color.highlight.border <- "darkred"
    
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "black", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$Out <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will draop shadow
    nodes$title <- Outdegree # Text on click
    nodes$size <- Outdegree*4 # Node size
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "orange"
    nodes$color.highlight.background <- "red"
    nodes$color.highlight.border <- "darkred"
    
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "black", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$Inbound <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will draop shadow
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "orange"
    nodes$color.highlight.background <- "red"
    nodes$color.highlight.border <- "darkred"
    
    edges <- data.frame(from = Submdata[,1], 
                        to = Submdata[,2], width = Submdata$value)
    visNetwork(nodes, edges,height = "500px", width = "100%")%>% visEdges(arrows = 'to') %>%
      visEdges(color = list(color = "green", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$OutBound <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will draop shadow
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "orange"
    nodes$color.highlight.background <- "red"
    nodes$color.highlight.border <- "darkred"
    
    edges <- data.frame(from = Submdata[,1], 
                        to = Submdata[,2], width = Submdata$value)
    visNetwork(nodes, edges,height = "500px", width = "100%")%>% visEdges(arrows = 'from') %>%
      visEdges(color = list(color = "green", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  
  output$all <- renderVisNetwork({
    if(is.null(data())){return ()}  
    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will draop shadow
    nodes$borderWidth <- 2 # Node border width
    
    
    #nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$`in-degree centrality`]
    nodes$color.border <- "orange"
    nodes$color.highlight.background <- "red"
    nodes$color.highlight.border <- "darkred"
    
    edges <- data.frame(from = Submdata[,1], 
                        to = Submdata[,2], width = Submdata$value)
    visNetwork(nodes, edges,height = "500px", width = "100%")%>%
      visEdges(color = list(color = "green", highlight = "blue")) %>% visEdges(color = list(hover = "red"))%>% 
      visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                         degree = list(from = 0, to = 2))) %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
                                         background: #f8f8f8;
                                         color: darkblue;
                                         border:none;
                                         outline:none;'))
  })
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(mldata)){return ()}
    mldata
  })
  
    
  output$transformedtable <- renderTable({
    if(is.null(mldata)){return ()}
    melt(mldata, id = 1)
  })
  
  output$Summary <- renderTable(a,rownames = TRUE)
  
  output$text <- renderUI({ 
    str1 <- paste("Total Calls Placed", Totalcalls)
    str2 <- paste("Most Calls were received by",r[1,],"and",r[2,])
    str3 <- paste("Most Calls were Placed by:",b3)
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
  output$obs <- renderUI({ 
    str1 <- paste("1.From the inbound and outbound graphs we can infer that, Kay has connection with almost everyone and seems to be the lead for Drug trafficking scandal.
                  2.Also, Kay has strong connections with Menna, Dante, Tommy , Steve and Blacky from which we can say these people might be the secondary level leaders taking instructions from Kay.
                  3.We can see that each of these strong connections are managing a small group of people. For example Dante is seen managing Robert and Parratta works under Menna. 
                  4.There are also people who take instructions directly from Kay, like charles, Louis etc.,")
   
    HTML(paste(str1, sep = '<br/>'))
  })
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(mldata))
      h5("Powered by", tags$img(src='RStudio-Ball.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("Data", tableOutput("table")),tabPanel("Transformed Data", tableOutput("transformedtable")),tabPanel("Centrality Score", tableOutput("Summary")),tabPanel("Summary", uiOutput("text")),
                  tabPanel("Network Graphs", tabsetPanel( tabPanel("Degree", visNetworkOutput("Degree")),tabPanel("betweenness", visNetworkOutput("betweennesss")),tabPanel("Indegree", visNetworkOutput("In")), tabPanel("Outdegree", visNetworkOutput("Out")),
                                                          tabPanel("All Connections", visNetworkOutput("all")),tabPanel("Inbound Calls", visNetworkOutput("Inbound")),tabPanel("OutBound Calls", visNetworkOutput("OutBound"))
                  )
                  ), tabPanel("Observations", tableOutput("obs")))
  })
  
  
  }

shinyApp(ui = ui, server = server)