#Shiny app to display the resting state data from NFL Charities

library(shiny)
library(ggplot2)
library(lattice)
library(plyr)
#reshape data to put all of the graph metrics into one response variable and one metric variable
long <- reshape(berlin, 
            varying = c("degree", "localeff", "cluscoeff", "strength"),
            v.names = "value",    
            timevar = "metric",
            times = c("Degree", "Local Efficiency", "Clustering Coefficient", "Strength"),
            new.row.names = 1:19520,
            direction="long")
metriclist<-unique(long$metric)
subnetlist <- unique(as.character(long$subnet))

ui <- fluidPage(
  sidebarPanel(
    selectInput("metrics", "Choose a graph metric:", choices=metriclist, selected ="Degree"),
    selectInput("subnets", "Choose a SubNetwork:", choices=subnetlist, selected ="global"),
    #selectInput("compare", "Choose a Comparison:", choices=as.character(long$subnet)),
    sliderInput("range", "Cross-correlational threshold range:",
                min = 0, max =0.9, step=0.1, value = c(0,0.9))
  ),
  mainPanel(
    plotOutput("testPlot")
  ))

server <- function(input, output){
  
  metric <- reactive(input$metrics)
  subnets <- reactive(input$subnets)
  compare <- reactive(input$compare)
  
  output$testPlot <- renderPlot({
    pdata <- subset(long, subnet==as.character(subnets()))
    pdata <- subset(pdata, metric==as.character(metric()))
    psum <- ddply(pdata, c("thr", "conc"), summarize,
                  N = length(value),
                  mean = mean(value),
                  sd = sd(value),
                  se = sd / sqrt(N)
                  )
    ggplot(psum, aes(x=thr, y=mean, colour=conc, group=conc))+
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=0.1, size=0.75)+
      geom_line(size=2)+geom_point(size=4)+theme_classic(base_size = 24)+xlab("r Threshold") +
      ylab("Local Efficiency") +
      guides(colour=guide_legend(title="Concussion\nHistory"))+
      scale_color_manual(values=c("#96C0E6", "#D5BA31"))+
      theme(legend.justification = c(1, 1), legend.position = c(1, 1))
  })
  } 

shinyApp(ui = ui, server = server)

