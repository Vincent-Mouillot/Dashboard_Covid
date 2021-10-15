library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(httr)
library(jsonlite)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    x  <- faithful[, 2]

    aff<- eventReactive(input$bout, {

      input$bins
    })

    output$distPlot <- renderPlotly({

       ggplotly( ggplot()+geom_histogram(mapping = aes(x), data=faithful, bins = aff(), color="cyan", fill="orange"))
    })

    output$distPlot2 <- renderPlotly({

      ggplotly( ggplot()+geom_histogram(mapping = aes(x), data=faithful, bins = aff(), color="cyan", fill="orange"))
    })

    output$mea<-renderValueBox({
      valueBox( round(mean(x)), subtitle = "Moyenne")
    })

    output$mea2<-renderValueBox({
      valueBox( round(mean(x)), subtitle = "Moyenne", color = "black")
    })
})
