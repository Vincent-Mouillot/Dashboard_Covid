library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    x  <- faithful[, 2]

    aff<- eventReactive(input$bout, {

      input$bins
    })

    output$distPlot <- renderPlotly({

       ggplotly( ggplot()+geom_histogram(mapping = aes(x), data=faithful, bins = aff(), color="blue", fill="orange"))
    })

})
