library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(httr)
library(jsonlite)
library(dplyr)
library(RColorBrewer)
library(colorspace)
library(colorscience)

shinyServer(function(input, output) {

    sumna<-function(x){
      sum(x,na.rm = TRUE)
    }

    donnj<- eventReactive(input$bout, { #recuperation donnees mise en forme vector de somme
      ap<-paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=",as.character(input$jj),sep = "")
      donneebr<-GET(ap)
      lis<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précis
      dat<-lis$allFranceDataByDate[,c(4:9)]
      glimpse(dat)
      apply(dat,2,sumna)

    })



    output$hosp<-renderValueBox({
      valueBox( donnj()[1], subtitle = "Hosp")
    })

    output$rea<-renderValueBox({
      valueBox( donnj()[2], subtitle = "Rea")
    })

    output$nhosp<-renderValueBox({
      valueBox( donnj()[3], subtitle = "Nvle Hosp")
    })

    output$nrea<-renderValueBox({
      valueBox( donnj()[4], subtitle = "Nvle Rea")
    })

    output$de<-renderValueBox({
      valueBox( donnj()[5], subtitle = "Deces")
    })

    output$gu<-renderValueBox({
      valueBox( donnj()[6], subtitle = "Guerison")
    })


})
