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
library(tibble)

shinyServer(function(input, output) {
  sumna <- function(x) {
    sum(x, na.rm = TRUE)
  }



  donn <- eventReactive(input$bout, {
    # recuperation donnees mise en forme vector de somme
    ap <- paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=",
                as.character(input$jj), sep = "")
    donneebr <- GET(ap)
    lis <- fromJSON(rawToChar(donneebr$content))
    # On obtient la liste des infos par dep pour une date précis
    dat <- lis$allFranceDataByDate[, c(2,4:9)]
    dat<-dat %>% as.data.frame()
    dat<- dat %>% filter(nom == input$loc)
    #glimpse(dat)
    #apply(dat, 2, sumna) #non besoin , France est dans les individus (pour Vincent)(ou pas à boir)
  })





  output$hosp <- renderValueBox({
    valueBox(donn()[2], subtitle = "Hosp")
  })

  output$rea <- renderValueBox({
    valueBox(donn()[,3], subtitle = "Rea")
  })

  output$nhosp <- renderValueBox({
    valueBox(donn()[,4], subtitle = "Nvle Hosp")
  })

  output$nrea <- renderValueBox({
    valueBox(donn()[,5], subtitle = "Nvle Rea")
  })

  output$de <- renderValueBox({
    valueBox(donn()[5], subtitle = "Deces")
  })

  output$gu <- renderValueBox({
    valueBox(donn()[6], subtitle = "Guerison")
  })



  #####Deuxieme onglet pour chaque dep
  sel<-function(x){
    s<-x %>% as.data.frame() %>% select_if(is.numeric)
    s
  }

  sel_date<-function(x){
    s<-x %>% as.data.frame() %>% select(date)
    s
  }

  mef_don<-function(x){  #a mettre en reactive dans serveur en ajoutant sel et sel_date
    don<-x %>% sapply(sel)
    dat<-x %>% sapply(sel_date)

    don<-t(don[-c(1:34)]) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()
    dat<-((da[-c(1:34)]))
    #les 33 premieres lignes sont pourries et pas standardisees
    #a partir de la ligne 34 toutes les lignes ont la meme forme donc plus simple
    donnee<-data.frame(don,row.names = dat)
    donnee #df de 6  col et +500 lignes avec date en nom de ligne
  }

  #filtre des donnees en fction des dates donnees
  #ajouter ggplot avec donnees
})
