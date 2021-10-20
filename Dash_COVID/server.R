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
    dat <- lis$allFranceDataByDate[, c(4:9)]
    glimpse(dat)
    apply(dat, 2, sumna)
  })



  output$hosp <- renderValueBox({
    valueBox(donn()[1], subtitle = "Hosp")
  })

  output$rea <- renderValueBox({
    valueBox(donn()[2], subtitle = "Rea")
  })

  output$nhosp <- renderValueBox({
    valueBox(donn()[3], subtitle = "Nvle Hosp")
  })

  output$nrea <- renderValueBox({
    valueBox(donn()[4], subtitle = "Nvle Rea")
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

  mef_don<-function(x,date_depart,date_fin){  #a mettre en reactive dans serveur en ajoutant sel et sel_date
    glimpse(x)
    don<-x %>% sapply(sel)
    glimpse(don)
    dat<-x %>% sapply(sel_date)
    glimpse(dat)

    don<-t(don[-c(1:34)]) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()
    glimpse(don)
    cname<-colnames(don)
    don<-unlist(don)
    dat<-((da[-c(1:34)]))
    #les 33 premieres lignes sont pourries et pas standardisees
    #a partir de la ligne 34 toutes les lignes ont la meme forme donc plus simple
    donnee<-don %>% matrix(ncol = 6) %>% data.frame(row.names = dat)
    colnames(donnee)<- cname

    seqD<-seq.Date(from=as.Date(date_depart),to=as.Date(date_fin),by=1)
    d<-donnee[c(as.character( seqD)),]
    glimpse(d)
    d
  }

  donn_dep<- eventReactive(input$boutrange, {

    apdep<-paste("https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=",
                 as.character("Rhône"), #changer avec input mais recup liste dep avant
                 sep = "")
    donndep<-GET(apdep)
    donneedep<-fromJSON(rawToChar(donndep$content))

    don<-mef_don(donneedep$allDataByDepartement,boutrange[1],boutrange[2])

    don
  })



  output$graph<- renderPlot(ggplot(data = donn_dep()) +
                              geom_point(mapping = aes(x = seq(1:nrow(donn_dep())))))
  #filtre des donnees en fction des dates donnees
  #ajouter ggplot avec donnees
})
