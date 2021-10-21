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
  #####Recup liste dep
  replace_acc<-function(x){
    str_replace_all(x,c("Ã¨" = "è", "Ã´" = "ô", "Ã©" = "é"))
  }

  liste_departement<-function(){
    ldep<-"https://geo.api.gouv.fr/departements"
    listede<-GET(ldep)
    listede<-fromJSON(rawToChar(listede$content))

    listede<-listede %>%
      apply(2,replace_acc) %>%
      as.data.frame() %>%
      select(nom) %>%
      arrange(nom)
    listede[,1]
  }

  #####Premier onglet
  sumna <- function(x) {
    sum(x, na.rm = TRUE)
  }

  donn <- eventReactive(input$bout, {
    # recuperation donnees mise en forme vector de somme
    ap <- paste(
            "https://coronavirusapi-france.now.sh/AllDataByDate?date=",
            as.character(input$jj),
            sep = "")
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
  mef_don_dep<-function(x,date_depart,date_fin){
    x<-x %>%
      filter(sourceType == "sante-publique-france-data") %>%
      select(date,
             hospitalises,
             reanimation,
             deces,
             gueris,
             nouvellesHospitalisations,
             nouvellesReanimations)
    donnee<-x %>%
            data.frame(row.names = x$date) %>%
            select(-date)

    seqD<-seq.Date(from=as.Date(date_depart),
                   to=as.Date(date_fin),
                   by=1)
    d<-donnee[c(as.character( seqD)),]

    d
  }


  output$ddep<-renderUI({
    li_dep<-liste_departement()

    selectInput("departe",
                "Choisir dep",
                choices = c("France",li_dep) #pb avec France
                )
  })

  donn_dep<- eventReactive(input$boutrange, {
    apdep<-paste(
      "https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=",
      as.character(input$departe), #changer avec input mais recup liste dep avant
      sep = "") #marche que pour le Rhone pour le moment
    donndep<-GET(apdep)
    donneedep<-fromJSON(rawToChar(donndep$content))
    glimpse(donneedep$allDataByDepartement)
    don<-mef_don_dep(donneedep$allDataByDepartement,
                     input$range[1],
                     input$range[2])
    glimpse(don)
    don
  })



  output$graph_sit<- renderPlotly(
    ggplotly(
      ggplot(data = donn_dep(),
             aes(x = as.Date(rownames(donn_dep())))) +
          geom_line(mapping = aes(y=hospitalises,
                                  colour = "hosp")) +
          geom_line(mapping = aes(y=reanimation,
                                  colour = "rea" )) +
          scale_colour_manual("",
                              breaks = c("hosp","rea"),
                              values = c("blue", "orange")) +
          xlab("Date") +
          ylab("Nombre de personne") +
          labs(title = "Situation hopitaux jour par jour")
    ) )

  output$graph_cumul<- renderPlotly(
    ggplotly(
      ggplot(data = donn_dep(),
             aes(x = as.Date(rownames(donn_dep())))) +
          geom_line(mapping = aes(y=deces,
                                  colour = "deces")) +
          geom_line(mapping = aes(y=gueris,
                                  colour = "gueris" )) +
          scale_colour_manual("",
                              breaks = c("deces","gueris"),
                              values = c("blue", "orange")) +
          xlab("Date") +
          ylab("Nombre de personne") +
          labs(title = "Cumul décès et guérison")
  ) )

  output$graph_nvx<- renderPlotly(
    ggplotly(
      ggplot(data = donn_dep(),
             aes(x = as.Date(rownames(donn_dep())))) +
          geom_line(mapping = aes(y=nouvellesHospitalisations,
                                  colour = "nvlleh")) +
          geom_line(mapping = aes(y=nouvellesReanimations,
                                  colour = "nvller" )) +
          scale_colour_manual("",
                              breaks = c("nvlleh","nvller"),
                              values = c("blue", "orange")) +
          xlab("Date") +
          ylab("Nombre de personne") +
          labs(title = "Arrivée en hopital")
  ) )

})
