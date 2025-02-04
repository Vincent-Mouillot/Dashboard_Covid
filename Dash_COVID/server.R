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
library(stringr)
library(sf)
library(leaflet)
library(lubridate)
library(tidyr)

shinyServer(function(input, output) {
  ##### Recup liste dep
  replace_acc_onglet1 <- function(x) {
    x <- str_replace_all(x, c("Ã¨" = "è", "Ã´" = "ô", "Ã©" = "é"))
  }

  replace_acc_onglet2 <- function(x) {
    x <- str_replace_all(x, c("Ã¨" = "e", "Ã´" = "o", "Ã©" = "e", "ô" = "o"))
    x <- str_to_lower(x) # A modifier pour propre
    x
  }

  liste_departement <- function() {
    ldep <- "https://geo.api.gouv.fr/departements"
    listede <- GET(ldep)
    listede <- fromJSON(rawToChar(listede$content))

    listede <- listede %>%
      apply(2, replace_acc_onglet1) %>%
      as.data.frame() %>%
      select(nom) %>%
      arrange(nom)
    listede[, 1]
  }

  ##### Premier onglet
  sumna <- function(x) {
    sum(x, na.rm = TRUE)
  }

  donn <- eventReactive(input$bout, {
    # recuperation donnees mise en forme vector de somme
    date <- paste(day(input$jj), "-",
      month(input$jj), "-",
      year(input$jj),
      sep = ""
    )
    ap <- paste(
      "https://coronavirusapifr.herokuapp.com/data/departements-by-date/",
      as.character(date),
      sep = ""
    )
    donneebr <- GET(ap)
    lis <- fromJSON(rawToChar(donneebr$content))
    dat <- lis %>% select(
      lib_dep,
      hosp,
      rea,
      incid_hosp,
      incid_rea,
      incid_dchosp,
      incid_rad
    )
    dat <- dat %>% as.data.frame()
    dat$lib_dep <- dat$lib_dep %>%
      as.data.frame() %>%
      apply(1, replace_acc_onglet1)
    dat <- dat %>% filter(lib_dep == input$loc)
    dat
  })

  output$hosp <- renderValueBox({
    valueBox(donn()[, 2],
      subtitle = "Hospitalisation",
      icon("hospital")
    )
  })


  output$rea <- renderValueBox({
    valueBox(donn()[, 3],
      subtitle = "Réanimation",
      icon("bed")
    )
  })

  output$nhosp <- renderValueBox({
    valueBox(donn()[, 4],
      subtitle = "Nouvelle Hospitalisation",
      icon("plus")
    )
  })

  output$nrea <- renderValueBox({
    valueBox(donn()[, 5],
      subtitle = "Nouvelle Réanimation",
      icon("plus")
    )
  })

  output$de <- renderValueBox({
    valueBox(donn()[, 6],
      subtitle = "Décès",
      icon("heart-broken")
    )
  })

  output$gu <- renderValueBox({
    valueBox(donn()[, 7],
      subtitle = "Guérison",
      icon("heart")
    )
  })

  output$download <- downloadHandler(
    filename = function() {
      paste("donnee_du_jour", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      write.csv(donn(), con)
    }
  )



  ##### Deuxieme onglet pour chaque dep
  mef_don_dep <- function(x, date_depart, date_fin) {
    x <- x %>%
      select(
        date,
        hosp,
        rea,
        dchosp,
        incid_hosp,
        incid_rea,
        incid_dchosp,
        incid_rad
      )

    seqD <- seq.Date(
      from = as.Date(date_depart),
      to = as.Date(date_fin),
      length.out = as.numeric(difftime(as.Date(date_fin), as.Date(date_depart)))
    ) # ,
    # by=1)
    d <- x %>% filter(date %in% as.character(seqD))
    d <- d %>% drop_na()
    d
  }

  det <- function(date){
    date <- paste(day(date), "-", month(date), "-", year(date), sep = "")

    apdep <- paste(
      "https://coronavirusapifr.herokuapp.com/data/france-by-date/",
      as.character(date), sep = "")

    donndep <- GET(apdep)
    donneedep <- fromJSON(rawToChar(donndep$content))
    don <- donneedep %>% as.data.frame() %>%
      select(date,
             hosp,
             rea,
             dchosp,
             incid_hosp,
             incid_rea,
             incid_dchosp,
             incid_rad
      )
    return(don)
  }

  donn_dep <- eventReactive(input$boutrange, {
    if(input$loc == "France"){
      seq_date <- seq.Date(as.Date(input$range[1], "%d-%m-%Y"),
                           as.Date(input$range[2], "%d-%m-%Y"), by = 1)
      don <- det(seq_date[1])
      for (i in 1:length(seq_date)){
        don <- rbind(don, det(seq_date[i]))
      }

    }
    else{
    apdep <- paste(
      "https://coronavirusapifr.herokuapp.com/data/departement/",
      as.character(replace_acc_onglet2(input$loc)), # changer avec input mais recup liste dep avant
      sep = ""
    )
    donndep <- GET(apdep)
    donneedep <- fromJSON(rawToChar(donndep$content))

    don <- donneedep %>% as.data.frame()
    don <- mef_don_dep(don, input$range[1], input$range[2])}
    don
  })


  output$graph_sit <- renderPlotly(
    ggplotly(
      ggplot(
        donn_dep(),
        aes(x = as.Date(date))
      ) +
        geom_line(aes(y = hosp, colour = "hosp")) +
        geom_line(mapping = aes(
          y = rea,
          colour = "rea"
        )) +
        scale_colour_manual("",
          breaks = c("hosp", "rea"),
          values = c("blue", "orange")
        ) +
        xlab("Date") +
        ylab("Nombre de personne") +
        labs(title = "Situation hopitaux jour par jour")
    )
  )

  output$graph_cumul <- renderPlotly(
    ggplotly(
      ggplot(
        data = donn_dep(),
        aes(x = as.Date(date))
      ) +
        geom_line(mapping = aes(
          y = incid_dchosp,
          colour = "deces"
        )) +
        geom_line(mapping = aes(
          y = incid_rad,
          colour = "gueris"
        )) +
        scale_colour_manual("",
          breaks = c("deces", "gueris"),
          values = c("blue", "orange")
        ) +
        xlab("Date") +
        ylab("Nombre de personne") +
        labs(title = "Nombre de décès et de guérison par jour")
    )
  )

  output$graph_nvx <- renderPlotly(
    ggplotly(
      ggplot(
        data = donn_dep(),
        aes(x = as.Date(date))
      ) +
        geom_line(mapping = aes(
          y = incid_hosp,
          colour = "hopital"
        )) +
        geom_line(mapping = aes(
          y = incid_rea,
          colour = "réa"
        )) +
        scale_colour_manual("",
          breaks = c("hopital", "réa"),
          values = c("blue", "orange")
        ) +
        xlab("Date") +
        ylab("Nombre de personne") +
        labs(title = "Nombre d'arrivées en hopital et en réanimation par jour")
    )
  )


  output$tot_dc <- renderValueBox({
    valueBox(sum(donn_dep()$incid_dchosp, na.rm = TRUE),
      subtitle = "Total de mort à l'hopital sur la période",
      color = "blue"
    )
  })

  output$tot_gue <- renderValueBox({
    valueBox(sum(donn_dep()$incid_rad, na.rm = TRUE),
      subtitle = "Total de guéris sur la période",
      color = "orange"
    )
  })

  output$tot_hos <- renderValueBox({
    valueBox(sum(donn_dep()$incid_hosp, na.rm = TRUE),
      subtitle = "Total admis à l'hopital sur la période",
      color = "blue"
    )
  })

  output$tot_ad <- renderValueBox({
    valueBox(sum(donn_dep()$incid_rea, na.rm = TRUE),
      subtitle = "Total admis en réa sur la période",
      color = "orange"
    )
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      paste("donnee_dep_periode", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      write.csv(donn_dep(), con)
    }
  )



  ##### 3e onglet carto
  # map_fr<-function(){
  #   FranceFormes <- getData(name="GADM", country="FRA", level=2)
  #   plot(FranceFormes, main="Carte de la France, départements")
  #   Fr<-st_as_sf(FranceFormes)
  # }

  # donnc <- eventReactive(input$boutcart, {
  #   # recuperation donnees mise en forme vector de somme
  #   date <- paste(day(input$jj),"-",
  #                 month(input$jj), "-",
  #                 year(input$jj), sep = ""
  #   )
  #   ap <- paste(
  #     #"https://coronavirusapi-france.now.sh/AllDataByDate?date=",
  #     "https://coronavirusapifr.herokuapp.com/data/departements-by-date/",
  #     as.character(date),
  #     sep = "")
  #   donneebr <- GET(ap)
  #   lis <- fromJSON(rawToChar(donneebr$content))
  #   # On obtient la liste des infos par dep pour une date précis
  #   dat <- lis # %>% select(tolower(input$var))
  #   dat
  # })

  # output$depa<-renderUI({
  #   lit_dep<-liste_departement()
  #
  #   selectInput("departem",
  #               "Choisir dep",
  #               choices = c("France",lit_dep)
  #   )
  # })

  # France<- st_read(here::here("Dash_COVID/departements-20180101.shp"), quiet=TRUE)
  # #dep <- France %>%  dplyr::filter(nom %in% "Doubs")
  #
  #
  #
  #    output$mymap <- renderLeaflet({
  #      #donn_dep()$gueris
  #     bin <- c(0, 50, 100, Inf)
  #    pal <- colorBin("YlOrRd", domain =  donnc()$hosp, bins = bin)
  #
  #      isolate({
  #        long<-3 ; lat<-47 ; z=5.05
  #        leaflet() %>%
  #          setView(long,lat,z) %>%
  #          addProviderTiles("Esri.WorldTerrain")%>%
  #          addMiniMap(width = 75, height = 75, zoomLevelOffset = -7) %>%
  #          addPolylines(data = France, color="black", fillOpacity = 0,
  #                       weight = 1, opacity = 1) %>%
  #          addPolygons(data = donn(), stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
  #                       color = ~colorQuantile("YlOrRd", hosp)(hosp) )
  #
  #      })
  #    })
})
