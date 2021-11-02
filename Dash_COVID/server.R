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

  output$loc<-renderUI({
    li_dep<-liste_departement()

    selectInput("loc",
                "Choisir dep",
                choices = c("France",li_dep) #pb avec France
    )
  })


  donn <- eventReactive(input$bout, {
    # recuperation donnees mise en forme vector de somme
    ap <- paste(
            "https://coronavirusapi-france.now.sh/AllDataByDate?date=",
            as.character(input$jj),
            sep = "")
    donneebr <- GET(ap)
    lis <- fromJSON(rawToChar(donneebr$content))
    # On obtient la liste des infos par dep pour une date précis
    #dat <- lis$allFranceDataByDate[, c(2,4:9)]
    dat <- lis$allFranceDataByDate %>% select(nom,
                                              hospitalises,
                                              reanimation,
                                              nouvellesHospitalisations,
                                              nouvellesReanimations,
                                              deces,
                                              gueris)
    dat<-dat %>% as.data.frame()
    un<-unique(dat[,1])
    dat<-dat %>% filter(nom == un)
    #liste<-dat %>% apply(2, replace_acc) %>% as.data.frame() %>% select(nom)
    dat$nom<-dat$nom %>% as.data.frame() %>% apply(1, replace_acc)
    #dat<- cbind(liste, dat[,-1])
    dat<- dat %>% filter(nom == input$loc)
    glimpse(dat)
    dat
  })

  output$hosp <- renderValueBox({
    valueBox(donn()[2], subtitle = "Hosp")
  })


  output$rea <- renderValueBox({
    valueBox(donn()[3], subtitle = "Rea")
  })

  output$nhosp <- renderValueBox({
    valueBox(donn()[4], subtitle = "Nvle Hosp")
  })

  output$nrea <- renderValueBox({
    valueBox(donn()[5], subtitle = "Nvle Rea")
  })

  output$de <- renderValueBox({
    valueBox(donn()[6], subtitle = "Deces")
  })

  output$gu <- renderValueBox({
    valueBox(donn()[7], subtitle = "Guerison")
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


  #####3e onglet carto
  map_fr<-function(){
    FranceFormes <- getData(name="GADM", country="FRA", level=2)
    plot(FranceFormes, main="Carte de la France, départements")
    Fr<-st_as_sf(FranceFormes)
  }

  donnc <- eventReactive(input$boutcart, {
    # recuperation donnees mise en forme vector de somme
    ap <- paste(
      "https://coronavirusapi-france.now.sh/AllDataByDate?date=",
      as.character(input$datc),
      sep = "")
    donneebr <- GET(ap)
    lis <- fromJSON(rawToChar(donneebr$content))
    # On obtient la liste des infos par dep pour une date précis
    dat <- lis$allFranceDataByDate %>% select(tolower(input$var))
    dat
  })

  output$depa<-renderUI({
    li_dep<-liste_departement()

    selectInput("departe",
                "Choisir dep",
                choices = c("France",li_dep)
    )
  })

France<- st_read(here::here("Dash_COVID/departements-20180101.shp"), quiet=TRUE)
# dep <- France %>%  dplyr::filter(nom %in% "Doubs")

coordGPS<-function(dep){
  long<-3 ; lat<-47 ; z=5.05
 if (dep == "Mayotte"){long <-45.1181; lat<--12.825 ; z=20
       } else if (dep == "Guyane"){lat = 3.965; long = -53.02 ;z=3
      }  else if(dep =="La Réunion"){ lat=-21.124; long= 55.5386 ; z=22
      } else if (dep =="Martinique"){ lat = 14.6409; long= -60.9988 ; z=20
      }  else {long<-3 ; lat<-47 ; z=5.05}
 return(c(long, lat, z))
}
   output$mymap <- renderLeaflet(
    leaflet() %>%
    setView(lng =  3,lat =  47,
            zoom=5.05)%>% #"Esri.WorldTerrain"
      #"OpenTopoMap" Esri.WorldPhysical
    addProviderTiles("Esri.WorldTerrain")  %>% #Esri.WorldImagery
  #  addPolygons(data=dep, weight = 2, color="orange",fillOpacity=0.55) %>%
    addPolylines(data = France, color="black", fillOpacity = 0, weight = 1,
                 opacity = 1) %>%
    addMiniMap(width = 75, height = 75, zoomLevelOffset = -7)
   )


})
