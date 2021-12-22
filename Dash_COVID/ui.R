library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)




# Header bar of the dashboard (ie le titre du projet)
header <- dashboardHeader(title = "Dashboard sur les données du COVID",
                          titleWidth = 380)


# Menu depliant avec les deux onglets et les inputs du departement etc...
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Situation un jour donné",
           tabName = "sitj",
           icon = icon("head-side-virus")),
  menuItem("Historique",
           tabName = "hist",
           icon = icon("grunt")),
  menuItem("Carte",
           tabName = "carto",
           icon = icon("grunt"))
))

# Corps du dashboard avec les graphes et les values box
body <- dashboardBody(
  # creation des pages suivant les differents menuItem
  tabItems(
    tabItem(
      tabName = "sitj",
      fluidPage(
        titlePanel("Données du jour"),
        sidebarPanel(
          dateInput("jj",
                    "Selectionner le jour",
                    min = "2019-01-01",
                    max = "2021-08-12",
                    value = "2021-08-12",
                    weekstart = 1,
                    language = "fr"), # changer jour min pour mettre 1er jour contamination
          width = 3,
          uiOutput("loc"),
          actionButton("bout", "Affichage"),
          downloadButton('download', 'Telechargement')
        ),


        mainPanel( # 3lignes de deux values box


          h2("Widgets tab content"),
          fluidRow(
            valueBoxOutput("hosp"), #CHANGER les noms
            valueBoxOutput("rea")
          ),
          h2("Widgets tab content"),
          fluidRow(
            valueBoxOutput("nhosp"),
            valueBoxOutput("nrea")
          ),
          h2("Widgets tab content"),
          fluidRow(
            valueBoxOutput("de"),
            valueBoxOutput("gu")
          )
        )
        # actionButton de telechargement a ajouter sur le side panel
        # possibilite d ajouter un choix pour la france ou un dep particulier

        )
    ),
    tabItem(
      tabName = "hist",
      h2("Widgets tab content"),
      sidebarPanel(
        dateRangeInput("range",
                       "Selectionner la periode",
                       min = "2020-03-18",
                       max = "2021-12-12",
                       start = "2020-03-18",
                       end = "2021-12-12",
                       weekstart = 1,
                       language = "fr",
                       separator = "au"),
        uiOutput("ddep"),
        actionButton("boutrange",
                     "Affichage"),
        downloadButton('downloadData', 'Telechargement'),
        width = 4
      ),
      mainPanel(
        plotlyOutput("graph_sit"),
        plotlyOutput("graph_cumul"),
        plotlyOutput("graph_nvx")
      )
     # actionButton de telechargement a ajouter soit sur le main panel soit side panel
      #ajouter input ggplot et input choix dep
    ),
    tabItem(
      tabName = "carto",
      sidebarPanel(
        dateInput("datc",
                  "Choisir date",
                  min = "2019-01-01",
                  max = "2021-08-12",
                  value = "2021-08-12",
                  weekstart = 1,
                  language = "en"),
       # uiOutput("departem"),
        selectInput("var",
                    "Choix variable",
                    choices = c("Hospitalises",
                                "Reanimation",
                                "Deces",
                                "Gueris",
                                "NouvellesHospitalisations",
                                "NouvellesReanimations")),
        actionButton("boutcart", "Affichage")

      ),
      mainPanel(
       # plotOutput("map"),
        leafletOutput("mymap")

      )
    )
  )
)

# Mise en page du dashboard
dashboardPage(
  header,
  sidebar,
  body,
  skin = "green"
)
