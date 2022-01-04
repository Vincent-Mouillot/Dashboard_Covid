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
           icon = icon("grunt"))
  # menuItem("Carte",
  #          tabName = "carto",
  #          icon = icon("grunt"))
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
                    "Sélectionnez un jour",
                    min = "2019-01-01",
                    max = Sys.Date() -2, # date d'avant-hier
                    value = Sys.Date() -2,
                    weekstart = 1,
                    format = "dd-mm-yyyy",
                    language = "fr"), # changer jour min pour mettre 1er jour contamination
          width = 3,
          uiOutput("loc"),
          actionButton("bout", "Affichage"),
          downloadButton('download', 'Téléchargement')
        ),


        mainPanel( # 3lignes de deux values box


          h2(),
          fluidRow(
            valueBoxOutput("hosp"), #CHANGER les noms
            valueBoxOutput("rea")
          ),
          h2(),
          fluidRow(
            valueBoxOutput("nhosp"),
            valueBoxOutput("nrea")
          ),
          h2(),
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
      h2("Visualisation sur une période"),
      sidebarPanel(
        selectInput("dep_onglet2", "Choisissez le département",
                    choices= liste_departement()),
        # uiOutput("range_date"),
        actionButton("boutrange",
                     "Afficher des dates"),
        uiOutput("range_date"),
        actionButton("boutdate", "Afficher les graphiques"),
        downloadButton('downloadData', 'Téléchargement'),
        width = 4
      ),

      mainPanel(
        plotlyOutput("graph_sit"),
        plotlyOutput("graph_cumul"),
        plotlyOutput("graph_nvx")
      )
     # actionButton de telechargement a ajouter soit sur le main panel soit side panel
      #ajouter input ggplot et input choix dep
    )
    # tabItem(
    #   tabName = "carto",
    #   sidebarPanel(
    #     dateInput("datc",
    #               "Choisir date",
    #               min = "2019-01-01",
    #               max = Sys.Date() ,
    #               value = "2021-08-12",
    #               weekstart = 1,
    #               language = "fr"),
    #    # uiOutput("departem"),
    #     selectInput("var",
    #                 "Choix variable",
    #                 choices = c("hosp",
    #                             "rea",
    #                             "dchosp",
    #                             "incid_rad",
    #                             "incid_hosp",
    #                             "incid_rea")),
    #     actionButton("boutcart", "Affichage")
    #
    #   ),
    #   mainPanel(
    #    # plotOutput("map"),
    #     leafletOutput("mymap")
    #
    #   )
    # )
  )
)

# Mise en page du dashboard
dashboardPage(
  header,
  sidebar,
  body,
  skin = "green"
)
