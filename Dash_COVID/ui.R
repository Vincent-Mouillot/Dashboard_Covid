library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(httr)
library(jsonlite)
library(dplyr)

library(DT)


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
  menuItem("table",
           tabName = "table",
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
          actionButton("bout", "Affichage")
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
                       max = "2021-08-12",
                       start = "2020-03-18",
                       end = "2021-08-12",
                       weekstart = 1,
                       language = "fr",
                       separator = "au"),
        uiOutput("ddep"),
        actionButton("boutrange",
                     "Affichage"),
        downloadButton("tel"),
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
      tabName = "table",
      fluidRow(
        column(6,
               box(title = "Data", status = "success", solidHeader = T, width = 12,
                   splitLayout(
                     cellArgs = list(style = "padding: 10px"),
                     dateRangeInput("t_range",
                                    "Selectionner la periode",
                                    min = "2020-03-18",
                                    max = "2021-08-12",
                                    start = "2020-03-18",
                                    end = "2021-08-12",
                                    weekstart = 1,
                                    language = "fr",
                                    separator = "au"),
                     cellWidths = c(400,200),
                     selectInput("select2","select2",c("A3","A4","A5"), selected = "A3"))
               ),
               DTOutput("tab_don")
        ),
        column(6,
               box(title = "Data", status = "success", solidHeader = T, width = 12,

                     cellArgs = list(style = "padding: 10px"),
                     selectInput("select11","select11",c("A1","A2","A3"), selected = "A1")

               )
               )


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
