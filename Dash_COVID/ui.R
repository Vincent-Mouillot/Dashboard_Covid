library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(httr)
library(jsonlite)


#Header bar of the dashboard (ie le titre du projet)
header<-dashboardHeader(title = "Dashboard sur les données du COVID", titleWidth = 380)


#Menu depliant avec les deux onglets et les inputs du departement etc...
sidebar<-dashboardSidebar(sidebarMenu(
  sliderInput("bins",
              "Number of bins:",
              min = 1,
              max = 50,
              value = 30),
  menuItem("Situation un jour donné", tabName = "sitj", icon = icon("chart-bar")),
  menuItem("Historique", tabName = "hist", icon = icon("th")),
  actionButton("bout","Affichage")
))

#Corps du dashboard avec les graphes et les values box
body<-dashboardBody(
  #creation des pages suivant les differents menuItem
  tabItems(
    tabItem(tabName = "sitj",
    fluidPage(


  titlePanel("Old Faithful Geyser Data"),

    mainPanel(
      plotlyOutput("distPlot")
    )
    #actionButton de telechargement a ajouter soit sur le main panel soit side panel
)),
    tabItem(
      tabName = "hist",
      h2("Widgets tab content"),
      mainPanel(
        plotlyOutput("distPlot2")
      )
      #actionButton de telechargement a ajouter soit sur le main panel soit side panel

    )


)

)





#Mise en page du dashboard
dashboardPage(
  header,
  sidebar,
  body,
  skin = "green-light"
)


