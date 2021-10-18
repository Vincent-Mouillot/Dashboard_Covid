library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(httr)
library(jsonlite)
library(dplyr)

#Pour les inputs voir les donnees dans l'api par ex graphe historique contamination par jour et contamination cumulees etc..

#Header bar of the dashboard (ie le titre du projet)
header<-dashboardHeader(title = "Dashboard sur les données du COVID", titleWidth = 380)


#Menu depliant avec les deux onglets et les inputs du departement etc...
sidebar<-dashboardSidebar(sidebarMenu(
  dateInput("jj","Selectionner le jour",min = "2019-01-01",max = "2021-08-12",value = "2021-08-12" ,weekstart = 1, language = "fr"), #changer jour min pour mettre 1er jour contamination
  actionButton("bout","Affichage"),
  menuItem("Situation un jour donné", tabName = "sitj", icon = icon("head-side-virus")),
  menuItem("Historique", tabName = "hist", icon = icon("grunt"))
))

#Corps du dashboard avec les graphes et les values box
body<-dashboardBody(
  #creation des pages suivant les differents menuItem
  tabItems(
    tabItem(tabName = "sitj",
    fluidPage(


  titlePanel("Données du jour"), #ajouter date reactive au titre

    mainPanel( #3lignes de deux values box
      h2("Widgets tab content"), #changer titre
      fluidRow(
        valueBoxOutput("hosp"),
        valueBoxOutput("rea")

      ),
      h2("Widgets tab content"), #changer titre
      fluidRow(
        valueBoxOutput("nhosp"),
        valueBoxOutput("nrea")
      ),
      h2("Widgets tab content"), #changer titre
      fluidRow(
        valueBoxOutput("de"),
        valueBoxOutput("gu")
      )
    )
    #actionButton de telechargement a ajouter soit sur le main panel soit side panel
    #possibilite d ajouter un choix pour la france ou un dep particulier
)),
    tabItem(
      tabName = "hist",
      h2("Widgets tab content"),
      sidebarPanel(  dateInput("jd","Selectionner le jour de depart",min = "2019-01-01",value = "2020-01-01",weekstart = 1, language = "fr"),
                     downloadButton("tel")
      ),
      mainPanel(

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
  skin = "green"
)


