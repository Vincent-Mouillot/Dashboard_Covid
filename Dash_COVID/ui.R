library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)

header<-dashboardHeader(title = "Dashboard sur les données du COVID", titleWidth = 380)



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


body<-dashboardBody(
  tabItems(
    tabItem(tabName = "sitj",
    fluidRow(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

    mainPanel(
      plotlyOutput("distPlot")
    )

)),
    tabItem(
      tabName = "hist",
      h2("Widgets tab content")

    )


)

)






dashboardPage(
  header,
  sidebar,
  body,
  skin = "green"
)
# Define UI for application that draws a histogram

