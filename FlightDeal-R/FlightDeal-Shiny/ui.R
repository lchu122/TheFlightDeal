library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)

# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(
    title('FLIGHTDEAL')
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = 'map'),
      menuItem("Data", tabName = 'data')
    )
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'map',
              fluidRow(box(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                           leafletOutput("flight.map"), height = 800, width = 12))
              
              
              )
    )
  )
)
