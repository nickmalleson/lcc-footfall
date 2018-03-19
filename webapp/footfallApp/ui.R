#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(base)
library(shiny)
library(shinydashboard)

# Define UI for application that ...
shinyUI(
  
  dashboardPage(title = "Demo App", skin = "blue",
    
    dashboardHeader(title = "LCC Footfall Predictor", dropdownMenuOutput("msgOutput"),
 
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                   text = "2 new tabs added to the dashboard",
                                   icon = icon("dashboard"),
                                   status = "success"
                                 ),
                                    notificationItem(
                                    text = "Server is currently running at 95% load",
                                    icon = icon("warning"),
                                    status = "warning"
                                  )
                                  ),
                    dropdownMenu(type = "tasks",
                                 taskItem(
                                 value = 80,
                                 color  = "aqua",
                                 "Shiny Dashboard Education"
                                 ),
                                 taskItem(
                                   value = 55,
                                   color = "red",
                                   "Overall R Education"
                                 ),
                                 taskItem(
                                   value = 40,
                                   color = "green",
                                   "Data Science Education"
                                 )
                            
                                 )
                                
                    ),
    
    dashboardSidebar(

      sidebarMenu(
      sidebarSearchForm("searchText", "buttonSearch", "Search"),
      menuItem("Dashboard", tabName ="dashboard", icon = icon("dashboard")), #align-left
      sliderInput("m", "Set time to update prediction", 5, 60, 10),
      menuItem("Raw Data", tabName = "rawdata", icon=icon("database")),
      menuItem("Map", tabName = "map", icon=icon("map"))
 
          )),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",

                fluidRow(
                  valueBox(15*200, "Current footfall count", icon=icon("hourglass-3"), color = "yellow"),
                  valueBoxOutput("todayaverage"), #itemRequested
                  valueBoxOutput("eventTimeRemaining") #eventTimeRemaining
                ),
  
                  
                  fluidRow(
                    box(
                      width = 8, status = "info", solidHeader = TRUE,
                      title = "Popularity by package",
                      plotOutput("chart")),
                      
                   
                    box(
                      width = 4, status = "info", solidHeader = TRUE,
                      title = "Popularity by package2",
                      plotOutput("chart2"))
                    ),
                
        tabItem("predictorImportance",
                numericInput("maxrows", "Rows to show", 25),
                verbatimTextOutput("processedTable"),
                downloadButton("downloadCsv", "Download as CSV"))
 
    )
    
  )
  
)
)

)