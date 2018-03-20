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
library(leaflet)
library(ggplot2)
library(DT)

Cleaned_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/Cleaned_Dataset/input_Dataset.csv", sep=",", head=TRUE)

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
      
      menuItem("Dashboard", tabName ="dashboard", icon = icon("dashboard")), 
      sliderInput("p", "Adjust Footfall history (length)", 0, 365, 30),
      sliderInput("q", "Prediction length", 0, 30, 1),
      sliderInput("m", "Set time to update prediction", 5, 60, 10),
      
      menuItem("Map", tabName = "map", icon=icon("map")),
      
      menuItem("View Raw Data (Predictors)", tabName = "rawdata", icon=icon("database")), 
  
    
 
     
     conditionalPanel(
                 Cleaned_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/Cleaned_Dataset/input_Dataset.csv", sep=",", head=TRUE),
                 'input.dataset === "Cleaned_footfall"',
                 checkboxGroupInput("show_vars", "Columns in the dataset:",
                                  names(Cleaned_footfall), selected = names(Cleaned_footfall)))
     # conditionalPanel(
     #      'input.dataset === "iris"',
     #      helpText("Display 5 record by default.")
     #    ),
     
     
               
               )
             ),
 
          
    #menuSubItem
    
  body <- dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",

                fluidRow(
                  #valueBoxOutput("currentCount", "Current footfall count", icon=icon("hourglass-3"), color = "yellow"),
                  valueBoxOutput("currentCount"),
                  valueBoxOutput("todayaverage"), #itemRequested
                  valueBoxOutput("eventTimeRemaining") #eventTimeRemaining
                ),
  
                  
                  fluidRow(
                    box(
                      width = 8, status = "info", solidHeader = TRUE,
                      title = "Footfall history",
                      plotOutput("chart")),
                      
                   
                    box(
                      width = 4, status = "info", solidHeader = TRUE,
                      title = "Factors impacting footfall level (by importance)",
                      plotOutput("chart2"))
                  )
                  
                
                ),
                
        # tabItem("predictorImportance",
        #         numericInput("maxrows", "Rows to show", 25),
        #         verbatimTextOutput("processedTable"),
        #         downloadButton("downloadCsv", "Download as CSV")),
        # 
  
        tabItem(tabName = "map",
            h1("Camera Location"),
            fluidRow(
              column(width = 9,
                     box(
                       width = NULL, solidHeader = TRUE,
                       leafletOutput("busmap", height=500)
                     )
                     )  
              #h1("Camera Location")
            )
        ),
        
        tabItem(tabName = "rawdata",
                tabPanel("diamonds", DT::dataTableOutput("mytable1")),
                tabPanel("mtcars", DT::dataTableOutput("mytable2"))
                #h1("Explore datasets")
        )
 
    )
    
  )
  )
  
)


#)