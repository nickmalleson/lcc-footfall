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
library(maptools)

Cleaned_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/Cleaned_Dataset/input_Dataset.csv", sep=",", head=TRUE)

# Define UI for application that ...
shinyUI(

  
  
  dashboardPage(title = "Demo App", #skin = "yellow"
    
    dashboardHeader(title = "LCC Footfall Predictor"),
    
    dashboardSidebar(

      sidebarMenu(
      sidebarSearchForm("searchText", "buttonSearch", "Search"),
      
      menuItem("Dashboard", tabName ="dashboard", icon = icon("braille")), 
      sliderInput("p", "Adjust footfall history (to View)", 0, 365, 30),
      sliderInput("q", "Length of footfall (to predict)", 0, 30, 1),
      sliderInput("m", "Update current footfall in:", 5, 60, 10),
      
      menuItem("Map", tabName = "map", icon=icon("map")),
      
      menuItem("View predictors (datasets)", tabName = "rawdata", icon=icon("database")), 
  
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
                      title = "10 most impactful factors on footfall level ",
                      plotOutput("chart2"))
                  )
        ),
                  
 
        tabItem(tabName = "map",
            h1("City of Leeds, United Kingdom"),
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL, solidHeader = TRUE,
                       leafletOutput("busmap", height=1000)
                     )
                     )  
              #h1("Camera Location")
            )
        ),
        
        tabItem(tabName = "rawdata",
                tabPanel("diamonds", DT::dataTableOutput("mytable1")),
                tabPanel("mtcars", DT::dataTableOutput("mytable2")),
        
                #h1("Explore datasets")
        
        fluidRow(
          tabBox(
            tabPanel(title = "Temperature", status = "primary", solidHeader = T, background = "aqua",
                     plotOutput("histogram", width = "900px", height = "400px")),
            tabPanel(title = "Rainfall rate", status = "warning", solidHeader = T, background = "red",
                     "Use this controls to fine-tune your dashboard", br(),br(),
                     "Do not use lot of control as it confuses the user",
                     sliderInput("bins","Number of breaks", 1, 50, 10),
                     textInput("text_input", "Search Opportunities", value = "123456")),
            tabPanel(title = "Wind", status = "primary", solidHeader = T, background = "aqua"),
            tabPanel(title = "Humidity", status = "primary", solidHeader = T, background = "aqua"),
            tabPanel(title = "Holidays", status = "primary", solidHeader = T, background = "aqua")
            
          )
          
        )
 
    )
    
  )
  )
  
)

)


#)