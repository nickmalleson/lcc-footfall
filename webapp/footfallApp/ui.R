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
                    # dropdownMenu(type = "message",
                    #              messageItem(from = "Finance update", message = "we are on threshold"),
                    #              messageItem(from = "Sales update", message = "Sales are at 55%", icon = icon("bar-chart"), time = "22:00"),
                    #              messageItem(from = "Sales update", message = "Sales meeting at 6PM on Monday", icon = icon("handshake-o"), time = "03-25-2017")
                    # )
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
      #sliderInput("bins","Number of breaks", 1, 50, 10),
      
      sidebarMenu(
      sidebarSearchForm("searchText", "buttonSearch", "Search"),
      menuItem("Dashboard", tabName ="dashboard", icon = icon("dashboard")), #align-left
      sliderInput("m", "Set time to update prediction", 5, 60, 10),
       #menuSubItem("Dashboard Finance", tabName="finance"),
       #menuSubItem("Analysis", tabName="analysis"),
       #menuSubItem("Dashboard sales", tabName="sales"), 
      #menuItem("Detailed Analysis", badgeLabel="New", badgeColor = "green"),
      menuItem("Raw Data", tabName = "rawdata", icon=icon("database")),
      menuItem("Map", tabName = "map", icon=icon("map"))
      #sliderInput("n", "Set time to update prediction", 5, 500, 50)
      ##plotOutput("histogram")
      #sliderInput("bins","Number of breaks", 1, 50, 10),
      #textInput("text_input", "Search Opportunities", value = "123456")

    )),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",

                fluidRow(
                  valueBox(15*200, "Budget for 15 days", icon=icon("hourglass-3")),
                  valueBoxOutput("itemRequested"),
                  valueBoxOutput("eventTimeRemaining")
                ),
 
                fluidRow(
                  #tabBox(
                  tabPanel(title = "Histogram of Faithful", status = "primary", solidHeader = T, background = "aqua",
                      plotOutput("histogram"))
                  
                  
                  
                  # tabPanel(title = "Controls for Dashboard", status = "warning", solidHeader = T, background = "red",
                  #          "Use this controls to fine-tune your dashboard", br(),br(),
                  #          "Do not use lot of control as it confuses the user",
                  #          sliderInput("bins","Number of breaks", 1, 50, 10),
                  #          textInput("text_input", "Search Opportunities", value = "123456")),
                  # tabPanel(title = "Map", status = "primary", solidHeader = T, background = "aqua")
                  #          #plotOutput("histogram"))
                #)
                  )), #end of tabitem 1
        tabItem(tabName = "finance",
               h1("Finance Dashboard")
                 ),
        tabItem(tabName = "sales",
               h2("sales Dashboard")
           )
    
      
    )
    
  )
  
)
)

