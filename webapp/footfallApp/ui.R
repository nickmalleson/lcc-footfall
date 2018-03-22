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
  
     
  
  #frame 
  dashboardPage(title = "Demo App", skin = "green",
                
                #header of the app
                # dashboardHeader(
                #   title = "LCC Footfall Predictor",
                #   tags$li(class = "dropdown", tags$a((htmlOutput("headersTime"))))
                #   
                  dashboardHeader(title = 'LCC Footfall Predictor',
                                  
                                  #tags$li(class = "dropdown", tags$a((htmlOutput("headersTime")))),
                                  #tags$style(HTML(".man_made_class{color:#f2f205; text-align: center;}")),
                                 # div(style="text-align:center","This application is based on",br(), "Quandl Data")
                                  tags$li(class = "dropdown", style="text-align:bottom", tags$a((htmlOutput("headersLogo")))),
                                  
                                  tags$li(class = "dropdown",
                                          tags$a(href="https://en.wikipedia.org/wiki/University_of_Leeds", target="_blank", 
                                                 tags$img(height = "20px", alt="SNAP Logo", src="https://upload.wikimedia.org/wikipedia/en/a/a8/Logo_of_University_of_Leeds.png")
                                          )
                                  ) 
                                  
                  
                  #((htmlOutput("headersTime"))))
                ),
                
                dashboardSidebar(
                  
                  sidebarMenu(
                    
                    #sidebarSearchForm("searchText", "buttonSearch", "Search"),
                    
                    
                    menuItem("Dashboard", tabName ="dashboard", icon = icon("braille")),
                    
                      # 
                    
                      #adding a slider to specify how often should the predictions be updated
                      sliderInput("m", "Update current footfall in:", 5, 60, 30),
                    
                      #adding slider to adjust the length (history) of footfall to view
                      sliderInput("p", "Adjust footfall history (to View)", 0, 365, 30),
                    
                      #adding slider to view the length of footfall to predict
                      sliderInput("q", "Length of footfall (to predict)", 0, 30, 1),
                    

                    
                    #menuItem("Map", tabName = "map", icon=icon("map")),
                    
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
                  
                tags$style(HTML(".box-header{background:#FFFFFF; color:#000000; text-align:center; font-size:20px}")),
                #tag for icon
                  
                  tabItems(
                    tabItem(tabName = "dashboard",
  
                            #)
                            fluidRow(
                              
                              tags$head(
                                tags$style(HTML(".fa{font-size: 20px; }"))),
                              
                              
                              box(
                                title = p(tags$h4("Footfall Count (hours)"), tags$b(tags$h1(20 * 300)),
                                          actionButton("hourlyId", "",
                                                       icon=icon("arrow-circle-down"),
                                                       class = "btn-xs", title = "Update")
                                ), width = 4, solidHeader = FALSE, status = "warning", uiOutput("boxContentUI"),
                                
                                plotOutput("hour_footfall", width = "100%", height = "50px")
                                
                              ),
                              
                              box(
                                title = p(tags$h4("Footfall Count (days)"), tags$b(tags$h1(20 * 300)),
                                          actionButton("hourlyId", "",
                                                       icon=icon("arrow-circle-down"),
                                                       class = "btn-xs", title = "Update")
                                ), width = 4, solidHeader = FALSE, status = "warning", uiOutput("boxContentUI2"),
                                
                                plotOutput("daily_footfall", width = "100%", height = "50px")
                                
                              ),
                              
                              box(
                                title = p(tags$h4("Footfall Count (weeks)"), tags$b(tags$h1(20 * 300)),
                                          actionButton("hourlyId", "",
                                                       icon=icon("arrow-circle-down"),
                                                       class = "btn-xs", title = "Update")
                                ), width = 4, solidHeader = FALSE, status = "warning", uiOutput("boxContentUI3"),
                                
                                plotOutput("week_footfall", width = "100%", height = "50px")
                                
                              )
                              ),
                              
 
                            fluidRow(
                              box(
                                width = 8, status="primary", solidHeader = TRUE,
                                title = "Footfall history",
                                plotOutput("chart")),
                              
                              
                              # box(
                              #   width = 4, status = "info", solidHeader = TRUE,
                              #   title = "Map of City of Leeds",
                              #   plotOutput("chart2"))
                              
                              box(
                                width = 4, status = "primary", solidHeader = TRUE,
                                title = "Map of City of Leeds (Inset: City Central)",
                                leafletOutput("map_2", height=400)
                                
                              )
                            ),
                            
                            fluidRow(
                              #valueBoxOutput("currentCount", "Current footfall count", icon=icon("hourglass-3"), color = "yellow"),
                              # valueBoxOutput("currentCount"),
                              # valueBoxOutput("todayaverage"), #itemRequested
                              # valueBoxOutput("eventTimeRemaining") #eventTimeRemaining
                              
                              
                              # valueBoxOutput("currentCount"),
                              # valueBoxOutput("todayaverage"), #itemRequested
                              # valueBoxOutput("eventTimeRemaining") #eventTimeRemaining
                              
                              infoBox("1st", 10 * 2, icon = icon("arrow-circle-down")),
                              infoBox("2nd", 10 * 2, icon = icon("credit-card")),
                              infoBox("3rd", 10 * 2, icon = icon("credit-card")),
                              infoBox("4th", 10 * 2, icon = icon("credit-card"))
                            )
                            
                            
                          
                    
                    ),
                            
                              
                    
                    # tabItem(tabName = "map",
                    #     h1("City of Leeds, United Kingdom"),
                    #     fluidRow(
                    #       column(width = 12,
                    #              box(
                    #                width = NULL, solidHeader = TRUE,
                    #                leafletOutput("busmap", height=1000)
                    #              )
                    #              )  
                    #       #h1("Camera Location")
                    #     )
                    # ),
                    
                    tabItem(tabName = "rawdata",
                            tabPanel("diamonds", DT::dataTableOutput("mytable1")),
                            tabPanel("mtcars", DT::dataTableOutput("mytable2")),
                            
                            #h1("Explore datasets")
                            
                            fluidRow(
                              tabBox(
                                tabPanel(title = "Temperature", status = "primary", solidHeader = T, background = "aqua",
                                         plotOutput("histogram", width = "900px", height = "400px")),
                                tabPanel(title = "Rainfall rate", status = "warning", solidHeader = T, background = "red",
                                         "Use this controls to fine-tune your dashboard", br(), br(),
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