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
  
     
  
  #frame 
  dashboardPage(title = "Demo App", skin = "green",
                
                #header of the app
                # dashboardHeader(
                #   title = "Leeds CC Footfall Predictor",
                #   tags$li(class = "dropdown", tags$a((htmlOutput("headersTime"))))
                #   
                dashboardHeader(title = tags$b('LEEDS FOOTFALL'), 
                 #dashboardHeader(title = tags$p(class = "dropdown", style="text-align:left", tags$b('LEEDS FOOTFALL')),
                                  
                                  tags$li(class = "dropdown", style="text-align:left", tags$p(tags$b(h3(textOutput("headersTime"))))),
                               
                                  tags$li(class = "dropdown",
                                          tags$a(href="https://en.wikipedia.org/wiki/University_of_Leeds", target="_blank",
                                                 tags$img(height = "20px", alt="SNAP Logo", src="https://upload.wikimedia.org/wikipedia/en/a/a8/Logo_of_University_of_Leeds.png")
                                          ))
                                  
                                  
                  
                  #((htmlOutput("headersTime"))))
                ),
                
                dashboardSidebar( 
                  
                  sidebarMenu(
                    
                    #sidebarSearchForm("searchText", "buttonSearch", "Search"),
                    
                    
                    menuItem( 
                      
                      "DASHBOARD", tabName ="dashboard", icon = icon("braille")),    #textOutput("headersTime"))#
                  
                    menuItem("    Settings", tabName ="historySetting", 
                      #sidebarPanel(width = "100", skin = "blue",
                      sliderInput("m", "Update current footfall in:", 5, 60, 30), #)
                      #adding slider to adjust the length (history) of footfall to view
                      sliderInput("p", "Adjust footfall history (to View)", 0, 365, 30),

                      #adding slider to view the length of footfall to predict
                      sliderInput("q", "Length of footfall (to predict)", 0, 30, 1)
                      #),
                      ),
                    
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
                                tags$style(HTML(".fa{font-size: 15px; }"))),
                              
                              # box(
                              #   title = p(tags$h4("Footfall Count (hours)"), 
                              #             actionButton("hourlyId", "",
                              #                          icon=icon("arrow-circle-down"),
                              #                          class = "btn-xs", title = "Update")
                              #   ), width = 4, solidHeader = FALSE, status = "warning", uiOutput("boxContentUI"),
                              #   
                              #   plotOutput("hour_footfall", width = "100%", height = "50px")
                              #   
                              # ),
                              
                              box(
                                title = p(tags$h4(tags$b("Footfall Count (last hours)")),
                                          #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                          tags$b(tags$h1(textOutput("lastHourCount"))),
                                          tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia, 
                                          actionButton("hourlyId", tags$b("19%"),
                                                       icon=icon("arrow-circle-down"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4("vs. 7,140 (prev)"))), 
                                width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI"), 
                                plotOutput("hour_footfall", width = "100%", height = "50px")

                              ),
                              
                              box(
                                title = p(tags$h4(tags$b("Footfall Count (last days)")),
                                          tags$b(tags$h1(textOutput("lastDayCount"))),
                                          tags$head(tags$style("#lastDayCount{font-size:80px; font-family: Georgia}")), #Georgia, 
                                          actionButton("hourlyId", tags$b("23%"),
                                                       icon=icon("arrow-circle-up"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4("vs. 47,124 (prev)"))), 
                                width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI2"), 
                                
                                plotOutput("daily_footfall", width = "100%", height = "50px")
                                
                              ),
                              
                              box(
                                title = p(tags$h4(tags$b("Footfall Count (last weeks)")),
                                          tags$b(tags$h1(textOutput("lastWeekCount"))),
                                          tags$head(tags$style("#lastWeekCount{font-size:80px; font-family: Georgia}")), #Georgia, 
                                          actionButton("hourlyId", tags$b("43%"),
                                                       icon=icon("arrow-circle-up"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4("vs. 352,488 (prev)"))),  
                                width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI3"), 
                                
                                plotOutput("week_footfall", width = "100%", height = "50px")
                                
                              )
                              ),
                              
 
                            fluidRow(
                              box(
                                width = 8, height = "300px", status="primary", solidHeader = FALSE,
                                title = "Footfall history",
                                plotOutput("chart")),
                              

                              box(
                                width = 4, height = "300px", status = "primary", solidHeader = FALSE,
                                title = "Footfall forecast for the next x days",
                                plotOutput("forecast")
                                #leafletOutput("map_2", height=400)
                                
                              )
                            #)
                            ),
                            
                            
                          #tabItem(
                            fluidRow(
                              #valueBoxOutput("currentCount", "Current footfall count", icon=icon("hourglass-3"), color = "yellow"),
                              # valueBoxOutput("currentCount"),
                              # valueBoxOutput("todayaverage"), #itemRequested
                              # valueBoxOutput("eventTimeRemaining") #eventTimeRemaining
                              
                              
                              # valueBoxOutput("currentCount"),
                              # valueBoxOutput("todayaverage"), #itemRequested
                              # valueBoxOutput("eventTimeRemaining") #eventTimeRemaining
                              
                              box(
                                width = 4, status = "primary", solidHeader = FALSE,
                                title = "Next holidays"
                                #plotOutput("forecast")
                                #leafletOutput("map_2", height=400)
                                
                              ),
                              box(
                                width = 4, status = "primary", solidHeader = FALSE,
                                title = "Weather information"
                                #plotOutput("forecast")
                                #leafletOutput("map_2", height=400)
                                
                              ),
                              
                              box(
                                width = 4, status = "primary", solidHeader = TRUE,
                                title = "Map of City of Leeds (Inset: City Central)",
                                leafletOutput("map_2", height=400)

                              )
   
                    
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
                              tabBox(width = 13, height = 800,
                                tabPanel(title = "Temperature", status = "warning", solidHeader = T, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI4"),
                                           plotOutput("temp_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Rainfall rate", status = "warning", solidHeader = T, background = "red",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI5"),
                                           plotOutput("rainfall_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Wind", status = "primary", solidHeader = T, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI6"),
                                           plotOutput("wind_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Humidity", status = "primary", solidHeader = T, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI7"),
                                           plotOutput("humidity_patterns", width = "320%", height = "150px")
                                           
                                         )),
                               
                                 tabPanel(title = "Holidays", status = "primary", solidHeader = F, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI8"),
                                           plotOutput("holidays", width = "320%", height = "150px")

                                         ) )
                                
                              )
                              
                            )
                            
                    )
                    
                  )
                )
                
  )
  
)


#)

# box(
#   title = p(tags$h4(tags$b("Footfall Count (last hours)")),
#             #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
#             tags$b(tags$h1(textOutput("lastHourCount"))),
#             tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia, 
#             actionButton("hourlyId", tags$b("19%"),
#                          icon=icon("arrow-circle-down"),
#                          class = "btn-xs", title = "Update"), tags$b(tags$h4("vs. 7,140 (prev)"))), 
#   width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI"), 
#   plotOutput("hour_footfall", width = "100%", height = "50px")
#   
# )
