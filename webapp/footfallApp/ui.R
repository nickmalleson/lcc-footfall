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
library(scales)
library(DT)
library(shinyalert)
library(shinyjs)

sample_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/sample_Dataset/input_Dataset.csv", sep=",", head=TRUE)
vchoices <- 1:ncol(sample_footfall)
names(vchoices) <- names(sample_footfall)


# Define UI for application that ...
shinyUI(

# utd <- 1,
  
 
  #frame 
  dashboardPage(title = "Demo App", skin = "green",
  
                #   
                dashboardHeader(title = tags$b(tags$h3('Leeds Ftfall Counts')), 
                                  tags$li(class = "dropdown", 
                                         tags$p(tags$b(h3(textOutput("headersTime")))), #style="text-align:left", tags$p(tags$b(h3(textOutput("headersTime"))))
                                         tags$style("#headersTime{color: white;
                                          font-size: 15px; text-align: bottom; font-style: italic;
                                                               }")
                                          ),
                                tags$li(class = "dropdown", 
                                        tags$p(tags$b(h3(textOutput("headersTime2")))), #style="text-align:left", tags$p(tags$b(h3(textOutput("headersTime"))))
                                        tags$style("#headersTime2{color: white;
                                                   font-size: 25px; text-align: left; font-style: bold;
                                                   }")
                                          ),
                                
                                  tags$li(class = "dropdown",
                                          tags$a(href="https://en.wikipedia.org/wiki/University_of_Leeds", target="_blank",
                                                 tags$img(height = "20px", alt="SNAP Logo", src="https://upload.wikimedia.org/wikipedia/en/a/a8/Logo_of_University_of_Leeds.png")
                                          ))
                ),
                
                dashboardSidebar( 
                  
                  useShinyjs(),
                  
                  sidebarMenu(
                    
                    menuItem( 
                      "FOOTFALL DASHBOARD", tabName ="dashboard", icon = icon("braille")),    #textOutput("headersTime"))#
                  
                    menuItem("Footfall Forecast (Settings)", tabName ="forecastSetting", 
                      #sidebarPanel(width = "100", skin = "blue",
                      #sliderInput("m", "Update current footfall in:", 5, 60, 30), #)
                      #adding slider to adjust the length (history) of footfall to view
                      #adding slider to view the length of footfall to predict
                      #sliderInput("q", "Length of footfall (to predict)", 0, 30, 1)
                      # value is always yyyy-mm-dd, even if the display format is different
                      dateInput("dateToPredict", "Show estimated footfall for:", value = Sys.Date(), min=Sys.Date(), max=Sys.Date() + 7, format = "dd/mm/yy"),
                      tags$style(HTML(".dateToPredict {z-index:99999 !important;}"))),
                
                    
                    menuItem("History and Forecast (Settings)", tabName ="historyAndForecastSetting", 
                      sliderInput("p", "Modify data length (months)", 0, 80, 24), #use calculation
                      
                      radioButtons("timeOftheDayInput", "Modify 'Time of the Day'",
                                   choices = c("Daytime", "Evening", "Night", "Whole Day"),
                                   selected = "Whole Day"),
                      
                      radioButtons("chartType", "Chart Type", 
                                   choices = c("Line", "Bar"),
                                   selected = "Line"),
                      
                      checkboxGroupInput("trendLine", "Add trend line", 
                                   c("Yes")),
                      
                      checkboxGroupInput("add 4cast", "Add 4cast based on RF algorit.", 
                                         c("Yes")),
                      
                      sliderInput("q", "Length of future 4cast (d)", 0, 7, 0)
                    ),
                    
                    
                    menuItem("View predictors (datasets)", tabName = "rawdata", icon=icon("database")), 
                    
                    #sidebarPanel(id="tableCol", width = 13, skin="blue",
                    # conditionalPanel(
                    #   #sample_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/sample_Dataset/input_Dataset.csv", sep=",", head=TRUE),
                    #   'input.dataset === "diamonds"',
                    #   checkboxGroupInput("show_vars", "List of predictors:",
                    #                      names(diamonds), selected = names(diamonds))),
                    menuItem("View predictors", tabName ="predictors", 
                             
                    conditionalPanel(
                      sample_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/sample_Dataset/input_Dataset.csv", sep=",", head=TRUE),
                      #'input.dataset === "sample_footfall"',
                      checkboxGroupInput("show_vars2", "List of predictors:",
                                         names(sample_footfall)[1:14], selected = names(sample_footfall)[1:14]))
                    
                    # conditionalPanel(
                    #   #sample_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/sample_Dataset/input_Dataset.csv", sep=",", head=TRUE),
                    #   'input.dataset === "mtcars"',
                    #   helpText("Click the column header to sort a column")
                    # ),
                  #     conditionalPanel(
                  #       #sample_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/sample_Dataset/input_Dataset.csv", sep=",", head=TRUE),
                  #       'input.dataset === "iris"',
                  #       helpText("Display 5 records by default")
                  # )
                    #)
                    ),
                  
                  menuItem(" Settings", tabName = "settings", icon=icon("cogs")) 
                  )
                ),
                
  
                body <- dashboardBody(
                  
                tags$style(HTML(".box-header{background:#FFFFFF; color:#000000; text-align:center; font-size:20px}")),
                #tag for icon
                
                
                  tabItems(
                    tabItem(tabName = "dashboard",
  
                            #)
                            
                            # fluidPage(
                            # utd <- 1,
                            # if(utd == 1){
                            #   useShinyalert()
                            #   #actionButton("preview", "preview")
                            # }
                            # ),
                            
                            fluidRow(
                              
                              tags$head(
                                tags$style(HTML(".fa{font-size: 20px; }"))),

                              box(
                                title = p(tags$h4(tags$b("Daytime (8am-6pm)")), tags$h4(textOutput("tomorrowDay_1")), 
                                          tags$b(tags$h1(textOutput("lastHourCount"))),
                                          tags$head(tags$style("#lastHourCount{font-size:60px; font-family: Georgia}")), #Georgia, 
                                          actionButton("hourlyId", tags$b("19%"),
                                                       icon=icon("arrow-circle-down"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4("vs. 8,888 (today)"))), 
                                width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI"), 
                                plotOutput("morning_footfall", width = "100%", height = "50px")

                              ),
                              
                              box(
                                title = p(tags$h4(tags$b("Evening (6pm-9pm)")), tags$h4(textOutput("tomorrowDay_2")),
                                          tags$b(tags$h1(textOutput("lastDayCount"))),
                                          tags$head(tags$style("#lastDayCount{font-size:60px; font-family: Georgia}")), #Georgia, 
                                          actionButton("hourlyId", tags$b("23%"),
                                                       icon=icon("arrow-circle-up"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4("vs. 4,787 (today)"))), 
                                width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI2"), 
                                plotOutput("afternoon_footfall", width = "100%", height = "50px")
                                
                              ),
                              
                              box(
                                title = p(tags$h4(tags$b("Night (9pm-8am)")), tags$h4(textOutput("tomorrowDay_3")),
                                          tags$b(tags$h1(textOutput("lastWeekCount"))),
                                          tags$head(tags$style("#lastWeekCount{font-size:60px; font-family: Georgia}")),  
                                          actionButton("hourlyId", tags$b("43%"),
                                                       icon=icon("arrow-circle-up"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4("vs. 4,475 (today)"))),  
                                width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI3"), 
                                plotOutput("evening_footfall", width = "100%", height = "50px")
                                
                              ),
                              
                              box(
                                title = p(tags$h4(tags$b("Whole Day")), tags$h4(textOutput("tomorrowDay_4")),
                                          tags$b(tags$h1(textOutput("lastWeekCounty"))),
                                          tags$head(tags$style("#lastWeekCounty{font-size:60px; font-family: Georgia}")), #Georgia, 
                                          actionButton("hourlyId", tags$b("23%"),
                                                       icon=icon("arrow-circle-up"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4("vs. 4,390 (today)"))), 
                                width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI4"), 
                                plotOutput("all_footfall", width = "100%", height = "50px")
                                
                              ) ),
                              
 
                            fluidRow(
                              # box(
                              #   width = 12, height = "300px", status="primary", solidHeader = FALSE,
                              #   title = "Footfall history",
                              #   plotOutput("chart"))
                              
                              tabPanel(title = "Footfall history", status = "primary", solidHeader = TRUE, 
                                       box(width = 12, height = "300px",
                                         title = p(tags$h4(tags$b("Footfall information is not 'up-to-date!")),
                                                   #tags$style("MORE TO TALK ABOUT"{font-size:80px; font-family: Georgia}")),
                                                   ##tags$b(tags$h1(textOutput("lastHourCount"))),
                                                   #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                   tags$b(tags$h4("Go to 'Settings' page..."))),
                                         solidHeader = FALSE, status = "primary", uiOutput("boxContentUI10"), 
                                         plotOutput("footfall_history", width = "100%", height = "150px")
                                         
                                       ))
      
                            ),
                            

                            fluidRow(
                              box(
                                width = 4, status = "primary", solidHeader = FALSE,
                                title = "Next holidays"
                              ),
                              
                              box(
                                width = 4, status = "primary", solidHeader = FALSE,
                                title = "Weather information"
                              ),
                              
                              box(
                                width = 4, status = "primary", solidHeader = TRUE,
                                title = "Map of City of Leeds (Inset: City Central)",
                                leafletOutput("map_2", height=400)

                              )
   
                    
                    )
                    ),       
                              
                    
                    tabItem(tabName = "rawdata",
                            
                            tabsetPanel(
                              id='dataset',
                            #tabPanel("diamonds", DT::dataTableOutput("mytable1")),
                            tabPanel("sample_footfall", DT::dataTableOutput("mytable1_1"))
                            #tabPanel("mtcars", DT::dataTableOutput("mytable2")),
                            #tabPanel("iris", DT::dataTableOutput("mytable3"))
                            ),
                            
                            fluidRow(
                              tabBox(width = 13, height = 800,
                                tabPanel(title = "Last 1 month 'Temperature' Information", status = "warning", solidHeader = T, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI5"),
                                           plotOutput("temp_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Last 1 month 'Rainfall' Information", status = "warning", solidHeader = T, background = "red",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI6"),
                                           plotOutput("rainfall_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Last 1 month Wind Information", status = "primary", solidHeader = T, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI7"),
                                           plotOutput("wind_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Last 1 month 'Humidity' Information", status = "primary", solidHeader = T, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI8"),
                                           plotOutput("humidity_patterns", width = "320%", height = "150px")
                                           
                                         )),
                               
                                 tabPanel(title = "Last 1 month Holiday information", status = "primary", solidHeader = F, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays")),
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     tags$b(tags$h4("vs. 7,140 (prev)"))),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI9"),
                                           plotOutput("holidays", width = "320%", height = "150px")

                                         ) )
                                
                              )
                              
                            )
                            
                    ),
                    
                    tabItem(tabName = "settings",
                            
                            
                            fluidRow(
   
                                     
                              )
                              
                            )
                
                   # )
                    
                    
                  )
                )
                
  )
  
)
