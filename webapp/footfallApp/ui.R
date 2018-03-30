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
library(lubridate)

# historical_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/footfall_31_12_2016.csv", sep=",", head=TRUE)
# sample_footfall <- historical_footfall
# vchoices <- 1:ncol(sample_footfall)
# names(vchoices) <- names(sample_footfall)


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
                  
                  #useShinyjs(),
                  
                  sidebarMenu(
                    
                    menuItem( 
                      "FOOTFALL DASHBOARD", tabName ="dashboard", icon = icon("braille")),    #textOutput("headersTime"))#
                  
                    menuItem("Footfall Forecast (Settings)", tabName ="forecastSetting", 
                             
                      # sliderInput("n", "Number of points:",
                      #                    min = 10, max = 200, value = 50, step = 10),
                      #sidebarPanel(width = "100", skin = "blue",
                      #sliderInput("m", "Update current footfall in:", 5, 60, 30), #)
                      #adding slider to adjust the length (history) of footfall to view
                      #adding slider to view the length of footfall to predict
                      #sliderInput("q", "Length of footfall (to predict)", 0, 30, 1)
                      # value is always yyyy-mm-dd, even if the display format is different
                      dateInput("dateToPredict", "Show footfall forecast for:", value = Sys.Date(), min=Sys.Date(), max=Sys.Date() + 7, format = "dd/mm/yy")
                    ),
                    
                    menuItem("History and Forecast (trend)", tabName ="historyAndForecastSetting", 
                      
                      radioButtons("timeOftheDayInput", "Modify 'Time of the Day'",
                                   choices = c("Daytime", "Evening", "Night", "Whole Day"),
                                   selected = "Whole Day"),
                      
                      sliderInput("p", "Start date (months)", 0, 80, 24), #use calculation
                      
                      radioButtons("chartType", "Chart Type", 
                                   choices = c("Line", "Bar"),
                                   selected = "Line"),
                      
                      checkboxGroupInput("trendLine", "Add trend line", 
                                   c("Yes")),
                      
                      radioButtons("algorithm", "Change Forecast Algorithm", 
                                   choices = c("Random Forest", "XGBoost","Regression"),
                                   selected = "Random Forest")
                      
                    ),
                    
                    #Setting menu
                    menuItem("Settings", tabName = "settings", badgeLabel=textOutput("notify"), badgeColor= "green", icon=icon("cogs")),
                    
                    menuItem("View Raw Data", tabName = "rawdata", icon=icon("database")), 
                    
                    #sidebarPanel(id="tableCol", width = 13, skin="blue",
                    # conditionalPanel(
                    #   #sample_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/sample_Dataset/input_Dataset.csv", sep=",", head=TRUE),
                    #   'input.dataset === "diamonds"',
                    #   checkboxGroupInput("show_vars", "List of predictors:",
                    #                      names(diamonds), selected = names(diamonds))),
                    menuItem("Footfall details", tabName ="predictors", 
                             
                    conditionalPanel(
                      #historical_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/footfall_31_12_2016.csv", sep=",", head=TRUE),
                      historical_footCopy <- dataTableOutput('table'),
                      #print(head(historical_footCopy)),#sample_footfall <- historical_footfall,
                      #'input.dataset === "sample_footfall"',
                      #'
                      checkboxGroupInput("show_vars2", "List of predictors:",
                              c("Date", "Hour", "InCount"), selected = c("Date", "Hour", "InCount")))
                    
                      # checkboxGroupInput("show_vars2", "List of predictors:",
                      #                  names(historical_footCopy)[1:14], selected = names(historical_footCopy)[1:14]))
                      # 
                     #checkboxGroupInput("show_vars2", "List of predictors:",
                                        #c("Date"), selected = c("Date")))
                    # 
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
                    )
                        
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
                                          actionButton("hourlyId", tags$b("19%, from"),
                                                       icon=icon("arrow-circle-down"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4(textOutput("dateOnPredictionBoard1"))) ), 
                                width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI"), 
                                plotOutput("morning_footfall", width = "100%", height = "50px")

                              ),
                              
                              box(
                                title = p(tags$h4(tags$b("Evening (6pm-9pm)")), tags$h4(textOutput("tomorrowDay_2")),
                                          tags$b(tags$h1(textOutput("lastDayCount"))),
                                          tags$head(tags$style("#lastDayCount{font-size:60px; font-family: Georgia}")), #Georgia, 
                                          actionButton("hourlyId", tags$b("23%, from"),
                                                       icon=icon("arrow-circle-up"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4(textOutput("dateOnPredictionBoard2"))) ), 
                                width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI2"), 
                                plotOutput("afternoon_footfall", width = "100%", height = "50px")
                                
                              ),
                              
                              box(
                                title = p(tags$h4(tags$b("Night (9pm-8am)")), tags$h4(textOutput("tomorrowDay_3")),
                                          tags$b(tags$h1(textOutput("lastWeekCount"))),
                                          tags$head(tags$style("#lastWeekCount{font-size:60px; font-family: Georgia}")),  
                                          actionButton("hourlyId", tags$b("43%, from"),
                                                       icon=icon("arrow-circle-up"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4(textOutput("dateOnPredictionBoard3"))) ),  
                                width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI3"), 
                                plotOutput("evening_footfall", width = "100%", height = "50px")
                                
                              ),
                              
                              box(
                                title = p(tags$h4(tags$b("Whole Day")), tags$h4(textOutput("tomorrowDay_4")),
                                          tags$b(tags$h1(textOutput("lastWeekCounty"))),
                                          tags$head(tags$style("#lastWeekCounty{font-size:60px; font-family: Georgia}")), #Georgia, 
                                          actionButton("hourlyId", tags$b("23%, from"),
                                                       icon=icon("arrow-circle-up"),
                                                       class = "btn-xs", title = "Update"), tags$b(tags$h4(textOutput("dateOnPredictionBoard4"))) ), 
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
                                title = "Accuracy summary of algorithms"
                              ),
                              
                              box(
                                width = 4, status = "primary", solidHeader = FALSE,
                                title = "Predictors (Importance)"
                              ),
                              
                              box(
                                width = 4, status = "primary", solidHeader = TRUE,
                                title = "Map of City of Leeds (Inset: City Central)",
                                leafletOutput("mapLeeds", height=400)

                              )
   
                    
                    )
                    ),       
                              
                    tabItem(tabName = "settings",
                            
                            # print(DT::dataTableOutput("historical_Foot")),
                            #print(textOutput("lengthOfMissing")),
                            # Only show this panel if there are missing historical data
                            fluidRow(
                              
                              tabBox(width = 13, height = 800,
                                     tabPanel(title = "Missing Dates", status = "warning", solidHeader = T, background = "aqua",
                                              
                                              
                                              #tabBox(width = 13, height = 800,
                                              #tabPanel(title = "Last 1 month 'Temperature' Information", status = "warning", solidHeader = T, background = "aqua",
                                              box(
                                                title = "List of missing dates",
                                                #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                #tags$b(tags$h4("vs. 7,140 (prev)"))
                                                tabPanel("missedFootfall", DT::dataTableOutput("missed_Foot")),
                                                #),
                                                width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI15"),
                                                ##plotOutput("temp_patterns", width = "320%", height = "150px")
                                                "  ",
                                                htmlOutput("testHTML1"),
                                                textOutput("text2"),
                                                textOutput("testHTML3"),
                                                textOutput("testHTML4"),
                                                textOutput("text5"),
                                                textOutput("text6"),
                                                textOutput("text7"),
                                                textOutput("text8"),
                                                textOutput("text9"),
                                                textOutput("text10")
                                                # p("<b>Above table shows the list of date ranges in which footfall data are missing."),
                                                # p("Search for the missing data from either of the following sources:"),
                                                # p("1. https://datamillnorth.org/dataset/leeds-city-centre-footfall-data"),
                                                # p("2. https://data.gov.uk/dataset/leeds-city-centre-footfall-data"),
                                                # p("Note: Ensure that the file to be uploaded contains the following three columns:"),
                                                # p("(a) 'Date' - in either of these formats: 'dd/mm/yyyy' OR 'yyyy-mm-dd'"),
                                                # p("(b) 'Hour' - 'Hour of the day', i.e. 0, 1, 2, .... 23."),
                                                # p("(c) 'InCount' - Hourly aggregate of footfall count"),
                                                # p("Upload a .csv file to update the database")
                                              ),
                                              
                                              fileInput('file1', 'Choose file to upload',
                                                        accept = c(
                                                          'text/csv',
                                                          'text/comma-separated-values',
                                                          'text/tab-separated-values',
                                                          'text/plain',
                                                          '.csv',
                                                          '.tsv'
                                                        )
                                              ),
                                              
                                              #button to append an uploaded file..
                                              #fluidPage(
                                              useShinyjs(),
                                              fluidRow(column(1, align="center", offset = 0, actionButton("upload", "Upload..")))
                                               # textInput("element", "Watch what happens to me")
                                             # )
                                              
                                     ),
                                     

                                     tabPanel(title = "view the uploaded data", status = "warning", solidHeader = T, background = "aqua",
                                              id='gaps_missingData',
                                              box(
                                                tabPanel("sample_footfall", DT::dataTableOutput("gaps"))
                                                #tabPanel("mtcars", DT::dataTableOutput("mytable2")),
                                                #tabPanel("iris", DT::dataTableOutput("mytable3"))
                                              )
                                              
                                             
                                     ),
                                     
                                     tags$hr() # 
                                     
                                     
                                     
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
                                           title = p(tags$h4(tags$b("Holidays"))
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     #tags$b(tags$h4("vs. 7,140 (prev)"))
                                                     ),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI5"),
                                           plotOutput("temp_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Last 1 month 'Rainfall' Information", status = "warning", solidHeader = T, background = "red",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays"))
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     #tags$b(tags$h4("vs. 7,140 (prev)"))
                                                     ),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI6"),
                                           plotOutput("rainfall_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Last 1 month Wind Information", status = "primary", solidHeader = T, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays"))
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     #tags$b(tags$h4("vs. 7,140 (prev)"))
                                                     ),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI7"),
                                           plotOutput("wind_patterns", width = "320%", height = "150px")
                                           
                                         )),
                                
                                tabPanel(title = "Last 1 month 'Humidity' Information", status = "primary", solidHeader = T, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays"))
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     #tags$b(tags$h4("vs. 7,140 (prev)"))
                                                     ),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI8"),
                                           plotOutput("humidity_patterns", width = "320%", height = "150px")
                                           
                                         )),
                               
                                 tabPanel(title = "Last 1 month Holiday information", status = "primary", solidHeader = F, background = "aqua",
                                         box(
                                           title = p(tags$h4(tags$b("Holidays"))
                                                     #tags$head(tags$style("Footfall Count (hours)"{font-size:80px; font-family: Georgia}")), #Georgia
                                                     #tags$b(tags$h1(textOutput("lastHourCount"))),
                                                     #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                     #tags$b(tags$h4("vs. 7,140 (prev)"))
                                                     ),
                                           width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI9"),
                                           plotOutput("holidays", width = "320%", height = "150px")

                                         ) )
                                
                              )
                              
                            )
                            
                    )
                    
                         
                
                   # )
                    
                    
                  )
                )
                
  )
  
)

