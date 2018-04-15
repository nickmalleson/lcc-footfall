#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#install.packages("swirl", repos="http://cran.rstudio.com/", dependencies=TRUE)

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
library(shinyWidgets)
library(foreign)
library(shinyBS)
library(emojifont)
require("ggrepel")
library(maps)
library(owmr)
library(data.table)
library(dplyr)


#for progressbar animation
jscode <- "
shinyjs.play = function() {
$('.slider-animate-button').trigger('click');
}
"

jscode1 <- "
shinyjs.play = function() {
$('.slider-animate-button').trigger('click');
}
"

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
                  
                  #tags$style(HTML(".box-header{background:#FFFFFF; color:#000000; text-align:center; font-size:40px}")),
                  #tag f
                  #useShinyjs(),
                  
                  sidebarMenu(
                    
                    
                    
                    menuItem( 
                      tags$b('FOOTFALL DASHBOARD'), tabName ="dashboard", icon = icon("braille")),    #textOutput("headersTime"))#
                  
                    menuItem(div(style="text-align:center","~ Forecast (Settings)"), tabName ="forecastSetting" 
                             
                      # sliderInput("n", "Number of points:",
                      #                    min = 10, max = 200, value = 50, step = 10),
                      #sidebarPanel(width = "100", skin = "blue",
                      #sliderInput("m", "Update current footfall in:", 5, 60, 30), #)
                      #adding slider to adjust the length (history) of footfall to view
                      #adding slider to view the length of footfall to predict
                      #sliderInput("q", "Length of footfall (to predict)", 0, 30, 1)
                      # value is always yyyy-mm-dd, even if the display format is different
                      
                      # radioButtons("forecast_chartType", "Chart Type", 
                      #              choices = c("Line-Dot", "Line"),
                      #              selected = "Line"),
                      
                      # sliderInput("day_ahead", "Days ahead to forecast", 1, 5, 3), #)
                      # 
                      # dateInput("dateToPredict", "Select Date to forecast for:", value = Sys.Date(), min=Sys.Date(), max=Sys.Date() + 7, format = "dd/mm/yy")
                      
                      # radioButtons("algorithm", "Change Forecast Algorithm", 
                      #              choices = c("Random Forest", "XGBoost","Regression"),
                      #              selected = "Random Forest")
                      
                    ),
                    
                    menuItem(div(style="text-align:center","~ History (Settings)"), tabName ="historyAndForecastSetting", 
                      
                      radioButtons("chartType", "Chart Type", 
                                          choices = c("Dot", "Line"),
                                          selected = "Line"),
                             
                      radioButtons("timeOftheDayInput", "Modify Time of the Day",
                                   choices = c("Daytime", "Evening","Night", "Whole Day"),
                                   selected = "Whole Day"),
                      
                      checkboxInput("trendLine", label="Add trend line?", value = FALSE),
                      
                      #checkboxInput("showOutliers", label="Show Outliers?", value = FALSE),
                      
                      sliderInput("earliestDate", "Plot of last x-years", min=0, max=200, value=0, step=1), #use calculation 
                      

                      checkboxInput("prediction", label="Show prediction?", value = FALSE)
                      
                    ),
                    
                    #Setting menu
                    #menuItem("View Raw Data", tabName = "rawdata", icon=icon("database")),
                    
                    # menuItem(tags$b('Data View'), tabName = "dataview", icon=icon("database")), #cogs
                    # #menuItem(tags$b('Data View'), tabName = "dataview", badgeLabel=textOutput("notify"), badgeColor= "green", icon=icon("database")), #cogs
                    
                    
                    menuItem(tags$b('Data Preview & Settings'), tabName = "settings", badgeColor= "green", icon=icon("cogs")) #cogs
                    


                    # menuItem("Footfall details", tabName ="predictors"
                             
                    # conditionalPanel(
                    # 
                    #   historical_footCopy <- dataTableOutput('history'),
                    # 
                    #   checkboxGroupInput("show_vars2", "List of predictors:",
                    #           c("Date", "Hour", "InCount"), selected = c("Date", "Hour", "InCount")))
# 
#                     )
                        
                           )
   
                ),
                
  
                body <- dashboardBody(
                  
                tags$style(HTML(".box-header{background:#FFFFFF; color:#000000; text-align:center; font-size:20px}")),
                #tag for icon
                
                
                  tabItems(
                    tabItem(tabName = "dashboard",

                            
                            fluidRow(
                              
                              tags$head(
                                tags$style(HTML(".fa{font-size: 20px; }"))),

                              box(tags$b(h4("Set Weather Conditions:")), "  ",
                                fluidRow(
                                  box(
                                    dateInput("dateToForecast", "Select Date to forecast for:", value = NULL, min=Sys.Date(), max=Sys.Date() + 60, format = "dd/mm/yy"),
                                    #),
                                    background="blue", width = 12, solidHeader = TRUE, status = "primary", uiOutput("boxContentUI33") 
                                  )
                                ),
                                fluidRow(
                                  box(
                                    #title = p(tags$h4(textOutput("today")), tags$h3(tags$b("Footfall Count:")),
                                    
                                    selectizeInput('temp_level', 'Temperature', choices = c("Very Low", "Low", "Moderate", "High")),
                                    column(4,
                                      htmlOutput("picture")
                                    ),
                                    background="blue", width = 12, solidHeader = TRUE, status = "primary", uiOutput("boxContentUI30") 
                                    
                                  )
                                ),
                                fluidRow( 
                                  box(
                                    #title = p(tags$h4(textOutput("today")), tags$h3(tags$b("Footfall Count:")),
                                    
                                    selectizeInput('rainfall_level', 'Rainfall', choices = c("None", "Light", "Moderate", "Heavy")),
                                    background="blue", width = 12, solidHeader = TRUE, status = "primary", uiOutput("boxContentUI31") 
                                  )
                                ),
                                
                                background="maroon", width = 2, solidHeader = TRUE, status = "primary", uiOutput("boxContentUI32") 
                              ), 
                              
                              tabPanel(title = "Footfall history", status = "primary", solidHeader = TRUE, 
                                       box(width = 10, height = "540px",
                                           title = p(tags$h4(tags$b("Historical Patterns and Trend of Footfall Data"))
                                           ),
                                           background="blue", solidHeader = FALSE, status = "primary", uiOutput("boxContentUI10"), 
                                           plotOutput("footfall_history", width = "100%", height = "430px")
                                           
                                       ))
                              
                              
                             #fluidRow(
                               
                             ),

                            fluidRow(
                              # box(
                              #   width = 12, height = "300px", status="primary", solidHeader = FALSE,
                              #   title = "Footfall history",
                              #   plotOutput("chart"))
                              
                              box(
                                width = 4, status = "primary", solidHeader = TRUE,
                                title = tags$b('Boundary of City of Leeds (Inset: City Central)'),
                                leafletOutput("mapLeeds", height=430)
                                
                              ),
                              
                              box(
                                width = 2, status = "primary", solidHeader = TRUE,
                                title = tags$b('Factors influencing Footfall Rate (ordered by Importance)')
                                #leafletOutput("mapLeeds", height=300)
                                
                              ),
                              #)
                              
                        #fluidRow(
                              box(title=tags$h4(tags$b("Next 5-days Footfall Patterns")),
                                  
                                  width = 6, solidHeader = FALSE, status = "primary",
                                  plotOutput("forecasted_footfall", width = "99%", height = "430px"))
                            )
      
                            ),

                    #),
                    
       #)   
                    #),
                    
                    tabItem(tabName = "settings",
                            
                            # print(DT::dataTableOutput("historical_Foot")),
                            #print(textOutput("lengthOfMissing")),
                            # Only show this panel if there are missing historical data
                            
                        box(title = tags$p(tags$b(h4("Preview of Footfall Data Aggregates"))),
                          tabBox(width = 12, height = 800,
                                 
                                 tabPanel(title = tags$b('HF DayTime Aggre.'), status = "warning", solidHeader = T, background = "aqua",
                                          id='dayTime',
                                          box(
                                            tabPanel("dayTime_data", DT::dataTableOutput("dayTimeData")),
                                            br(),
                                            "Remarks on the 'Outlier' column: '0' - 'missing'; '1' - 'Outlier'; '2' - 'valid'",
                                            width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI20")          
                                          )
                                          
                                 ),
                                 
                                 tabPanel(title = tags$b('HF EveningTime Aggre.'), status = "warning", solidHeader = T, background = "aqua",
                                          id='eveningTime',
                                          box(
                                            tabPanel("eveningTime_data", DT::dataTableOutput("eveningTimeData")),
                                            br(),
                                            "Remarks on the 'Outlier' column: '0' - 'missing'; '1' - 'Outlier'; '2' - 'valid'",
                                            width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI21") 
                                          )
                                          
                                 ),
                                 
                                 tabPanel(title = tags$b('HF NightTime Aggre.'), status = "warning", solidHeader = T, background = "aqua",
                                          id='nightTime',
                                          box(
                                            tabPanel("nightTime_data", DT::dataTableOutput("nightTimeData")),
                                            br(),
                                            "Remarks on the 'Outlier' column: '0' - 'missing'; '1' - 'Outlier'; '2' - 'valid'",
                                            width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI22") 
                                            
                                          )
                                          
                                 ),
                                 
                                 tabPanel(title = tags$b('HF 24-Hour Aggre.'), status = "warning", solidHeader = T, background = "aqua",
                                          id='twentyfourHour',
                                          box(
                                            tabPanel("twentyFourHours_data", DT::dataTableOutput("twentyFourHoursData")),
                                            br(),
                                            "Remarks on the 'Outlier' column: '0' - 'missing'; '1' - 'Outlier'; '2' - 'valid'",
                                            width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI23") 
                                            
                                          )
                                          
                                 )
                                 
                          )
                          ),
                          
                        box(title = tags$p(tags$b(h4("Parameters and Data Settings"))),
                            
                              tabBox(width = 12, height = 800,
                                    
                                     tabPanel(title = tags$b('Basic Inputs'), status = "warning", solidHeader = T, background = "aqua",
                                              id='basic parameters',
                                              
                                              #list of names of cameara location
                                              htmlOutput("cameraTitle"),
                                              #tags$hr(),
                                              htmlOutput("cameraLocation"),
                                              tags$hr(),
                                              htmlOutput("warning_cameraLocation")
                                     ),
                                     
                                     tabPanel(title = tags$b('Update Weather Info.'), status = "warning", solidHeader = T, background = "aqua",
                                              id='update_predictors',
                                              box(tags$p(tags$b(h4("List of dates with missing Weather information (Temperature and Rain intensity)"))),  tags$hr(), 
                                                  tabPanel("predict_Info", DT::dataTableOutput("missed_Pred_Info")),
                                                  width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI14"),
                                                  tags$hr(),
                                              htmlOutput("testHTML1_pred"),
                                              #htmlOutput("notify_pred"),
                                              htmlOutput("text2_pred"),
                                              htmlOutput("testHTML3_pred"),
                                              htmlOutput("testHTML4_pred")
                                              #htmlOutput("otherInfo_pred")
                                              ),
                                              
                                              tags$style(".shiny-file-input-progress {display: none}"),
                                              
                                              fileInput('file3', 'Choose file to upload',
                                                        accept = c(
                                                          'text/csv',
                                                          'text/comma-separated-values',
                                                          'text/tab-separated-values',
                                                          'text/plain',
                                                          '.csv',
                                                          '.tsv'
                                                        )),
                                              
                                              #processing bar for uploading file (historical)
                                              fluidPage(
                                                #tags$b("Loading..."), br(),
                                                progressBar(id = "pb3", value = 0)
                                              ),
                                              
                                              tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-grid-text, .irs-grid-pol, .irs-slider {visibility:hidden !important;}'))),
                                              useShinyjs(), extendShinyjs(text = jscode1),
                                              #numericInput("seconds", "how many seconds your calculation will last?", value=6),
                                              tags$hr(),
                                              #uiOutput("processingbar1"),
                                              
                                              #htmlOutput("processing"),
                                              htmlOutput("Uploaded_file_checks_Passed3"),
                                              tags$hr(), # 
                                              htmlOutput("issues3"),
                                              textOutput("fields_absent3"),
                                              textOutput("fall_outside_daterange3"),
                                              textOutput("date_Overlapping3"),
                                              textOutput("timeFormatWrong3"),
                                              #textOutput("typo_camera_Name3"),
                                              tags$hr(), # 
                                              htmlOutput("resolve_issue3"),
                                              useShinyjs(),
                                              htmlOutput("append_button_Descrip3"),
                                              useShinyjs(),
                                              #preview button
                                              #fluidRow(column(1, align="center", offset = 0, 
                                              actionButton("append_file3", "Update predictor Info.", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              #)),hr(),
                                              #fluidRow(column(1, align="center", offset = 0, 
                                              ###actionButton("confirm_Append", "Continue", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              #)),
                                              tags$hr(), # 
                                              htmlOutput("taskCompleted3"),
                                              tags$hr(),
                                              actionButton("train_Prediction_Model", "Re-train Prediction Model", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              htmlOutput("restart_app3")
                                              #),
                                              
                                              
                                              ##tags$hr(), # 
                                              
                                              ##htmlOutput("aggre_HF_file_updated3"),
                                              
                                              ##tags$hr(), # 
                                              
                                              ##htmlOutput("reload_HF_update3"),
                                              
                                              ##box(
                                                ##title =  textOutput("table_after_append3"),
                                                ##tabPanel("missedFootfallafterAppend", DT::dataTableOutput("missed_Foot_after_Append3")),
                                                #),
                                                ##width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI16")
                                                
                                              ##),
                                              
                                              ##tags$hr() #
                                            
                                            ),
                                     
                                     
                                     tabPanel(title = tags$b('Update HF records'), status = "warning", solidHeader = T, background = "aqua",
                                              #tabBox(width = 13, height = 800,
                                              #tabPanel(title = "Last 1 month 'Temperature' Information", status = "warning", solidHeader = T, background = "aqua",
                                              box(tags$p(tags$b(h4("List of missing dates in the historical footfall database"))),  tags$hr(), # ,#"F
                                                #box(tags$p(tags$b(h4("Existing raw HF dataset"))),  tags$hr(), # ,#"F
                                                tabPanel("missedFootfall", DT::dataTableOutput("missed_Foot")),
                                                #),
                                                width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI15"),
                                                ##plotOutput("temp_patterns", width = "320%", height = "150px")
                                                "  ",
                                                htmlOutput("msg"),
                                                
                                                htmlOutput("testHTML1"),
                                                textOutput("text2"),
                                                htmlOutput("testHTML3"),
                                                htmlOutput("testHTML4"),
                                                htmlOutput("otherInfo")
                                                # textOutput("text6"),
                                                # textOutput("text7"),
                                                # textOutput("text8"),
                                                # textOutput("text9"),
                                                # textOutput("text10"),
                                                # textOutput("text11")
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
                                              
                                              tags$style(".shiny-file-input-progress {display: none}"),
                                              
                                              fileInput('file1', 'Choose file to upload',
                                                        accept = c(
                                                          'text/csv',
                                                          'text/comma-separated-values',
                                                          'text/tab-separated-values',
                                                          'text/plain',
                                                          '.csv',
                                                          '.tsv'
                                                        )),
                                              
                                              #processing bar for uploading file (historical)
                                              fluidPage(
                                                #tags$b("Loading..."), br(),
                                                progressBar(id = "pb1", value = 0)
                                              ),
                                              
                                              #actionButton("go", "Compute"),
                                              #fluidRow(column(1, align="center", offset = 0, 
                                                tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-grid-text, .irs-grid-pol, .irs-slider {visibility:hidden !important;}'))),
                                                useShinyjs(), extendShinyjs(text = jscode1),
                                                #uiOutput("processingbar1"),
                                                tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-grid-text, .irs-grid-pol, .irs-slider {visibility:hidden !important;}'))),
                                                useShinyjs(), extendShinyjs(text = jscode1),
                                                #numericInput("seconds", "how many seconds your calculation will last?", value=6),
                                                tags$hr(),
                                                uiOutput("processingbar1"),
                                                                                   
                                              #htmlOutput("processing"),
                                              htmlOutput("Uploaded_file_checks_Passed"),
                                              tags$hr(), # 
                                              htmlOutput("issues"),
                                              textOutput("fields_absent"),
                                              textOutput("fall_outside_daterange"),
                                              textOutput("date_Overlapping"),
                                              textOutput("timeFormatWrong"),
                                              textOutput("typo_camera_Name"),
                                              tags$hr(), # 
                                              htmlOutput("resolve_issue"),
                                              useShinyjs(),
                                              htmlOutput("append_button_Descrip"),
                                              useShinyjs(),
                                              #preview button
                                              #fluidRow(column(1, align="center", offset = 0, 
                                                              actionButton("append", "Append records", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                              #)),hr(),
                                              #fluidRow(column(1, align="center", offset = 0, 
                                                              actionButton("confirm_Append", "Continue", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              #)),
                                              tags$hr(), # 
                                              htmlOutput("confirm_Append"),
                                                       #),
                                            
                                              
                                              tags$hr(), # 
                                              
                                              htmlOutput("aggre_HF_file_updated"),
                                              
                                              tags$hr(), # 
                                              
                                              htmlOutput("reload_HF_update"),
                                              
                                              box(
                                                title =  textOutput("table_after_append"),
                                                tabPanel("missedFootfallafterAppend", DT::dataTableOutput("missed_Foot_after_Append")),
                                                #),
                                                width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI16")
                                              
                                              ),
                                              
                                              tags$hr() #
      
                                              
                                              #fluidRow(column(1, align="center", offset = 0, 
                                                             # actionButton("InCount_aggre_files", "Generate aggregated data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                              #))
                                              

                                              
                                     ), 
                                     
                                     tabPanel(title = tags$b('Replace all HF records'), status = "warning", solidHeader = T, background = "aqua",
                                              id='gaps_missingData',
                                              box(
                                                tags$p(tags$b(h4("Existing raw HF dataset"))),  tags$hr(), # ,#"From: 'Most recent' to 'Earliest'",
                                                  tabPanel("history_footfall", DT::dataTableOutput("history")),
                                                  tags$hr(),
                                                  htmlOutput("HF_view"),
                                                  htmlOutput("HF_directory"),
                                                  
                                                  tags$hr(),
                                                  htmlOutput("why_re_gen_HF"),
                                                  htmlOutput("why_re_gen_HF2"),
                                                  tags$hr(),
                                                  htmlOutput("regen_HF_warning"),
                                                  tags$hr(),
                                                  
                                                  tags$style(".shiny-file-input-progress {display: none}"),
                                                  
                                                  fileInput('file2', 'Upload new raw footfall dataset to replace the existing historical footfall data (Max. size: 200MB)',
                                                            accept = c(
                                                              'text/csv',
                                                              'text/comma-separated-values',
                                                              'text/tab-separated-values',
                                                              'text/plain',
                                                              '.csv',
                                                              '.tsv'
                                                            )),
                                                  
                                                  #processing bar for uploading file (historical)
                                                  fluidPage(
                                                    #tags$b("Loading..."), br(),
                                                    progressBar(id = "pb2", value = 0)
                                                  ),
                                                  
                                                  htmlOutput("aggre_HF_processing"),
                                                  
                                                  tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-grid-text, .irs-grid-pol, .irs-slider {visibility:hidden !important;}'))),
                                                  useShinyjs(), extendShinyjs(text = jscode),
                                                  #numericInput("seconds", "how many seconds your calculation will last?", value=6),
                                                  tags$hr(),
                                                  uiOutput("processingbar2"), 
                                                  htmlOutput("processing_append"),
                                                  
                                                  #htmlOutput("processing"),
                                                  #progressbar to upload file
                                                  
                                                  tags$hr(), # 
                                                  htmlOutput("issues_1"),
                                                  textOutput("fields_absent_1"),
                                                  textOutput("timeFormatWrong_1"),
                                                  textOutput("typo_camera_Name_1"),
                                                  tags$hr(), # 
                                                  htmlOutput("resolve_issue_1"),
                                                  htmlOutput("Uploaded_file_checks_Passed_1"),
                                                  tags$hr(), # 
                                                  actionButton("aggre_HF", "Generate aggregated HF", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                  actionButton("aggre_HF_confirm", "Continue", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                  verbatimTextOutput("default"),
                                                  tags$hr(),
                                                  htmlOutput("taskCompleted"),
                                                  tags$hr(),
                                                  htmlOutput("data_aggre_dir"),
                                                  htmlOutput("reload_HF"),
                                                  
                                                  br(),
                                                  
                                                  #),
                                                  
                                                  
                                                  #verbatimTextOutput("console"),
                                                  # fluidPage(
                                                  # shinyjs::useShinyjs(),
                                                  # actionButton("btn","Click me"),
                                                  # textOutput("text")),
                                                  
                                                  #),
                                                  hr(),
                                                width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI17") 
                                              )
                                              #rm(list = ls())  
                                     )
                                     #tab2
                                     ##htmlOutput("testHTML1"),
                                    
 
                                     #tags$hr() # 
     
                             )
                        )
                                 
                                 
                                     
                          #) ##################
                          #),
                          
                          
                          #fluidRow(
                            #)###
                    )#setting end
                    

                   # )
                    
                    
                  ) #body
                )
                
  )
  
)




#  #tabPanel(title = "....", status = "primary", solidHeader = TRUE,  
#   box(
#     title = p(tags$h4(tags$b("Next 5 day/Daily Forecast")), 
#     width = 6, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI2"),
#     plotOutput("afternoon_footfall", width = "120%", height = "250px")
#     
#width = 8, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI12")  # ),
# )),
#), 

# box(
#   title = p(tags$h4(tags$b("Night (9pm-8am)")), tags$h4(textOutput("tomorrowDay_3")),
#             tags$b(tags$h1(textOutput("lastWeekCount"))),
#             tags$head(tags$style("#lastWeekCount{font-size:60px; font-family: Georgia}")),  
#             actionButton("hourlyId", tags$b("43%, from"),
#                          icon=icon("arrow-circle-up"),
#                          class = "btn-xs", title = "Update"), tags$b(tags$h4(textOutput("dateOnPredictionBoard3"))) ),  
#   width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI3"), 
#   plotOutput("evening_footfall", width = "100%", height = "50px")
#   
# )

# box(
#   title = p(tags$h4(tags$b("24-Hours")), tags$h4(textOutput("tomorrowDay_4")),
#             tags$b(tags$h1(textOutput("lastWeekCounty"))),
#             tags$head(tags$style("#lastWeekCounty{font-size:60px; font-family: Georgia}")), #Georgia, 
#             actionButton("hourlyId", tags$b("23%, from"),
#                          icon=icon("arrow-circle-up"),
#                          class = "btn-xs", title = "Update"), tags$b(tags$h4(textOutput("dateOnPredictionBoard4"))) ), 
#   width = 3, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI4"), 
#   plotOutput("all_footfall", width = "100%", height = "50px")
#   
# ) 
#),



