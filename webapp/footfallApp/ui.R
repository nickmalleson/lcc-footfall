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
library(shinyWidgets)
library(foreign)
library(shinyBS)


#for progressbar animation
jscode <- "
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
                      dateInput("dateToPredict", "Show footfall forecast for:", value = Sys.Date(), min=Sys.Date(), max=Sys.Date() + 7, format = "dd/mm/yy"),
                      
                      radioButtons("algorithm", "Change Forecast Algorithm", 
                                   choices = c("Random Forest", "XGBoost","Regression"),
                                   selected = "Random Forest")
                      
                    ),
                    
                    menuItem("History and Forecast (trend)", tabName ="historyAndForecastSetting", 
                      
                      radioButtons("timeOftheDayInput", "Modify Time of the Day",
                                   choices = c("Daytime", "Evening","Night", "Whole Day"),
                                   selected = "Whole Day"),
                      
                      checkboxInput("trendLine", label="Add trend line?", value = FALSE),
                      
                      
                      sliderInput("earliestDate", "Plot of last x-years", min=0, max=200, value=0, step=1), #use calculation 
                      
                      radioButtons("chartType", "Chart Type", 
                                   choices = c("Line", "Bar", "Dot"),
                                   selected = "Line"),
                      
                      
                      checkboxInput("prediction", label="Show prediction?", value = FALSE)
                      
                    ),
                    
                    #Setting menu
                    #menuItem("View Raw Data", tabName = "rawdata", icon=icon("database")),
                    
                    menuItem("Data Settings", tabName = "settings", badgeLabel=textOutput("notify"), badgeColor= "green", icon=icon("database")) #cogs
                    


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
                                title = p(tags$h4(tags$b("24-Hours")), tags$h4(textOutput("tomorrowDay_4")),
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
                                       box(width = 12, height = "350px",
                                         title = p(tags$h4(tags$b("Footfall information is not 'up-to-date!"))
                                                   #tags$style("MORE TO TALK ABOUT"{font-size:80px; font-family: Georgia}")),
                                                   ##tags$b(tags$h1(textOutput("lastHourCount"))),
                                                   #tags$head(tags$style("#lastHourCount{font-size:80px; font-family: Georgia}")), #Georgia,
                                                   #tags$b(tags$h4("Go to 'Settings' page..."))
                                                   ),
                                         solidHeader = FALSE, status = "primary", uiOutput("boxContentUI10"), 
                                         plotOutput("footfall_history", width = "100%", height = "250px")
                                         
                                       ))
      
                            ),
                            

                            # fluidRow(
                            #   box(
                            #     width = 4, status = "primary", solidHeader = FALSE,
                            #     title = "Accuracy summary of algorithms"
                            #   ),
                              
                              # fluidRow(
                              #   box(
                              #     width = 4, status = "primary", solidHeader = FALSE,
                              #     title = "Accuracy summary of algorithms"
                              #   ),
                             # tabItem(tabName = "rawdata",
                                      
                                      
                               fluidRow(
                                   tabBox(width = 13, height = 800,
 
                                               tabPanel(title = "HF DayTime Aggre.", status = "warning", solidHeader = T, background = "aqua",
                                                        id='dayTime',
                                                        box(
                                                          tabPanel("dayTime_data", DT::dataTableOutput("dayTimeData")),
                                                          br(),
                                                          "Remarks on the 'Outlier' column: '0' - 'missing'; '1' - 'Outlier'; '2' - 'valid'"
                                                          
                                                        )
                                                        
                                               ),
                                               
                                               tabPanel(title = "HF EveningTime Aggre.", status = "warning", solidHeader = T, background = "aqua",
                                                        id='eveningTime',
                                                        box(
                                                          tabPanel("eveningTime_data", DT::dataTableOutput("eveningTimeData")),
                                                          br(),
                                                          "Remarks on the 'Outlier' column: '0' - 'missing'; '1' - 'Outlier'; '2' - 'valid'"
                                                        )
                                                        
                                               ),
                                               
                                               tabPanel(title = "HF NightTime Aggre.", status = "warning", solidHeader = T, background = "aqua",
                                                        id='nightTime',
                                                        box(
                                                          tabPanel("nightTime_data", DT::dataTableOutput("nightTimeData")),
                                                          br(),
                                                          "Remarks on the 'Outlier' column: '0' - 'missing'; '1' - 'Outlier'; '2' - 'valid'"
                                                          
                                                        )
                                                        
                                               ),
                                               
                                               tabPanel(title = "HF 24-Hour Aggre.", status = "warning", solidHeader = T, background = "aqua",
                                                        id='twentyfourHour',
                                                        box(
                                                          tabPanel("twentyFourHours_data", DT::dataTableOutput("twentyFourHoursData")),
                                                          br(),
                                                          "Remarks on the 'Outlier' column: '0' - 'missing'; '1' - 'Outlier'; '2' - 'valid'"

                                                        )
                                                        
                                               ),
                                          
                                          box(
                                            width = 6, status = "primary", solidHeader = TRUE,
                                            title = "Boundary of City of Leeds (Inset: City Central)",
                                            leafletOutput("mapLeeds", height=500)
                                            
                                          )
                                               
                                        )
                                        
                                      )
                                      
                              # box(
                              #   width = 4, status = "primary", solidHeader = FALSE,
                              #   title = "Predictors (Importance)"
                              # ),
                            
   
                    
                    ),
   
                    tabItem(tabName = "settings",
                            
                            # print(DT::dataTableOutput("historical_Foot")),
                            #print(textOutput("lengthOfMissing")),
                            # Only show this panel if there are missing historical data
                            fluidRow(
                              
                              tabBox(width = 13, height = 800,
                                     
                                     tabPanel(title = "Parameters", status = "warning", solidHeader = T, background = "aqua",
                                              id='basic parameters',
                                              box(tags$p(tags$b(h4("Names of Camera Location"))),  tags$hr(), # ,#"From: 'Most recent' to 'Earliest'",
                                                  tabPanel("basic_parameters", DT::dataTableOutput("list_of_cameraNames"))
                                              )
                                              ),
                                     
                                     tabPanel(title = "Historical Footfall (HF)", status = "warning", solidHeader = T, background = "aqua",
                                              id='gaps_missingData',
                                              box(tags$p(tags$b(h4("Existing raw HF dataset"))),  tags$hr(), # ,#"From: 'Most recent' to 'Earliest'",
                                                tabPanel("history_footfall", DT::dataTableOutput("history")),
                                                #tabPanel("mtcars", DT::dataTableOutput("mytable2")),
                                                #tabPanel("iris", DT::dataTableOutput("mytable3"))
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
                                                
                                                tags$hr(),
                                                
                                                #actionButton("go", "Compute"),
                                                #fluidRow(column(1, align="center", offset = 0, 
                                                tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-grid-text, .irs-grid-pol, .irs-slider {visibility:hidden !important;}'))),
                                                useShinyjs(), extendShinyjs(text = jscode),
                                                #numericInput("seconds", "how many seconds your calculation will last?", value=6),
                                                uiOutput("processingbar3"), 
                                                
                                                #htmlOutput("processing"),
                                                
                                                tags$hr(), # 
                                                htmlOutput("issues_1"),
                                                textOutput("fields_absent_1"),
                                                #textOutput("fall_outside_daterange2"),
                                                #textOutput("date_Overlapping2"),
                                                textOutput("timeFormatWrong_1"),
                                                textOutput("typo_camera_Name_1"),
                                                tags$hr(), # 
                                                htmlOutput("resolve_issue_1"),
                                                htmlOutput("Uploaded_file_checks_Passed_1"),
                                                
                                                #div(style="display:inline-block",submitButton("aggre_HF"), style="float:right"),
                                                #div(style="display:inline-block",submitButton("aggre_HF")', 'Download Data'), style="float:right")
                                                #column(width=3,
                                                actionButton("aggre_HF", "Re-generate aggregated HF", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                actionButton("aggre_HF_confirm", "Continue", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                verbatimTextOutput("default"),
                                                tags$hr(),
                                                htmlOutput("taskCompleted"),
                                                
                                                # bsModal("gen_aggre","This process may take several hours to complete!....takes hours!","aggre_HF", 
                                                #         HTML(paste("")),
                                                #         tags$head(tags$style("#gen_aggre .modal-footer{ display:none}"))),
                                                        
                                                

                                                #fluidRow(column(1, align="center", offset = 0, 
                                              
                                                #actionButton("aggre_HF_cancel", "Cancel", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                 #),
                                                br(),
                                                
                                                #),
                                            
                                                
                                                #verbatimTextOutput("console"),
                                                # fluidPage(
                                                # shinyjs::useShinyjs(),
                                                # actionButton("btn","Click me"),
                                                # textOutput("text")),
                                                
                                                #),
                                             hr()
 
                                              )
                                            #rm(list = ls())  
                                     ),
                                     #tab2
                                     tabPanel(title = "Update HF", status = "warning", solidHeader = T, background = "aqua",
                                              
                                              
                                              #tabBox(width = 13, height = 800,
                                              #tabPanel(title = "Last 1 month 'Temperature' Information", status = "warning", solidHeader = T, background = "aqua",
                                              box(
                                                title = "List of missing dates in the historical footfall database",
                                                tabPanel("missedFootfall", DT::dataTableOutput("missed_Foot")),
                                                #),
                                                width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI15"),
                                                ##plotOutput("temp_patterns", width = "320%", height = "150px")
                                                "  ",
                                                htmlOutput("msg"),
                                                
                                                htmlOutput("testHTML1"),
                                                textOutput("text2"),
                                                htmlOutput("testHTML3"),
                                                htmlOutput("testHTML4"),
                                                textOutput("text5"),
                                                textOutput("text6"),
                                                textOutput("text7"),
                                                textOutput("text8"),
                                                textOutput("text9"),
                                                textOutput("text10"),
                                                textOutput("text11")
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
                                              
                                              #actionButton("go", "Compute"),
                                              #fluidRow(column(1, align="center", offset = 0, 
                                                tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-grid-text, .irs-grid-pol, .irs-slider {visibility:hidden !important;}'))),
                                                useShinyjs(), extendShinyjs(text = jscode),
                                              #numericInput("seconds", "how many seconds your calculation will last?", value=6),
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
                                              fluidRow(column(1, align="center", offset = 0, 
                                                              actionButton("append", "Append records", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                              )),hr(),
                                              fluidRow(column(1, align="center", offset = 0, 
                                                        actionButton("generated_footfall_aggre_data", "Generate aggregated data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                       )),
                                                       #),
                                            
                                              
                                              tags$hr(), # 
                                              
                                              htmlOutput("msg_tableAppended"),
                                              
                                              htmlOutput("file_updated"),
                                              
                                              tags$hr(), # 
                                              
                                              box(
                                                title =  textOutput("table_after_append"),
                                                tabPanel("missedFootfallafterAppend", DT::dataTableOutput("missed_Foot_after_Append")),
                                                #),
                                                width = 4, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI16")
                                              
                                              ),
                                              
                                              tags$hr() #
      
                                              
                                              #fluidRow(column(1, align="center", offset = 0, 
                                                             # actionButton("InCount_aggre_files", "Generate aggregated data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                              #))
                                              

                                              
                                     ) #htmlOutput("testHTML1"),
                                    
 
                                     #tags$hr() # 
     
                             )
                            )###
                    )#setting end
                    

                   # )
                    
                    
                  ) #body
                )
                
  )
  
)

