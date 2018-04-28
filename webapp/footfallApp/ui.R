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
library(randomForest)
library(ROpenWeatherMap)
require(reshape2)

#api key for downloading weather forecast...(http://openweathermap.org/)
api_key="c8a930a2e30b695551e57d375a67d76e"

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


#----------------------------------------------------
# Designing UI layout
#----------------------------------------------------
shinyUI(

  #Title bar
  dashboardPage(title = "Demo App", skin = "green",
  
                 dashboardHeader(title = tags$b(tags$h3('Leeds Ftfall Pred')), 
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
                
                #----------------------------------------------------
                # Designing the content of the sidebar
                #----------------------------------------------------
                dashboardSidebar( 
                  
                  sidebarMenu(

                    menuItem( 
                      tags$b('FOOTFALL DASHBOARD'), tabName ="dashboard", icon = icon("braille")),    #textOutput("headersTime"))#
                  
                      menuItem(div(style="text-align:center","~ History (Settings)"), tabName ="historyAndForecastSetting", 
                      
                      radioButtons("chartType", "Chart Type", 
                                          choices = c("Dot", "Line"),
                                          selected = "Line"),
                 
                      checkboxInput("trendLine", label="Add trend line?", value = FALSE),
                      
                 
                      sliderInput("earliestDate", "Time Series (Skips from Origin)", min=0, max=10, value=0, post = "yr.", sep ="", step=1) #use calculation #24 - 120 ...value 120
                      

                    ),
                    

                    menuItem(tags$b('Data Preview & Settings'), tabName = "settings", badgeColor= "green", icon=icon("cogs")) #cogs

                           )
   
                ),
                
                #----------------------------------------------------
                # Designing the contents of the main body
                #----------------------------------------------------
                body <- dashboardBody(
                  
                tags$style(HTML(".box-header{background:#FFFFFF; color:#000000; text-align:center; font-size:20px}")),

                  tabItems(
                    tabItem(tabName = "dashboard",

                            
                            fluidRow(
                              
                              tags$head(
                                tags$style(HTML(".fa{font-size: 20px; }"))),

                              box(tags$b(h4("Set a Weather Scenario:")), "  ",
                                fluidRow(
                                  box(
                                    dateInput("dateToForecast", "Pick a date to forecast Footfall for:", value = as.character(Sys.Date()), min=Sys.Date(), max=as.Date("2019-12-31"), format = "dd/mm/yy"),

                                    background="black", width = 12, solidHeader = TRUE, status = "primary", uiOutput("boxContentUI33") 
                                  )
                                ),
                                fluidRow(
                                  box(

                                    selectizeInput('temp_level', 'Temperature', choices = c("Very Low", "Low", "Moderate", "High")),  #assumed equivalent temperature for the choices: "Very Low=5", "Low=10", "Moderate=15", "High=28")
                                    column(4,
                                      htmlOutput("picture")
                                    ),
                                    background="black", width = 12, solidHeader = TRUE, status = "primary", uiOutput("boxContentUI30") 
                                    
                                  )
                                ),
                                fluidRow( 
                                  box(

                                    selectizeInput('rainfall_level', 'Rainfall', choices = c("None", "Light", "Heavy")), #assumed equivalent rainfall measure for the choices: "None=0, "Light=0.5", "Heavy=10"
                                    background="black", width = 12, solidHeader = TRUE, status = "primary", uiOutput("boxContentUI31") 
                                  )
                                ),
                                
                                background="green", width = 2, solidHeader = TRUE, status = "primary", uiOutput("boxContentUI32") 
                              ), 
                             box(tags$b(h4('Historical Pattern (blue line) + Forecast (red point)')), "  ",
                              box(width = 12, height = "450px", solidHeader = FALSE, status = "success", uiOutput("boxContentUI10"), 
                                  #title = p(tags$b('Historical Patterns and Trend of Footfall Data')),
                                    plotOutput("footfall_history", width = "100%", height = "430px")
                                       ) , width = 10)
                                         
                             ),

                            fluidRow(
                              
                              box(
                                width = 4, status = "success", solidHeader = TRUE,
                                title = tags$b('Boundary of City of Leeds (Inset: City Central)'),
                                leafletOutput("mapLeeds", height=420)
                                
                              ),
                              
                      
                              box(
                                width = 8, solidHeader = TRUE, status = "success",
                                title = tags$b("Estimates of next 5 days footfall rates and their comparison with past weeks"),
                                plotOutput("forecasted_footfall", height = "420px")
                            )
                            )
      
                            ),

                    
                    tabItem(tabName = "settings",

                        box(title = tags$p(tags$b(h4("Preview of Footfall Data Aggregates"))),
                          tabBox(width = 12, height = 800,
                                 
                                 
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
                          
                        box(title = tags$p(tags$b(h4("To update historical footfall and weather records"))),
                            
                              tabBox(width = 12, height = 800,
                                    
                                     tabPanel(title = tags$b('Cameras'), status = "warning", solidHeader = T, background = "aqua",
                                              id='basic parameters',
                                              
                                              #list of names of cameara location
                                              htmlOutput("cameraTitle"),
                                              #tags$hr(),
                                              htmlOutput("cameraLocation"),
                                              tags$hr(),
                                              htmlOutput("warning_cameraLocation")
                                     ),
                                     

                                     tabPanel(title = tags$b('Update Footfall records'), status = "warning", solidHeader = T, background = "aqua",
                                              box(tags$p(tags$b(h4("List of missing dates in the historical footfall database"))),  tags$hr(), # ,#"F
                                                tabPanel("missedFootfall", DT::dataTableOutput("missed_Foot")),
                                                width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI15"),
                                                "  ",
                                                htmlOutput("msg"),
                                                
                                                htmlOutput("testHTML1"),
                                                textOutput("text2"),
                                                htmlOutput("testHTML3"),
                                                htmlOutput("testHTML4"),
                                                htmlOutput("otherInfo")
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
  
                                              fluidPage(
                                                #tags$b("Loading..."), br(),
                                                progressBar(id = "pb1", value = 0)
                                              ),

                                                tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-grid-text, .irs-grid-pol, .irs-slider {visibility:hidden !important;}'))),
                                                useShinyjs(), extendShinyjs(text = jscode1),
                                                tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-grid-text, .irs-grid-pol, .irs-slider {visibility:hidden !important;}'))),
                                                useShinyjs(), extendShinyjs(text = jscode1),
                                                tags$hr(),
                                                uiOutput("processingbar1"),
                                                                                   
                                              htmlOutput("Uploaded_file_checks_Passed"),
                                              tags$hr(), # 
                                              htmlOutput("reload_APP"),
                                              tags$hr(), #  
                                              htmlOutput("issues"),
                                              textOutput("fields_absent"),
                                              textOutput("fall_outside_daterange"),
                                              textOutput("date_Overlapping"),
                                              textOutput("typo_camera_Name"),
                                              tags$hr(), # 
                                              htmlOutput("resolve_issue"),
                                              useShinyjs(),
                                              htmlOutput("append_button_Descrip"),
                                              useShinyjs(),
                                                  actionButton("append", "Append records", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                              actionButton("confirm_Append", "Continue", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              tags$hr(), # 
                                              htmlOutput("confirm_Append"),

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

                                              
                                     ),
                                     
                                     tabPanel(title = tags$b('Update Weather records'), status = "warning", solidHeader = T, background = "aqua",
                                              id='update_predictors',
                                              box(tags$p(tags$b(h4("List of dates with missing Weather information (Temperature and Rain intensity)"))),  tags$hr(), 
                                                  tabPanel("predict_Info", DT::dataTableOutput("missed_Pred_Info")),
                                                  width = 12, solidHeader = FALSE, status = "primary", uiOutput("boxContentUI14"),
                                                  tags$hr(),
                                                  htmlOutput("testHTML1_pred"),
                                                  htmlOutput("text2_pred"),
                                                  htmlOutput("testHTML3_pred"),
                                                  htmlOutput("testHTML4_pred")
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
                                              tags$hr(),
                                              
                                              htmlOutput("Uploaded_file_checks_Passed3"),
                                              tags$hr(), # 
                                              htmlOutput("issues3"),
                                              textOutput("fields_absent3"),
                                              textOutput("fall_outside_daterange3"),
                                              textOutput("date_Overlapping3"),
                                              tags$hr(), # 
                                              htmlOutput("resolve_issue3"),
                                              useShinyjs(),
                                              htmlOutput("append_button_Descrip3"),
                                              useShinyjs(),
                                              actionButton("append_file3", "Update predictor Info.", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              tags$hr(), # 
                                              htmlOutput("taskCompleted3"),
                                              tags$hr(),
                                              actionButton("train_Prediction_Model", "Re-train Prediction Model", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              htmlOutput("restart_app3")
                                              
                                     )

                             )
                        )
                                 

                    )
                    
                  ) 
                )
                
  )
  
)


