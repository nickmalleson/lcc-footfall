#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#http://shiny.rstudio.com/

#import libraries
library(base)
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(DT)
library(maptools)
library(ggplot2)
library(scales)
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

#set the root directory
#ROOT_DIR = "C:/Users/geomad/Documents/GitHub/"
ROOT_DIR = "C:/Users/monsu/Documents/GitHub/"
#ROOT_DIR = "/Users/nick/research_not_syncd/git_projects/"

#repo of resources used by the web app
#parameter file directory
parameter_directory <- paste0(ROOT_DIR,"lcc-footfall/webapp/resources/")

#specifying the location of user-defined functions
#source("/path/to/file/ft_functions.r")
source(paste(parameter_directory, "ft_functions.R", sep=""))

# Manually set the time zone
Sys.setenv(TZ="Europe/London")

#api key for downloading weather forecast...(http://openweathermap.org/)
owmr_settings("c8a930a2e30b695551e57d375a67d76e")  #Open weather forecast api key 


EventTime <- Sys.time() - 1*1


#to restrict the file upload size to 120MB
options(shiny.maxRequestSize=200*1024^2) 


#----------------------------------------------------------
#MAIN CODE
#----------------------------------------------------------
shinyServer(function(input, output, session){

  output$aggre_HF_by  <- renderText({
    aggre_HF_by <- c("Time", "Location")
  })
  
  output$dateText  <- renderText({
    paste("input$date is", as.character(input$date))
  })

  #list of camera names (location)
  lists_Loc_Correct <- c("Briggate", "Briggate at McDonalds", "Commercial Street at Sharps",
                         "Commercial Street at Barratts", "Headrow", "Dortmund Square",
                         "Albion Street South", "Albion Street North")
  
  
  #Origin of footfall data collection
  HF_startDate <- as.Date("2009-01-01")
  
  #import the predictor information
  predictors_info <- read.table(file=paste(parameter_directory, "predictors_info", ".csv", sep=""), sep=",", head=TRUE) 
  predictors_info <- convert_Date(predictors_info)
  #extract the predictors info that have weather information.
  predictors_info_extract <- predictors_info[which(predictors_info$status==1),]  #head(predictors_info_extract)
  
  
  #---------------------------------------------------------------------------------------------------------------------
  #'FOOTFALL DASHBOARD' Menu
  #---------------------------------------------------------------------------------------------------------------------
  autoInvalidate1 <- reactiveTimer(5000)
  
  #display today's date on the header
  output$headersTime <- renderText({
    #invalidateLater(1000, session)
    date_function()
  })
  
  #display today's time on the header
  output$headersTime2 <- renderText({
    invalidateLater(1000, session)
    date_function2()
  })
  

  #--------------------------------------
  #Historical Pattern (blue line) + Forecast (red point)
  #--------------------------------------
  #To plot the footfall time series 
  #To include a predicted point based on the setting from the 'Set Weather Conditions:' panel
  output$footfall_history <- renderPlot({
    
    #initialisation
    temp_Value <- 0
    rain_Value <- 0
    
    #Take values from the 'Set Weather Conditions:' panel
    input_dateToForecast = as.Date(input$dateToForecast)
    input_temp_level = as.character(input$temp_level)
    input_rain_level = as.character(input$rainfall_level)
    
    #import other predictors and extract the record for the date selected Date above
    predictors_info <- read.table(file=paste(parameter_directory, "predictors_info", ".csv", sep=""), sep=",", head=TRUE) 
    predictors_info <- convert_Date(predictors_info, TimeField = FALSE)  
    x_new <- predictors_info[which(predictors_info$Date == input_dateToForecast), ]                
    
    #the assumed values corresponding to different selectable temperature levels
    if(input_temp_level=="Very Low"){temp_Value=5} 
    else if(input_temp_level=="Low"){temp_Value=10}
    else if(input_temp_level=="Moderate"){temp_Value=15}
    else {temp_Value=28}
    
    x_new$mean_temp <- temp_Value
    
    #the assumed values corresponding to different selectable rainfall levels
    if(input_rain_level=="None"){rain_Value=0} 
    else if(input_rain_level=="Light"){rain_Value=0.5}
    else {rain_Value=10}
    
    x_new$rain <- rain_Value
    
    #drop "Date" and "status" columns, because they are not needed for the prediction 
    x_new <- subset(x_new, select = -c(Date, status))
    
    #load the Random Forest prediction model
    load(paste(parameter_directory, "random_forest_model.rda", sep="")) #parameter_directory
    
    #predict footfall rate for the selected Date, temperature and rain values
    y_new <- round(predict(pred_model, x_new), digits = 0)
    
    Type_dummy <- 2
    outlier_dummy <-2
    y_new <- data.frame(Type_dummy, input_dateToForecast, y_new, outlier_dummy)
    colnames(y_new) <- c("Type","Date","InCount", "Outlier")   #data.frame(Type, Date, InCount, Outliers)
    #input chart type
    chartType = input$chartType
    data <- convert_Date(twentyFourHours_HF_aggre, TimeField = FALSE)
    
    #plot the historical footfall and add the predicted point to the plot
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*365), y_new, addTrend = input$trendLine, chartType=input$chartType)
  })
  

  #----------------------
  #plotting the map
  output$mapLeeds <- renderLeaflet({
    crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    city_central =read.table(file=paste(parameter_directory,"city_central.csv", sep=""), sep=",", head=TRUE)
    val=2
    city_Boundary = readShapePoly(paste(parameter_directory, "leeds_City.shp", sep=""))

    data <- as.data.frame(city_central[1:nrow(city_central),])
    leaflet(data = data) %>% addTiles() %>%
      addMarkers (~X_Lon, ~Y_Lat, popup = ~as.character(Id)) %>% addPolygons(data= city_Boundary, color = "black", fill=FALSE) %>%
      addCircles(data=data, ~X_Lon, ~Y_Lat, popup = ~as.character(Id),  stroke = TRUE, radius=1500)
  })
  
  
  #---------------------------------------------
  #to stream the next 5-days temperature and rainfall ('sourced predictors') from an open website
  #and predict the next 5-days footfall information with them.
  output$forecasted_footfall <- renderPlot({
    
    #streaming 3-hourly aggregates of weather forecast from "https://sci.ncas.ac.uk/leedsweather/", of the next 5-days
    weather_forecast <- get_forecast(lat = 53.8013, lon = -1.548567, cnt = 40) #coord of city centre
    
    Date <- substr(weather_forecast$list$dt_txt, 1, 10)
    uniqueDates <- data.frame(unique(Date))
    colnames(uniqueDates) <- "Date"
    
    weather_forecast <- cbind(Date, weather_forecast$list[, c("main.temp","rain.3h")])
    
    #calculate the mean temperature for each day
    temp_fiveDays <- aggregate(main.temp ~ Date, data = weather_forecast, FUN = mean, drop = FALSE)
    temp_fiveDays <- Data_kelvin_to_celsius(temp_fiveDays)
    #where temp is "NA" (due to instrument breakdown), substitute "5"
    temp_fiveDays <- merge(x = uniqueDates, y = temp_fiveDays, by = "Date", all.x = TRUE, all.y = TRUE)
    temp_fiveDays$main.temp[is.na(temp_fiveDays$main.temp)] <- 5
    #converting the 3 hours rain values to its corresponding 5 minutes aggregates (i.e. equivalence of values from 'http://sci.ncas.ac.uk/leedsweather/')
    weather_forecast$rain.3h <- weather_forecast$rain.3h * 3/5
    #calculate the total rainfall for each day
    rain_fiveDays <- aggregate(rain.3h ~ Date, data = weather_forecast, FUN = sum, na.action = na.omit)
    #where rain is "NA" (due to instrument breakdown), substitute "0"
    rain_fiveDays <- merge(x = uniqueDates, y = rain_fiveDays, by = "Date", all.x = TRUE, all.y = TRUE)
    rain_fiveDays$rain.3h[is.na(rain_fiveDays$rain.3h)] <- 0
    
    #import other predictors (auto-generated)
    predictors_info <- read.table(file=paste(parameter_directory, "predictors_info", ".csv", sep=""), sep=",", head=TRUE) 
    predictors_info <- convert_Date(predictors_info, TimeField = FALSE)  
    #extract the predictors info. for the current day plus the next five days.
    x_new_5_days <- predictors_info[which(as.character(predictors_info$Date) %in% as.vector(temp_fiveDays$Date)),]    
    
    #insert the newly calculated temperature and rainfall dataset into their appropriate fields.
    x_new_5_days$mean_temp <- temp_fiveDays$main.temp
    x_new_5_days$rain <- rain_fiveDays$rain.3h
    
    x_new_5_days <- subset(x_new_5_days, select = -c(Date, status))
    
    #load the prediction model (parameter)
    load(paste(parameter_directory, "random_forest_model.rda", sep=""))
    
    #predict footfall rates for the selected Date, given the inserted temperature and rain values
    y_new_5_days <- as.vector(round(predict(pred_model, x_new_5_days), digits = 0))
    y_new_5_days <- vector_perc_diff(y_new_5_days)
    
    #In order to compare the predicted values with previous footfall observations (of the same week days)
    hist_Profile <- NULL
    hist_Dates <- as.vector(temp_fiveDays$Date)
    hist_Dates_weekdays <- weekdays(as.Date(hist_Dates))
    #import existing footfall dataset
    HF_aggregates <- twentyFourHours_HF_aggre[order(twentyFourHours_HF_aggre$Date, decreasing=FALSE),]
    HF_aggregates_Days <- weekdays(as.Date(HF_aggregates$Date))
    
    y_new_5_days_past_weekdays <- NULL
    
    #extract 6 most recent footfall information of each week days.
    for(k in 1:length(hist_Dates_weekdays)){  #k=1
      id_week <- which(HF_aggregates_Days==hist_Dates_weekdays[k])
      extract_Same_weekDay <- HF_aggregates$InCount[id_week[length(id_week):(length(id_week)-5)]]
      y_new_5_days_past_weekdays <- cbind(y_new_5_days_past_weekdays, extract_Same_weekDay)
    }
    
    colnames(y_new_5_days_past_weekdays) <- hist_Dates_weekdays  
    rownames(y_new_5_days_past_weekdays) <- 1:nrow(y_new_5_days_past_weekdays)
    y_new_5_days_past_weekdays <- as.matrix(y_new_5_days_past_weekdays[,2:ncol(y_new_5_days_past_weekdays)])
    
    print(y_new_5_days)
    print(y_new_5_days_past_weekdays)
    print("33333333333333333333333333333333333333333333333")
    
    #plot
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot3(y=y_new_5_days, y_past = y_new_5_days_past_weekdays) #, chartType = input$forecast_chartType
  })
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------
  #'Data Preview & Settings' Menu
  #-----------------------------------------------------------------------------------------------------------------------------------
  
  #To generate the table displayed in the 'Preview of Footfall Data Aggregates' 
  #Import the aggregated footfall dataset
  twentyFourHours_HF_aggre <- read.table(file=paste(parameter_directory, "twentyFour_HoursAggregation.csv", sep=""), sep=",", head=TRUE)
  twentyFourHours_HF_aggre <- convert_Date(twentyFourHours_HF_aggre) 
  history_footfall <- twentyFourHours_HF_aggre
  output$twentyFourHoursData <- DT::renderDataTable({
    twentyFourHours_HT_Table <- DT::datatable(twentyFourHours_HF_aggre)
    return(twentyFourHours_HT_Table)
  })

  #'Camera' tab
  #---------------------------------------------
  output$cameraTitle = renderText({paste("<b> Names of Camera Location")})
  output$cameraLocation = renderText({paste("1.   Albion Street North", "<br>", "2.   Albion Street South", "<br>", "3.   Briggate", "<br>", "4.   Briggate at McDonalds",
                                             "<br>", "5.   Commercial Street at Barratts", "<br>","6.   Commercial Street at Sharps","<br>","7.   Dortmund Square","<br>",
                                            "8.   Headrow")})
  output$warning_cameraLocation = renderText({paste("Note: Before uploading any new footfall file using the 'Update Footfall records' tab,", "<br>", 
                                                    "ensure that the spellings of the camera (location) names","<br>",
                                                    "are exactly as typed above.")})
  
  #'Update Weather records' tab
  #---------------------------------------------
  #processing bar to display when updating predictors file
  observeEvent(input$aggre_HF_confirm, priority=10, {
    js$play()
  })
  output$processingbar2 = renderUI({
    shinyjs::hide("processingbar2")
    sliderInput("slider", label = "", width = '800px',min = 0, max = 99,value = 0, step = 1, post="% Done. Pls, wait...",
                animate = animationOptions(
                  interval = (8*7200), #5 seconds
                  playButton = "",
                  pauseButton = ""))})
  
  #generate the list of dates whose 'sourced predictors' information (i.e. temperature and rain) is yet to be updated
  missData_Predictors <- missingData(data=predictors_info, indicatorField = TRUE)
  
  # I will check this section later. I don't think they are necessary anymore.
  #Note: the appended row in missingData function has to be removed. 
  if(length(missData_Predictors)==3){
    missData_Predictors
  }
  #message for "missing data" warning
  if(nrow(missData_Predictors)==0){
    output$msg_pred <- renderText({
      paste("<b>Predictor Info. is up-to-date!")
    })
  }
  #missing dates in the predictors information
  if(nrow(missData_Predictors)>0){
    output$missed_Pred_Info <- DT::renderDataTable({
      DT::datatable(apply(missData_Predictors, 2, rev), options = list(lengthMenu = c(5, 10), pageLength = 5))
      })
 
    output$notify_pred <- renderText({ print("Issues")  }) 
  
    output$testHTML1_pred <- renderText({paste("<b>Above table shows the list of days in which predictors (weather) information is missing", "<br>")})
    output$text2_pred <- renderText({paste("Any of the missing dates can be updated by uploading a .csv file containing three essential columns, namely:", "<br>",
                                          "(a) 'Date' - containing the list of all or some of the dates (from the table above), in any of the following formats: 'dd/mm/yyyy', 'dd-mm-yyyy', 'yyyy/mm/dd', OR 'yyyy-mm-dd'", "<br>",
                                          "(b) 'mean_temp' - the respective average temperature (in degree Celcius) for each date", "<br>",
                                          "(c) 'rain' - Average rain intensity (in mm/hr) for each date", "<br>",
                                          "<br>",
                                          "An 'upload' button will appear after a valid file has been uploaded", "<br>",
                                          "<br>",
                                          "Weather information can be obtained from any of the following websites:")})
    output$testHTML3_pred <- renderText({paste("<b>1. https://sci.ncas.ac.uk/leedsweather/")})
    output$testHTML4_pred <- renderText({paste("<b>2. https://www.wunderground.com/weather/gb/leeds")})
    }
    
  #When a file containing new 'sourced predictors' information for the missing dates (above) is uploaded.
    observe({
    
      shinyjs::hide("append_file3")  
      shinyjs::hide("issues3")
      shinyjs::hide("fields_absent3")
      shinyjs::hide("fall_outside_daterange3")
      shinyjs::hide("date_Overlapping3")
      shinyjs::hide("resolve_issue3")
      shinyjs::hide("Re-train Prediction Model")
      
      req(input$file3)
      uploaded_file3 <- read.csv(input$file3$datapath,
                                header = TRUE,
                                sep = ",")#,
      
      #import the existing predictor information file
      predictors_info <- read.table(file=paste(parameter_directory, "predictors_info", ".csv", sep=""), sep=",", head=TRUE) 
      predictors_info <- convert_Date(predictors_info)
      #extract the predictors info whose 'sourced predictors' have already been updated.
      predictors_info_extract <- predictors_info[which(predictors_info$status==1),]  
      predictors_info_extract_Current <- predictors_info_extract[which(predictors_info_extract$Date <= Sys.Date()),] #
      
      startTimeC <- Sys.time()
      
      observe({
      timeUpd <- (as.numeric(round(Sys.time()-startTimeC, digits=1))*50)
      updateProgressBar(session = session, id = "pb3", value = timeUpd) #input$i
      invalidateLater(1000, session)
      })
 
    #initialisation
    total_issues3 = 0
    issue1_3 = 0
    issue2_3 = 0
    issue3_3 = 0

    #checking the uploaded file for errors and conflicts
    leng_name3 <- uploaded_fieldnames(uploaded_file3, essential_Fields =  c("Date","mean_temp","rain")) #checking essential field names
    out_Len3 <- dateRange_Checker(predictors_info, uploaded_file3) #checking if dates falls outsides desired range 
    overlap_Dates3 <- dateOverlap_Checker(predictors_info_extract_Current, uploaded_file3) #checking whether any of the uploaded record overlap with the dates in the database 

    essential_Fields <- c("Date", "mean_temp", "rain")
    
    if(as.numeric(leng_name3)!=length(essential_Fields)){
      issue1_3<-1}
    if(out_Len3>0){
      issue2_3<-1}
    if(overlap_Dates3>0){
      issue3_3<-1}
    
    total_issues3 <- issue1_3 + issue2_3 + issue3_3 #+ issue4_3 #+ issue5_3
    #if there is no issues, hide all these textboxes
    if(total_issues3==0){
      #turn off
      shinyjs::hide("issues3")
      shinyjs::hide("fields_absent3")
      shinyjs::hide("fall_outside_daterange3")
      shinyjs::hide("date_Overlapping3")
      shinyjs::hide("resolve_issue3")
      shinyjs::hide("taskCompleted3")
      shinyjs::hide("restart_app3")
      #but show the these items
      output$Uploaded_file_checks_Passed3 <- renderText({paste("<b>File checks completed! Click the button below to update predictors info.")})
      shinyjs::show("append_file3")
      shinyjs::show("append_button_Descrip3")
      shinyjs::hide("Re-train Prediction Model")
    }
    
    if(total_issues3!=0){
      #if there are issues, hide all these items
      shinyjs::hide("Uploaded_file_checks_Passed3")
      shinyjs::hide("append_file3")
      shinyjs::hide("append_button_Descrip3")
      #but show these items
      shinyjs::show("issues3")
      shinyjs::show("fields_absent3")
      shinyjs::show("fall_outside_daterange3")
      shinyjs::show("date_Overlapping3")
      shinyjs::show("resolve_issue3")
      shinyjs::hide("taskCompleted3")
      shinyjs::hide("restart_app3")
      shinyjs::hide("Re-train Prediction Model")
      
      #message to display for each error identified
      output$issues3 <- renderText({paste("<b>ISSUES IDENTIFIED:", "<br>")})
      if(issue1_3==1){
        output$fields_absent3 <- renderText({print("*  One or more of the essential fieldnames missing: 'Date', 'mean_temp', 'rain'")})}
      if(issue2_3==1){
        output$fall_outside_daterange3 <- renderText({print("*  One or more of the uploaded dates fall outside the expected date range (i.e. earliest date in the already loaded data and the current date")})}
      if(issue3_3==1){
        output$date_Overlapping3 <- renderText({print("*  Weather info. for one or more of the uploaded dates have previously been uploaded!")})}

      output$resolve_issue3 <- renderText({paste("<b>Please, resolve issues and re-upload file.....")})
    }
    
    })
    
    #when the append button is clicked
    observeEvent(input$append_file3, {
      
      shinyjs::hide("Re-train Prediction Model")
      req(input$file3)
      #use the uploaded file
      uploaded_file3 <- read.csv(input$file3$datapath,
                                 header = TRUE,
                                 sep = ",")#,

      #convert date field to appropriate format
      predictors_info <- convert_Date(predictors_info, TimeField = FALSE) 
      weatherInfo <- convert_Date(uploaded_file3, TimeField = FALSE) 
      
      #identify the rows of the predictors file to be updated
      id_to_update <- which(predictors_info$Date %in% weatherInfo$Date)
      #update the 'sourced predictors' fields
      predictors_info[id_to_update, c("mean_temp")] <- weatherInfo$mean_temp
      predictors_info[id_to_update, c("rain")] <- weatherInfo$rain
      #update the status to read "1" - indicating the update 
      predictors_info[id_to_update, c("status")] <- 1
      #}
      
      #sort the predictor information from oldest to ealiest
      predictors_info <- predictors_info[order(predictors_info$Date),] 
      #export the updated 'predictor' field 
      write.table(predictors_info, file=paste(parameter_directory, "predictors_info", ".csv", sep=""), sep=",", row.names=FALSE)
      shinyjs::show("taskCompleted3")
      output$taskCompleted3 <- renderText({paste(tags$p(tags$b(h4("The Weather information for the specified date(s) have been updated successfully!"))))})  #renderText({paste(tags$p(tags$b(h3("Replacing the Existing Raw HF Dataset"))))})
      
      output$restart_app3 <- renderText({paste(tags$p(tags$b(h4("Re-training completed!  Please, restart app. to effect these changes."))))}) 
      
      shinyjs::hide("append_file3")
      #show the button to re-train the model
      shinyjs::show("Re-train Prediction Model")
    })
 
    #upon clicking the "Re-train Prediction Model" button
    observeEvent(input$train_Prediction_Model, priority=10, {
      
      #import/re-import the updated predictors file to retrain the 'random_forest_model.rda' model.
      predictors <- read.table(file=paste(parameter_directory, "predictors_info", ".csv", sep=""), sep=",", head=TRUE)
      predictors <- convert_Date(predictors)
      #import the aggregated footfall dataset in order to fetch footfall information for training
      aggre_footfall <- read.table(file=paste(parameter_directory, "twentyFour_HoursAggregation.csv", sep=""), sep=",", head=TRUE)
      aggre_footfall <- convert_Date(aggre_footfall)
      
      #to match the footfall data with the predictors data and subset the last 3 years of the dataset
      cleaned_data_for_training <- data_Preparation_for_training(aggre_footfall=aggre_footfall, predictors=predictors, modelName ="randomForest", training_length_in_yrs = 3)
      #re-train the model
      pred_model <- randomForest(InCount ~., data=cleaned_data_for_training)
      
      #save the predictive model (parameters)
      save(pred_model, file=paste(parameter_directory,  "random_forest_model.rda", sep="")) 
      #display a message, instructing to re-start the webtool
      shinyjs::show("restart_app3")
      
    })
  

  #'Update Footfall records' tab
  #---------------------------------------------
  #processing bar to display when appending footfall dataset
  observeEvent(input$confirm_Append, priority=10, {
    js$play()
  })
  output$processingbar1 = renderUI({
    shinyjs::hide("processingbar1")
    sliderInput("slider", label = "", width = '800px',min = 0, max = 99,value = 0, step = 1, post="% processed...",
                animate = animationOptions(
                  interval = (8*300), #5 seconds
                  playButton = "",
                  pauseButton = ""))})
  
  #generate the list of dates whose footfall information is not up-to-date (i.e. current date)
  missData <- missingData(history_footfall)
  
  # I will check this section later. I don't think they are necessary anymore.
    if(length(missData)==3){
    missData
    }
  #"missing data" warning
  if(nrow(missData)>0){
    output$notify <- renderText({
      print("Issues")
    }) 
  }
  #warnings
  if(nrow(missData)==0){
    output$msg <- renderText({
      paste("<b>No date is completely missed across all cameras!")
    })
    shinyjs::hide("file1")  
    shinyjs::hide("progressingbar2")  
  }
  
  #list of missing dates in the footfall information
  if(nrow(missData)>0){
    output$missed_Foot <- DT::renderDataTable({
      DT::datatable(apply(missData, 2, rev))
    })
  
  #Some messages/instructions to display  
  output$testHTML1 <- renderText({paste("<b>Above table shows the list of periods in which footfall data are missing.", "<br>")})
  output$text2 <- renderText({paste("Search for the missing data from either of the following sources:")})
  output$testHTML3 <- renderText({paste("<b>1. https://datamillnorth.org/dataset/leeds-city-centre-footfall-data")})
  output$testHTML4 <- renderText({paste("<b>2. https://data.gov.uk/dataset/leeds-city-centre-footfall-data")})

  output$otherInfo <- renderText({paste("Note: Ensure that the file to be uploaded contains the following three columns:","<br>",
                                        "(a) 'Date' - in any format","<br>",
                                        "(b) 'Hour' - 'Hour of the day', either in the format 'hh:mm', or i.e. 0, 1, 2, .... 23.", 
                                        "(c) 'InCount' - Hourly aggregate of footfall count", "<br>",
                                        "(d) 'LocationName' - Containing the names assigned to camera locations (See 'Basic Inputs' tab for the list)", "<br>",
                                        "<br>",
                                        "Upload a .csv file to update the database", "<br>", 
                                        "An 'upload' button will appear after a valid file has been uploaded")})

  output$HF_view <- renderText({paste("<b>The corresponding aggregated HF can be viewed on the 'DASHBOARD' page'; and the actual .csv files can be found in the 'aggregated_historical_HF' directory")})
  output$append_button_Descrip <- renderText({paste("<b> By clicking the 'append' button, the different aggregates, based on the time segmentation, c(0:23), representing the 'whole day' footfall aggregation, will be generated")})
  }

  observe({
    #to hide upload button
    shinyjs::hide("append")
    shinyjs::hide("append_button_Descrip") #   
    shinyjs::hide("confirm_Append")
    shinyjs::hide("reload_APP")
    })

  observe({
    req(input$file1)
    #To check the gaps that an uploaded file fill
    uploaded_file <- read.csv(input$file1$datapath,
                              header = TRUE,
                              sep = ",")#,
    #to take care of data with null records (i.e. especially for new footfall dataset available on the websites)
    uploaded_file <- uploaded_file[which(!is.na(uploaded_file$InCount)),]
    
    #uploading file
    startTimeC <- Sys.time()
    observe({
      #for(i in 1:100){
      timeUpd <- (as.numeric(round(Sys.time()-startTimeC, digits=1))*50)
      updateProgressBar(session = session, id = "pb1", value = timeUpd) #input$i
      invalidateLater(1000, session)
      #}
    })
    
    #initialisation
    total_issues = 0
    issue1 = 0
    issue2 = 0
    issue3 = 0
    issue5 = 0
   
    #checking the uploaded file for errors and conflicts
    leng_name <- uploaded_fieldnames(uploaded_file, essential_Fields = c("Date","Hour","InCount", "LocationName")) #checking essential field names
    out_Len <- dateRange_Checker(history_footfall, uploaded_file) #checking if dates falls outsides desired range 
    overlap_Dates <- dateOverlap_Checker(history_footfall, uploaded_file) #checking whether any of the uploaded record overlap with the dates in the database 
    check_typo_in_Camera_Name <- check_typo_in_Camera_Name(data=uploaded_file, lists_Loc_Correct)

    essential_Fields <- c("Date","Hour","InCount", "LocationName")
    
    if(as.numeric(leng_name)!=length(essential_Fields)){
      issue1<-1}
    if(out_Len>0){
      issue2<-1}
    if(overlap_Dates>0){
      issue3<-1}
    if(check_typo_in_Camera_Name>0){
      issue5<-1
    }
    
    total_issues <- issue1 + issue2 + issue3 + issue5 #+ issue5
    if(total_issues==0){
      #if there is no issues, then hide the following items
      shinyjs::hide("issues")
      shinyjs::hide("fields_absent")
      shinyjs::hide("fall_outside_daterange")
      shinyjs::hide("date_Overlapping")
      #shinyjs::hide("timeFormatWrong")
      shinyjs::hide("typo_camera_Name")
      shinyjs::hide("resolve_issue")
      shinyjs::hide("reload_APP")
      #but show the following items
      output$Uploaded_file_checks_Passed <- renderText({paste("<b>File checks completed!")})
      shinyjs::show("append")
      shinyjs::show("append_button_Descrip")

      }
    
    if(total_issues!=0){
      #if there are issues, then hide the following items
      shinyjs::hide("processingbar1")
      shinyjs::hide("Uploaded_file_checks_Passed")
      shinyjs::hide("append")
      shinyjs::hide("append_button_Descrip")
      shinyjs::hide("reload_APP")
      
      #message to display for each error identified
      output$issues <- renderText({paste("<b>ISSUES IDENTIFIED:", "<br>")})
      if(issue1==1){
      output$fields_absent <- renderText({print("*  One or more of the essential fieldnames missing: 'Date', 'Hour', 'InCount', 'LocationName'")})}
      if(issue2==1){
        output$fall_outside_daterange <- renderText({print("*  One or more of the uploaded dates fall outside the expected range (i.e. earliest date in the footfall (database) and the current date")})}
      if(issue3==1){
        output$date_Overlapping <- renderText({print("*  Some dates in the uploaded file overlap with dates in the existing footfall database")})}
      if(issue5==1){
        output$typo_camera_Name <- renderText({paste("*  Errors detected in the name(s) of camera location. Check that there are no typo errors in the names of camera locations. Check 'Parameter' tab for correct spellings of location names.")})
      }
      
      output$resolve_issue <- renderText({paste("<b>Please, resolve issues and re-upload file.....")})
    }
    
  })
  
  #perform the following action upon clicking 'append' button
  observeEvent(input$append, {
    shinyjs::show("confirm_Append")
  })
    #show 'Continue' button
    observeEvent(input$confirm_Append, {

    req(input$file1)
    #To check the gaps that an uploaded file fill
    uploaded_file <- read.csv(input$file1$datapath,
                              header = TRUE,
                              sep = ",")#,
    
    #to take care of data with null records (new/latest footfall data downloads)
    uploaded_file <- uploaded_file[which(!is.na(uploaded_file$InCount)),]
    shinyjs::show("processingbar1")

    #subset the uploaded footfall dataset for only the necessary fields
    uploadedData_Subset <- uploaded_file[,c("Date","Hour","InCount", "LocationName")]
     
    #removing possible whitespaces in the names of camera location
    uploadedData_Subset <- remove_whiteSpace_in_Camera_Name(uploadedData_Subset)
  
    #get the most recent date from the uploaded dataset
    max_Date <- max(uniq_Dates(uploadedData_Subset))

    time_aggregation <- c("twentyFour_Hours")
    #first aggregating the footfall count in each stations for all hours of the day
    result1 <- subset_Dataset(orig_Data = uploadedData_Subset, cameraLoc = "LocationName")

    #now aggregate the footfall count accross the stations for each day
    aggregate_Location <- aggregate_Location(orig_Data_sub = result1)
    # #-----------------------------------
    #To determine whether the originally uploaded file actually contained aggregated records. 
    HourField <- as.vector(is.na(aggregate_Location$Hour[1]))
    
   if(nrow(aggregate_Location)>1){
    for(j in 1:length(hours_of_the_Day)){ #i<-1   #length(hours_of_the_Day )
       print (hours_of_the_Day[[j]])
      
      if(HourField==FALSE){
       aggregate_time_of_the_Day <- footfall_by_time_of_the_Day(loc_agg_data=aggregate_Location, time_aggre = hours_of_the_Day[[j]])}
      
      if(HourField==TRUE){
        aggregate_time_of_the_Day <- subset(aggregate_Location, select=c(Date, InCount)) 
        colnames(aggregate_time_of_the_Day) <- c("Date", "InCount")
      }
      
       outlier_events <- outliers(data=aggregate_time_of_the_Day)
    #   #append the outlier list to the result
       update_aggregate <- cbind(aggregate_time_of_the_Day, outlier_events)
       colnames(update_aggregate)<- c("Date","InCount","outlier")
       update_aggregate <- as.data.frame(update_aggregate)
       
       print(update_aggregate)
       print("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
       
      #Import the existing aggregate file and merge the uploaded (now aggregated) files
      existing_time_aggre_HF <- read.table(file=paste(parameter_directory, time_aggregation[j], "Aggregation.csv", sep=""), sep = ",", head=TRUE)
      existing_time_aggre_HF <- convert_Date(existing_time_aggre_HF)
      existing_time_aggre_HF <- as.data.frame(existing_time_aggre_HF)
      Date_Combined <- c(existing_time_aggre_HF$Date, as.vector(update_aggregate$Date))
      InCount_Combined <- c(existing_time_aggre_HF$InCount, as.vector(update_aggregate$InCount))
      data_Combined <- as.data.frame(cbind(Date_Combined, InCount_Combined))
      colnames(data_Combined) <- c("Date","InCount")
      print(data_Combined[2800:nrow(data_Combined),])
   
      print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")

      #drop the outlier field and re-compute a new one using the combined records
      outlier_events <- outliers(data=data_Combined)

      #append the outlier list to the combined dataset
      aggregates_updated <- cbind(data_Combined, outlier_events)
      
      colnames(aggregates_updated)<- c("Date","InCount","outlier")
      
      #just checking...
      print(aggregates_updated[2800:nrow(aggregates_updated),])
      print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      
      aggregates_updated <- aggregates_updated[order(aggregates_updated$Date),]
      aggregates_updated <- convert_Date(aggregates_updated)
      #writing the data aggregates based on four time segmentations
      
      aggregates_updated <- aggregates_updated[order(aggregates_updated$Date),] #####
      
      #just checking...
      print(aggregates_updated[2800:nrow(aggregates_updated),])
      print("TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT")
      
      #export the newly combined (aggregaed) dataset
      write.table(aggregates_updated, file=paste(parameter_directory, time_aggregation[j], "Aggregation.csv", sep=""), sep=",", row.names=FALSE) 
      
      shinyjs::hide("processingbar1")
      output$aggre_HF_file_updated <- renderText({paste("<b> The aggregated HF files have been generated from the uploaded file and appended to the existing aggregated files accordingly!")})
      output$reload_HF_update <- renderText({paste(tags$p(tags$b(h2("Please, re-load the application to see changes made. Thanks."))))}) # have to check this
   }

    shinyjs::hide("append_button_Descrip")
    shinyjs::hide("append")
    shinyjs::hide("confirm_Append")
   }
    
    #If one or more camera are down for for all the 'Dates' uploaded, no aggregated datasets will be generated. 
    #Thus,'aggregate_Location' variable would return "1".
    if(aggregate_Location==1){
      #Therefore, append button will not appear, as there is nothing to append
      shinyjs::hide("append_button_Descrip")
      shinyjs::hide("append")
      shinyjs::hide("confirm_Append")   
      #instead, a message will appear, informing about the situation
      showModal(modalDialog(
        title = "Missing Data!",
        "Data from one or more camera location is missing. The aggregated time series will not be updated! However, HF file would be updated. Please, wait while the processing is completed!",
        easyClose = FALSE
      ))
    }
    
    #if actuall update is made...
    output$reload_APP <- renderText({paste(tags$p(tags$b(h2("Please, re-load the application to see changes made. Thanks."))))})
    shinyjs::hide("processingbar1")
    shinyjs::show("reload_APP")
    shinyjs::hide("Uploaded_file_checks_Passed")
    
  })
   
  observe({
    shinyjs::hide("aggre_HF")
    #shinyjs::show("processingbar1")
    shinyjs::hide("aggre_HF_confirm")
  })


  }) 
    
