#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
library(ggplot2)
library(scales)
library(shinyalert)
library(shinyjs)
library(lubridate)
library(shinyWidgets)
library(foreign)

#option(digits.secs = 1)
EventTime <- Sys.time() - 1*1

#functions
#function to display number with thousand separator
th_separator <- function (x) format(round(as.numeric(x), 1), nsmall=0, big.mark=",")
# 

#subset data, colleting the necessary fields: 'Date', 'Hour', 'Id' & 'LocationName'
subset_Dataset <- function(orig_Data, cameraLoc = "LocationName"){
  #create unique field for the dataset#
  #convert date to appropriate format
  orig_Data_Conv <- convert_Date(orig_Data) #### #head(orig_Data_Conv) #nrow(orig_Data_Conv)
  
  #create unique field (i.e. combination of date and time)
  unique_field <- matrix(paste(orig_Data_Conv$Date, orig_Data_Conv$Hour, sep="-"),,1)
  colnames(unique_field) <- c("Id")
  
  cam_ID <- which(colnames(orig_Data)==cameraLoc)
  #append to real data
  orig_Data_sub <- cbind(orig_Data_Conv$Date, orig_Data$Hour, unique_field, orig_Data$InCount, as.character(orig_Data[,cam_ID]))   #head(length(which(orig_Data=="2011-01-01-0"))  
  orig_Data_sub<-as.data.frame(orig_Data_sub)
  colnames(orig_Data_sub) <- c("Date","Hour","Id","InCount","Loc_Id") #head(orig_Data_sub)
  return(orig_Data_sub)
}

#------------------------

aggregate_Location <- function(orig_Data_sub){
  
  cameraLoc <- as.vector(unique(orig_Data_sub$Loc_Id))
  
  #pick a unique Id
  #sum 'InCount' across all stations.
  
  orig_Data_agg_Loc <- NULL
  uniqId <- unique(orig_Data_sub$Id)
  
  #this is to ensure that all cameras are represented, otherwise "NA" is reported
  loc_agg_data <-  matrix(0, 1, 4)
  rownames(loc_agg_data) <- length(cameraLoc)
  row.N <- "100" #just any number different from 'length(unique_Times)' 
  appd_Row <- matrix(0, 1, 4)
  rownames(appd_Row) <- length(cameraLoc)
flush.console()
print("point1")
  for(i in 1:length(uniqId)){ #i<-1
    #for(i in 1:24){ #i<-24
    
    data_Sub <- orig_Data_sub[which(orig_Data_sub$Id==uniqId[i]),]
    InCountN <- sum(as.numeric(as.vector(orig_Data_sub[which(orig_Data_sub$Id==uniqId[i]),c("InCount")])))
    
    #check if all the unique_times are present
    true_Ct <- length((data_Sub$Loc_Id%in%cameraLoc)=="TRUE")
    
    orig_Data_agg_Loc <- rbind(orig_Data_agg_Loc, cbind(as.character(data_Sub$Date[1]), as.numeric(as.vector(data_Sub$Hour[1])), as.character(uniqId[i]), InCountN))
    combine_Result <- cbind(as.character(data_Sub$Date[1]), as.numeric(as.vector(data_Sub$Hour[1])), as.character(uniqId[i]), InCountN)
    loc_agg_data[which(rownames(loc_agg_data)==true_Ct),] <- as.vector(combine_Result)
    rownames(loc_agg_data) <- rep("100", nrow(loc_agg_data))
    
    loc_agg_data <- rbind(loc_agg_data, appd_Row)
    flush.console()
    print(paste("point2", i, length(uniqId)))
  }
  
  #clean it up
  loc_agg_data <- loc_agg_data[-which(loc_agg_data[,1]=="0"),]
  rownames(loc_agg_data) <- 1:nrow(loc_agg_data)
  loc_agg_data <- as.data.frame(loc_agg_data)
  colnames(loc_agg_data) <- c("Date","Hour","Id","InCount")
  return(loc_agg_data)
  
}

#-----------------------------------

#function to identify outliers
outliers <- function(data=result1){
  hold_result <- matrix(0, length(x), 1)
  x<-as.numeric(as.vector(data$InCount))  #median(x, na.rm=TRUE)
  ind_hold.na <- which(is.na(x))
  ind_hold.not.na  <- which(!is.na(x))
  # hold.not.na <- matrix(0, length(ind_not_na),1)
  x_2 <- x[ind_hold.not.na]
  med <- median(x_2)
  MAD <-median(abs(med-x_2))
  dtf <<- data.frame(ID=seq.int(length(x_2)), obs=x_2, outlier=abs(x_2-med)>3.5*(MAD/0.6745))
  dtf <- as.data.frame(cbind(dtf, ind_hold.not.na))
  colnames(dtf) <- c("id","obs","outlier","ind")
  outlier_ind <- which(dtf$outlier=="TRUE")
  not_outlier_ind <- which(dtf$outlier!="TRUE")
  hold_result[dtf$ind[outlier_ind],1] <- 1  #'1' for outliers
  hold_result[dtf$ind[not_outlier_ind],1] <- 2  #'2' for not outlier
  hold_result[ind_hold.na,1] <- 0
  return(hold_result)
} 
#------------------------


# function to plot line graph with filled area-under-curve
auc_plot <- function(y, plotStyle=1){
  #autoInvalidate1 <- reactiveTimer(5000, session)

  x <- 1:length(y)
  n <- length(y)
  #using ggplot2
  if(plotStyle==1){
     xy_1 <- as.data.frame(cbind(x, y))
     xy_1Type <- rep(1, nrow(xy_1))
     xy_1Type[length(xy_1Type)] <- 2  #changing the type of the last point, so that it can be colored differently
     xy_1 <- data.frame(xy_1Type,  xy_1)
    # a <- ggplot(data=xy_1, aes(x=x, y=y)) + geom_line() + geom_point()   # Left (to compare)  #gam for n > 1000.
    # print(a)
    
     print(ggplot(xy_1, aes(x, y, group=xy_1Type)) +
             geom_line(color="blue", size = 1) +
             #geom_point(color=xy_1Type, size = 2) +
             geom_point(color="blue", size = 2) +
             geom_area(aes(ymin = 0,ymax = y),
                       alpha = 0.3,fill = "blue")) }
  #to generate regular plot
  if(plotStyle==2){
    s = smooth.spline(x, y, spar=0.5)
    xy <- predict(s, seq(min(x), max(x), by=1)) # Some vertices on the curve
    m <- length(xy$x)                         
    x.poly <- c(xy$x, xy$x[m], xy$x[1])         # Adjoin two x-coordinates
    y.poly <- c(xy$y, 0, 0)                     # .. and the corresponding y-coordinates
    plot(range(x), c(0, max(y)), type='n', xlab="X", ylab="Y", axes=F)
    polygon(x.poly, y.poly, col="lightblue", border=NA)
    lines(s, col="blue", lwd=2)
    points(x.poly[1:(length(x.poly)-2)], y.poly[1:(length(y.poly)-2)], pch=16, col="blue") # (Optional)
    points(x.poly[(length(x.poly)-2)], y.poly[(length(y.poly)-2)], pch=16, col="red", cex=2) # (Optional)
  }
  
}


#function to display time
date_function <- function(){
  date_time <- Sys.time()
  dateT <- substr(as.character(date_time), 1, 10)
  timeT <- substr(as.character(date_time), 11, 20)
  dayT <- weekdays(as.Date(dateT))
  print(paste(dayT, ", ", dateT, ", ", sep=""))}

date_function2 <- function(){
  date_time <- Sys.time()
  timeT <- substr(as.character(date_time), 11, 20)
  #dayT <- weekdays(as.Date(dateT))
  print(paste(" ",timeT, "GMT", sep=" "))}

#function to display time
date_function3 <- function(){
  date_time <- Sys.time()
  dateT <- substr(as.character(date_time), 1, 10)
  timeT <- substr(as.character(date_time), 11, 20)
  dayT <- weekdays(as.Date(dateT))
  print(paste(dayT, ", ", dateT, sep=""))}

#function to display tomorrow's day in the forecast panels
day_function <- function(){
  dateD <- Sys.Date() + 1
  dayT <- paste(weekdays(as.Date(dateD)), ", ", (Sys.Date()+1), sep = "")
  print(dayT)}

#function to show gaps in the dates in the historical datasets
missingData <- function(data){
  dataValues <- data$Date
  DateFromData <- as.character(dataValues)
  #To ensure "Date" column conform to the format "yyyy-mm-dd"
  dateField <- matrix(DateFromData,,1)
  colnames(dateField) <- c("Date") # data[1:10000,] head(data)
  #to detect dates not in right format (i.e. yyyy-mm-dd)
  converDate1 <- as.Date(parse_date_time(dateField,"dmy"))
  listInconvertible <- which(!is.na(converDate1))
  dateField[listInconvertible] <- as.character(converDate1[listInconvertible])   #data[89480:89559,]
  #append back to the dataset
  #dataValues <- cbind(min(dateField), "missing dates(months)")
  dataValues <- dateField   
  #append current date to the list..
  dataValues <- rbind(dataValues, as.character(Sys.Date()))
  #to identify gaps in the dataset.
  DF <- as.Date(dataValues)
  DF_Dates <- diff(DF)
  missing_Dates <-   data.frame(from = (DF[DF_Dates>1]+1), to = (DF[c(1, DF_Dates)>1]-1), No_of_days = (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]+1))
  return(missing_Dates)
}

#function to return list of unique dates in a dataset
uniq_Dates <- function(data){
  #first convert the data to the right format i.e. "yyyy-mm-dd"
  dataValues <- data$Date
  DateFromData <- as.character(dataValues)
  #To ensure "Date" column conform to the format "yyyy-mm-dd"
  dateField <- matrix(DateFromData,,1)
  colnames(dateField) <- c("Date") # data[1:10000,] head(data)
  #to detect dates not in right format (i.e. yyyy-mm-dd)
  converDate1 <- as.Date(parse_date_time(dateField,"dmy"))
  listInconvertible <- which(!is.na(converDate1))
  dateField[listInconvertible] <- as.character(converDate1[listInconvertible])   #data[89480:89559,]
  #append back to the dataset
  #dataValues <- cbind(min(dateField), "missing dates(months)")
  dataValues <- unique(dateField)
  return(dataValues)
}

#function to convert list of dates to format 'yyyy-mm-dd'. Returns the dataset, but with date format changed
convert_Date <- function(data){
  #first convert the data to the right format i.e. "yyyy-mm-dd"
  dataValues <- data$Date
  DateFromData <- as.character(dataValues)
  #To ensure "Date" column conform to the format "yyyy-mm-dd"
  dateField <- matrix(DateFromData,,1)
  colnames(dateField) <- c("Date") # data[1:10000,] head(data)
  #to detect dates not in right format (i.e. yyyy-mm-dd)
  converDate1 <- as.Date(parse_date_time(dateField,"dmy"))
  listInconvertible <- which(!is.na(converDate1))
  dateField[listInconvertible] <- as.character(converDate1[listInconvertible])   #data[89480:89559,]
  #append back to the dataset
  #dataValues <- cbind(min(dateField), "missing dates(months)")
  data$Date <- dateField
  return(data)
}


#function to check that uploaded contains the three fields, "Date","Hour","InCount", "LocationName"
uploaded_fieldnames <- function(data){
  essential_Fields <- c("Date","Hour","InCount", "LocationName")
  names_uploaded <- essential_Fields %in% colnames(data)
  leng_name <- length(which(names_uploaded=="TRUE"))
  #return(leng_name)
}

#function to check that all the uploaded records fall within appropriate time range i.e. start date of the historical data and the current time
dateRange_Checker <- function(history_footfall, data){
  #unique dates in the footfall (database) data
  uniqueDate_footfallDatabase <- uniq_Dates(history_footfall)
  #unique dates in the uploaded data
  uniqueDate_uploaded <- uniq_Dates(data)
  #check that all dates fall with range (from the start of footfall data and the current time)
  outside_Dates1 <- which(uniqueDate_uploaded < min(uniqueDate_footfallDatabase))
  outside_Dates2 <- which(uniqueDate_uploaded > Sys.Date())
  outside_Dates <- c(outside_Dates1, outside_Dates2)
  out_Len <- length(outside_Dates)#first check to implement
  return(out_Len)
}

#now to check where there is overlap in the dates:
dateOverlap_Checker <- function(history_footfall, data){
  #unique dates in the footfall (database) data
  uniqueDate_footfallDatabase <- uniq_Dates(history_footfall)
  #unique dates in the uploaded data
  uniqueDate_uploaded <- uniq_Dates(data)
  #check that there is no overlap between the dates dates
  overlap_Dates <- which(uniqueDate_uploaded %in% uniqueDate_footfallDatabase)
  return(length(overlap_Dates))
}

#function to identify outliers#, returns 0 as "na" datapoint, "1" for outliers and  "2" for not outlier


#----------------------------------------------------------

shinyServer(function(input, output, session){


#first check that footfall data is up-to-date
#append all footfall files in the directory 
  
  
  history_footfall <- do.call("rbind", lapply(list.files("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/historical_footfall/",
                                                  full=TRUE),read.csv, header=TRUE))
  
  output$history <- renderDataTable(history_footfall)
  # historical_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/historical_footfall/historical_footfall_up_to_31_12_2016.csv", sep=",", head=TRUE)
  # history_footfall <- historical_footfall
  

  
  # output$gaps <- DT::renderDataTable({
  #   req(input$file1)
  #   file_For_Missing_Data <- read.csv(input$file1$datapath,
  #                                     header = TRUE,
  #                                     sep = ",")#,
  #   uploaded_Table <- DT::datatable(file_For_Missing_Data)
  #   return(uploaded_Table)
  # })


  
  #output$mytable1 <- DT::renderDataTable({
    #   DT::datatable(diamonds2[, input$show_vars, drop=FALSE])
    # })
  # observe({
  #   shinyjs::hide("UI") #hide the processbar first
  # }) 
  
  disable("slider")
  observeEvent(input$file1, priority=10, {
    #shinyjs::hide("UI")
    js$play()
    #Sys.sleep(1) # simulate computation
    })
    #showNotification("File uploaded!", type="error")})
  output$processingbar1 = renderUI({
    shinyjs::show("processingbar1")
    sliderInput("slider", label = "", width = '300px',min = 0,max = 100,value = 0,step = 1, post="%",
                animate = animationOptions(
                  interval = (8*8), #5 seconds
                  playButton = "",
                  pauseButton = ""))})

  #to hide upload button

  # output$table.output <- renderText({
  #   #input$tbl^2
  #   names(history_footfall)
  # })
  #output$table.output <- renderTable({
    #input$tbl^2
  #})
  
    # observeEvent(input$preview,{
    #   shinyalert("Action required!", tags$b("Historical footfall data NOT up-to-date, see 'Settings' page", br(), "Go to 'Settings' page"), type="warning") #default, message, warning, error
    # })

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
  
  #display today's date on the header
  output$dateOnPredictionBoard1 <- renderText({
    #invalidateLater(1000, session)
    date_function3()
  })
  
  #display today's date on the header
  output$dateOnPredictionBoard2 <- renderText({
    #invalidateLater(1000, session)
    date_function3()
  })
  
  #display today's date on the header
  output$dateOnPredictionBoard3 <- renderText({
    #invalidateLater(1000, session)
    date_function3()
  })
  
  #display today's date on the header
  output$dateOnPredictionBoard4 <- renderText({
    #invalidateLater(1000, session)
    date_function3()
  })
  #date to display on tomorrow forecast
  output$tomorrowDate <- renderText({
    date_function()
  })
  
  #day of tomorrow
  output$tomorrowDay_1 <- renderText({
    day_function()
  })
  
  output$tomorrowDay_2 <- renderText({
    day_function()
  })
  
  output$tomorrowDay_3 <- renderText({
    day_function()
  })
  
  output$tomorrowDay_4 <- renderText({
    day_function()
  })
  
  output$morning_footfall <- renderPlot({
    c <- 1:25
    set.seed(11)
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y, plotStyle=2)
  })
  
  output$afternoon_footfall <- renderPlot({
    c <- 1:25
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y, plotStyle=2)
  })
  
  output$evening_footfall <- renderPlot({
    c <- 1:25
    #generate some random number
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y, plotStyle=2)
  })
  
  output$all_footfall <- renderPlot({
    c <- 1:25
    #generate some random number
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y, plotStyle=2)
  })
  
  #plot footfall history
  output$footfall_history <- renderPlot({
    c <- 1:100
    #generate some random number
    set.seed(1)
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y, plotStyle=1)
    #autoInvalidate1()
    #Sys.sleep(1)
  })
  
  output$msgOutput = renderMenu({
    msgs <- apply(read.csv(file = "C:/Users/monsu/Desktop/RShinyDashboard/dash12/misc/messages.csv"), 1, function(row){
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  output$lastHourCount <- renderText({
    paste(th_separator(36*200))
  }) 
  
 
  output$lastDayCount <- renderText({
    paste(th_separator(30*200))
  }) 
  
  output$lastWeekCount <- renderText({
    paste(th_separator(32*200))
  }) 
  
  output$lastWeekCounty <- renderText({
    paste(th_separator(27*200))
  }) 
  
  output$mapLeeds <- renderLeaflet({
    crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    city_central =read.table("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/misc/city_central.csv", sep=",", head=TRUE)
    city_Boundary = readShapePoly("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/misc/leeds_City.shp")
    leaflet(data = city_central[1:nrow(city_central),]) %>% addTiles() %>%
      addMarkers (~X_Lon, ~Y_Lat, popup = ~as.character(Id)) %>% addPolygons(data = city_Boundary, color = "black", fill=FALSE)
    
  })
  
  
 
  
  # #history_footfall =   history_footfall[history(nrow(  history_footfall), 1000),]
  # diamonds2 = diamonds[history(nrow(diamonds),1000),]
  # 
  # output$mytable1 <- DT::renderDataTable({
  #   DT::datatable(diamonds2[, input$show_vars, drop=FALSE])
  # })
  # 
  #history_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/history_Dataset/input_Dataset.csv", sep=",", head=TRUE)
  #historical_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/footfall_31_12_2016.csv", sep=",", head=TRUE)
  #history_footfall <- historical_footfall
  #history_footfall2 =   history_footfall[history(nrow(  history_footfall), 1000),]
  output$mytable1_1 <- DT::renderDataTable({
    DT::datatable(history_footfall[, input$show_vars2, drop=FALSE])
  })
  
  #A copy of the footfall data
  #output$mytable1_2 <- DT::renderDataTable({
  #output$tableCopy <- renderDataTable(iris)
  #})
  # output$mytable2 <- DT::renderDataTable({
  #   DT::datatable(mtcars, options=list(orderClasses = TRUE))
  # })
  # 
  # output$mytable3 <- DT::renderDataTable({
  #   DT::datatable(iris, options=list(orderClasses = TRUE))
  # })
  
  #temperature trend
  output$temp_patterns <- renderPlot({
    c <- 1:100
    #generate some random number
    set.seed(1)
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y, plotStyle=2)
    autoInvalidate1()
    Sys.sleep(1)
  })
  
  #holiday
  output$holidays <- renderPlot({
    c <- 1:100
    set.seed(2)
    #generate some random number
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y, plotStyle=2)
    autoInvalidate1()
    Sys.sleep(1)
  })
  
  output$rainfall_patterns <- renderPlot({
    c <- 1:100
    set.seed(3)
    #generate some random number
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y, plotStyle=2)
    autoInvalidate1()
    Sys.sleep(1)
  })
  
  output$humidity_patterns <- renderPlot({
    c <- 1:100
    set.seed(4)
    #generate some random number
    y <- sample(c^2)
    par(mar=c(0,0,0,2)+0.0, mgp=c(0,0,0))
    auc_plot(y, plotStyle=2)
    autoInvalidate1()
    Sys.sleep(1)
  })
  
  output$wind_patterns <- renderPlot({
    c <- 1:100
    set.seed(5)
    #generate some random number
    y <- sample(c^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y, plotStyle = 2)
    autoInvalidate1()
    Sys.sleep(1)
  })
  #detecting missing data

  #create a list dates occuring in the dataset
  missData <- missingData(history_footfall)

  #to hide "missing data" warning
  if(nrow(missData)>=1){
    output$notify <- renderText({
      print("Issues")
    }) 
  }
   
  #message for "missing data" warning
  if(nrow(missData)>=1){
    output$msg <- renderText({
      print("No missing data detected")
    }) 
  }
  
  #run this if there are missing dataset
  if(nrow(missData)>=1){
    
    output$missed_Foot <- DT::renderDataTable({
      #DT::datatable(historical_footfall[,c("Date","Hour","InCount")])
      DT::datatable(missData)
      #DT::datatable(missing_dates)
    })
    
  output$testHTML1 <- renderText({paste("<b>Above table shows the list of date ranges in which footfall data are missing.", "<br>")})
  output$text2 <- renderText({paste("Search for the missing data from either of the following sources:")})
  output$testHTML3 <- renderText({paste("<b>1. https://datamillnorth.org/dataset/leeds-city-centre-footfall-data")})
  output$testHTML4 <- renderText({paste("<b>2. https://data.gov.uk/dataset/leeds-city-centre-footfall-data")})
  output$text5 <- renderText({paste("Note: Ensure that the file to be uploaded contains the following three columns:")})
  output$text6 <- renderText({paste("   (a) 'Date' - in any of the following formats: 'dd/mm/yyyy', 'dd-mm-yyyy', 'yyyy/mm/dd', OR 'yyyy-mm-dd'")})
  output$text7 <- renderText({paste("   (b) 'Hour' - 'Hour of the day', i.e. 0, 1, 2, .... 23.")})
  output$text8 <- renderText({paste("   (c) 'InCount' - Hourly aggregate of footfall count")})
  output$text9 <- renderText({paste("   (d) 'LocationName' - Containing the names assigned to camera locations")}) 
  output$text10 <- renderText({paste("Upload a .csv file to update the database")})
  output$text11 <- renderText({paste("An 'upload' button will appear after a valid file has been uploaded")})
  }

  #observe({
    #req(input$file1)
    #output$processing <- renderText({print("processing.....wait!")})
  #read the uploaded data to fill in the gap in historical footfall. Purpose: to display

  # output$gaps <- DT::renderDataTable({
  #   req(input$file1)
  #   file_For_Missing_Data <- read.csv(input$file1$datapath,
  #                  header = TRUE,
  #                  sep = ",")#,
  #   uploaded_Table <- DT::datatable(file_For_Missing_Data)
  #     return(uploaded_Table)
  # })
 #})
  
  observe({
    #to hide upload button
    shinyjs::hide("append")
    shinyjs::hide("processingbar2")
    shinyjs::hide("generated_footfall_aggre_data")
  })
  #uploaded data.....: Purpose: observe command is used where no output is returned.
  #uploaded data to fill gaps in the historical footfall record.....: Purpose: observe command is used where no output is returned.
  observe({
    
    shinyjs::show("processingbar1")
    shinyjs::hide("processingbar2")
    #initialisation
    issue1 = 0
    issue2 = 0
    issue3 = 0
    
    req(input$file1)
    #To check the gaps that an uploaded file fill
    uploaded_file <- read.csv(input$file1$datapath,
                              header = TRUE,
                              sep = ",")#,
    #to delay the display...
    ##output$checking <- renderText({paste("checking...... ")})
    
    #shinyjs::hide("upload")
    #checking whether the uploaded file contain essential fields
    leng_name <- uploaded_fieldnames(uploaded_file) #checking essential field names
    out_Len <- dateRange_Checker(history_footfall, uploaded_file) #checking if dates falls outsides desired range 
    overlap_Dates <- dateOverlap_Checker(history_footfall, uploaded_file) #checking whether any of the uploaded record overlap with the dates in the database 
  
    essential_Fields <- c("Date","Hour","InCount", "LocationName")
    
    if(as.numeric(leng_name)!=length(essential_Fields)){
      issue1<-1}
    
    if(out_Len>0){
      issue2<-1}
    
    if(overlap_Dates>0){
      issue3<-1}
    
      #print((leng_name))
      
    # #if((out_Len<-dateRange_Checker(historical_footfall, data))>0){issue2=1}
    # #if((overlap_Dates<-dateOverlap_Checker(historical_footfall, data))>0){issue3=1}
    # 
    # #if(issue1==1){
    #   #print("The uploaded file does not contain one of the following field names")}
    # #if(issue2==1){
    #   #print("One or some of the uploaded dates fall outside the expected range (i.e. earliest date of footfall (database) and the current date")}
    # #if(issue3==1){
    #   #print("Some dates in the uploaded file overlap with dates in the footfall database")}
    # 
    total_issues <- issue1 + issue2 + issue3
    
    #if there is no issues, then show "Upload" button
    if(total_issues==0){
      #turn off
      output$issues <- renderText({paste(" ")})
      output$fields_absent <- renderText({print(" ")})
      output$fall_outside_daterange <- renderText({print(" ")})
      output$date_Overlapping <- renderText({print("")})
      output$resolve_issue <- renderText({paste(" ")})
      
      #turn on
      output$Uploaded_file_checks_Passed <- renderText({paste("<b>'Successful!")})
      shinyjs::show("append")
      shinyjs::show("processingbar1")
      shinyjs::show("processingbar2")
      
      disable("slider1")
      observeEvent(input$append, priority=10, {
        js$play()
        #Sys.sleep(1) # simulate computation
      })
      #showNotification("File uploaded!", type="error")})
      
      
      # output$processingbar2 = renderUI({
      #   shinyjs::show("processingbar1")
      #   sliderInput("slider1", label = "", width = '300px',min = 0,max = 100,value = 0,step = 1, post="%",
      #               animate = animationOptions(
      #                 interval = (8*8), #5 seconds
      #                 playButton = "",
      #                 pauseButton = ""))})
      }
    # 
    if(total_issues!=0){
      #turn off
      shinyjs::hide("processingbar1")
      shinyjs::hide("processingbar2")
      output$Uploaded_file_checks_Passed <- renderText({paste(" ")})
      
      #turn on
      #shinyjs::show("append")}  ###renderText({paste("<b>Above table shows the list of date ranges in which footfall data are missing.", "<br>")})
      output$issues <- renderText({paste("<b>ISSUES IDENTIFIED:", "<br>")})
      if(issue1==1){
      output$fields_absent <- renderText({print("*  One or more of the essential fieldnames missing: 'Date', 'Hour', 'InCount', 'LocationName'")})}
      if(issue2==1){
        output$fall_outside_daterange <- renderText({print("*  One or more of the uploaded dates fall outside the expected range (i.e. earliest date in the footfall (database) and the current date")})}
      if(issue3==1){
        output$date_Overlapping <- renderText({print("*  Some dates in the uploaded file overlap with dates in the footfall database")})}
      shinyjs::hide("append")
      output$resolve_issue <- renderText({paste("<b>Please, resolve issues and re-upload file.....")})
    }
    
  })
  
  #perform the following action upon clicking 'append' button
  observeEvent(input$append, {
    #output$msg_tableAppended <- renderText({paste("Tables appended. See the remaining missing dates below:  ")})
    #create two files
    historicalData_Subset <- history_footfall[,c("Date","Hour","InCount", "LocationName")]
    print(historicalData_Subset)
    req(input$file1)
    #To check the gaps that an uploaded file fill
    uploaded_file <- read.csv(input$file1$datapath,
                              header = TRUE,
                              sep = ",")#,
    uploadedData_Subset <- uploaded_file[,c("Date","Hour","InCount", "LocationName")]
    
    #cleaning the uploaded file; remove outliers
    uploadedData_Subset 
    
    
    #new historical data
    updated_FootfallDataset <- as.data.frame(rbind(historicalData_Subset, uploadedData_Subset))
    colnames(updated_FootfallDataset) <- c("Date","Hour","InCount", "LocationName")
    #sorting
    #updated_FootfallDataset <- updated_FootfallDataset[
      #with(updated_FootfallDataset, order(Date,Hour))
      #]
    #write the appended files for different aggregation
    

    #export appended data#--------------------------------------------------
  #   output$table_Appended <- DT::renderDataTable({
  #     result_appended <- DT::datatable(updated_FootfallDataset)
  #     return(result_appended)
  # })

   
    #gaps after append
    missData_after_append <- missingData(updated_FootfallDataset)
      #result missing data table after the append
      output$missed_Foot_after_Append <- DT::renderDataTable({
        #DT::datatable(historical_footfall[,c("Date","Hour","InCount")])
        DT::datatable(missData_after_append)
        #DT::datatable(missing_dates)
      })
      
      output$Uploaded_file_checks_Passed <- renderText({paste("<b>New records appended successfully! Click 'Generate aggregated data' button to complete the process")})
      
      #title of table after append
      output$table_after_append <- renderText({
        paste("List of missing dates after append")
      })
      
      shinyjs::show("generated_footfall_aggre_data")
      shinyjs::hide("append")
      
      disable("slider1")
      observeEvent(input$generated_footfall_aggre_data, priority=10, {
        js$play()
        #Sys.sleep(1) # simulate computation
      })
      # output$processingbar1 = renderUI({
      #   sliderInput("slider3", label = "", width = '300px',min = 0,max = 100,value = 0,step = 1, post="%",
      #               animate = animationOptions(
      #                 interval = (8*8), #5 seconds
      #                 playButton = "",
      #                 pauseButton = ""))})
      #find the most recent date in the historical footfall dataset
      max_Date <- max(uniq_Dates(updated_FootfallDataset))
      write.table(updated_FootfallDataset, file=paste("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/historical_footfall/historical_footfall_up_to_", max_Date, ".csv", sep=""), sep=",")
      output$file_updated <- renderText({paste("<b> Historical footfall data updated! See the working directory.")})
      
  })
  
  #export appended data
  observeEvent(input$generated_footfall_aggre_data, {
    #output$msg_tableAppended <- rend
    
    orig_Data <- do.call("rbind", lapply(list.files("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/historical_footfall/",
                                                 full=TRUE),read.csv, header=TRUE))
    
    max_Date <- max(uniq_Dates(orig_Data))
    #write.table(updated_FootfallDataset, file=paste("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/historical_footfall_up_to_", max_Date, ".csv", sep=""), sep=",")
    
    #to generate aggregated dataset at varying temporal scales
    print(max_Date)
    #create a list of time aggregate
    hours_of_the_Day <- list(c(0:23), c(8:17), c(18:20), c(21,22,23, 0, 1, 2, 3, 4, 5))
    
    for(i in 1:length(hours_of_the_Day )){ #i<-1
      
      #inputData <- read.table(file="file_3daysData.csv", sep=",", head=TRUE)  #head(orig_Data)
      #orig_Data <- inputData
      
      if(i==1){
        print (hours_of_the_Day[[i]])
        #result1 <- aggregate_Data_for_Plot(orig_Data=orig_Data, cameraLoc = "LocationName", time_aggre = hours_of_the_Day[[i]])
        result1 <- subset_Dataset(orig_Data)
        aggregate_across_location_by_Date <- aggregate_Location(result1)
        #remove outliers
        ##outliersLoc <- outliers(result1) 
        #append the outlier list to the result
        ##result1 <- cbind(result1, outliersLoc)
        ##colnames(result1)<- c("Date","InCount","outlier")
        head(aggregate_across_location_by_Date)
        write.table(aggregate_across_location_by_Date, file="wholeDay.csv", sep=",")
        ##print(time_aggre)
        
      }
      
    }
    
    
    
  })


  
})

