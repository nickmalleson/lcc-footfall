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

#option(digits.secs = 1)
EventTime <- Sys.time() - 1*1

#functions
#function to display number with thousand separator
th_separator <- function (x) format(round(as.numeric(x), 1), nsmall=0, big.mark=",")
# 

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

#function to check that uploaded contains the three fields, "Date","Hour","InCount"
uploaded_fieldnames <- function(data){
  names_uploaded <- c("Date","Hour","InCount") %in% colnames(data)
  leng_name <- length(which(names_uploaded=="TRUE"))
  #return(leng_name)
}

#function to check that all the uploaded records fall within appropriate time range i.e. start date of the historical data and the current time
dateRange_Checker <- function(historical_footfall, data){
  #unique dates in the footfall (database) data
  uniqueDate_footfallDatabase <- uniq_Dates(historical_footfall)
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
dateOverlap_Checker <- function(historical_footfall, data){
  #unique dates in the footfall (database) data
  uniqueDate_footfallDatabase <- uniq_Dates(historical_footfall)
  #unique dates in the uploaded data
  uniqueDate_uploaded <- uniq_Dates(data)
  #check that there is no overlap between the dates dates
  overlap_Dates <- which(uniqueDate_uploaded %in% uniqueDate_footfallDatabase)
  return(length(overlap_Dates))
}

#function to identify outliers
outliers <- function(data){
  med <- median(x)
  MAD <-median(abs(med-x))
  dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=abs(x-med)>3.5*(MAD/0.6745))
  #dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier= ((0.6745 * abs(x-med))/MAD>3.5))
  midp <<- med
  lower <<- med-2*(MAD/0.6745)
  upper <<- med+2*(MAD/0.6745)
  outliern <<- length(which(dtf=="TRUE"))
  return(dft)
} 

#----------------------------------------------------------

shinyServer(function(input, output, session){


#first check that footfall data is up-to-date
#append all footfall files in the directory 
  
  
  historical_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/footfall_31_12_2016.csv", sep=",", head=TRUE)
  history_footfall <- historical_footfall
  
  output$history <- renderDataTable(history_footfall)
  
  #output$mytable1 <- DT::renderDataTable({
    #   DT::datatable(diamonds2[, input$show_vars, drop=FALSE])
    # })
    
    
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
  missData <- missingData(historical_footfall)

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
  output$text9 <- renderText({paste("Upload a .csv file to update the database")})
  output$text10 <- renderText({paste("An 'upload' button will appear after a valid file has been uploaded")})
  }

  #read the uploaded data to fill in the gap in historical footfall. Purpose: to display
  output$gaps <- DT::renderDataTable({
    req(input$file1)
    file_For_Missing_Data <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",")#,
    uploaded_Table <- DT::datatable(file_For_Missing_Data)
      return(uploaded_Table)
  })
  
  observe({
    #to hide upload button
    shinyjs::hide("append")
  })
  #uploaded data.....: Purpose: observe command is used where no output is returned.
  #uploaded data to fill gaps in the historical footfall record.....: Purpose: observe command is used where no output is returned.
  observe({
    
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
    out_Len <- dateRange_Checker(historical_footfall, uploaded_file) #checking if dates falls outsides desired range 
    overlap_Dates <- dateOverlap_Checker(historical_footfall, uploaded_file) #checking whether any of the uploaded record overlap with the dates in the database 
  
    if(as.numeric(leng_name)!=3){
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
      output$Uploaded_file_checks_Passed <- renderText({paste("<b>'Checks' passed!")})
      shinyjs::show("append")}
    # 
    if(total_issues!=0){
      #turn off
      output$Uploaded_file_checks_Passed <- renderText({paste(" ")})
      
      #turn on
      #shinyjs::show("append")}  ###renderText({paste("<b>Above table shows the list of date ranges in which footfall data are missing.", "<br>")})
      output$issues <- renderText({paste("<b>ISSUES IDENTIFIED:", "<br>")})
      if(issue1==1){
      output$fields_absent <- renderText({print("*  One or more of the essential fieldnames missing: 'Date', 'Hour', 'InCount'")})}
      if(issue2==1){
        output$fall_outside_daterange <- renderText({print("*  One or more of the uploaded dates fall outside the expected range (i.e. earliest date in the footfall (database) and the current date")})}
      if(issue3==1){
        output$date_Overlapping <- renderText({print("*  Some dates in the uploaded file overlap with dates in the footfall database")})}
      shinyjs::hide("append")
      output$resolve_issue <- renderText({paste("<b>Please, resolve issues and re-upload file.....")})
    }
    
  })
  
  #perform the following upon clicking 'append' button
  observeEvent(input$append, {
    #output$msg_tableAppended <- renderText({paste("Tables appended. See the remaining missing dates below:  ")})
    #create two files
    historicalData_Subset <- history_footfall[,c("Date","Hour","InCount")]
    print(historicalData_Subset)
    req(input$file1)
    #To check the gaps that an uploaded file fill
    uploaded_file <- read.csv(input$file1$datapath,
                              header = TRUE,
                              sep = ",")#,
    uploadedData_Subset <- uploaded_file[,c("Date","Hour","InCount")]
    
    #cleaning the uploaded file; remove outliers
    uploadedData_Subset 
    
    
    #new historical data
    updated_FootfallDataset <- as.data.frame(rbind(historicalData_Subset, uploadedData_Subset))
    colnames(updated_FootfallDataset) <- c("Date","Hour","InCount")
    #sorting
    #updated_FootfallDataset <- updated_FootfallDataset[
      #with(updated_FootfallDataset, order(Date,Hour))
      #]
    #write the appended files for different aggregation
    
    
    #export appended data
    output$table_Appended <- DT::renderDataTable({
      result_appended <- DT::datatable(updated_FootfallDataset)
      return(result_appended)
    #implement error catching here..
    ##updated_FootfallDataset <- updated_FootfallDataset[,c("Date","Hour","InCount")]
    #print(updated_FootfallDataset[(length(updated_FootfallDataset)-10):length(updated_FootfallDataset),])
    
    #DT::datatable(updated_FootfallDataset[,c("Date","Hour","InCount"), drop=FALSE] )
  })
    
    #gaps after append
    missData_after_append <- missingData(updated_FootfallDataset)
      #result missing data table after the append
      output$missed_Foot_after_Append <- DT::renderDataTable({
        #DT::datatable(historical_footfall[,c("Date","Hour","InCount")])
        DT::datatable(missData_after_append)
        #DT::datatable(missing_dates)
      })
      #title of table after append
      output$table_after_append <- renderText({
        paste("List of missing dates after append")
      })
      
      #create files for each time aggregation.
      #create .csv of footfall in morning (6-9pm) over time 
      
      
      
  })
  
  
  
  
  
})

