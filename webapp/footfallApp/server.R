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


#function to display tomorrow's day in the forecast panels
day_function <- function(){
  dateD <- Sys.Date() + 1
  dayT <- paste(weekdays(as.Date(dateD)), ", ", (Sys.Date()+1), sep = "")
  print(dayT)}
#print(date_time)

#----------------------------------------------------------

shinyServer(function(input, output, session){


  #first check that footfall data is up-to-date
  #utd <- 1
  #if(utd == 1){
    
    observeEvent(input$preview,{
      shinyalert("Action required!", tags$b("Historical footfall data NOT up-to-date, see 'Settings' page", br(), "Go to 'Settings' page"), type="warning") #default, message, warning, error
    })

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
    paste(th_separator(301*200))
  }) 
  
 
  output$lastDayCount <- renderText({
    paste(th_separator(306*200))
  }) 
  
  output$lastWeekCount <- renderText({
    paste(th_separator(302*200))
  }) 
  
  output$lastWeekCounty <- renderText({
    paste(th_separator(39*200))
  }) 
  
  output$map_2 <- renderLeaflet({
    crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    city_central =read.table("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/misc/city_central.csv", sep=",", head=TRUE)
    city_Boundary = readShapePoly("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/misc/leeds_City.shp")
    leaflet(data = city_central[1:nrow(city_central),]) %>% addTiles() %>%
      addMarkers (~X_Lon, ~Y_Lat, popup = ~as.character(Id)) %>% addPolygons(data = city_Boundary, color = "black", fill=FALSE)
    
  })
  
  
 
  
  # #Cleaned_footfall =   Cleaned_footfall[sample(nrow(  Cleaned_footfall), 1000),]
  # diamonds2 = diamonds[sample(nrow(diamonds),1000),]
  # 
  # output$mytable1 <- DT::renderDataTable({
  #   DT::datatable(diamonds2[, input$show_vars, drop=FALSE])
  # })
  # 
  Cleaned_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/Cleaned_Dataset/input_Dataset.csv", sep=",", head=TRUE)
  #Cleaned_footfall2 =   Cleaned_footfall[sample(nrow(  Cleaned_footfall), 1000),]
  output$mytable1_1 <- DT::renderDataTable({
    DT::datatable(Cleaned_footfall[, input$show_vars2, drop=FALSE])
  })
  
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
  #output$chart
  
})

