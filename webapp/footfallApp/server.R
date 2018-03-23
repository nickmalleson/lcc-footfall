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



#option(digits.secs = 1)
EventTime <- Sys.time() - 1*1

#functions
#function to display number with thousand separator
th_separator <- function (x) format(round(as.numeric(x), 1), nsmall=0, big.mark=",")

#function to plot line graph with filled area-under-curve
auc_plot <- function(y, t=1){
  #autoInvalidate1 <- reactiveTimer(5000, session)
  x <- 1:length(y)
  n <- length(y)
  s = smooth.spline(x, y, spar=0.5)
  xy <- predict(s, seq(min(x), max(x), by=1)) # Some vertices on the curve
  m <- length(xy$x)                         
  x.poly <- c(xy$x, xy$x[m], xy$x[1])         # Adjoin two x-coordinates
  y.poly <- c(xy$y, 0, 0)                     # .. and the corresponding y-coordinates
  plot(range(x), c(0, max(y)), type='n', xlab="X", ylab="Y", axes=F)
  polygon(x.poly, y.poly, col="lightblue", border=NA)  
  # Show the polygon fill only
  lines(s, col="blue", lwd=2)
  points(x.poly[1:(length(x.poly)-2)], y.poly[1:(length(y.poly)-2)], pch=16, col="blue") # (Optional)
  points(x.poly[(length(x.poly)-2)], y.poly[(length(y.poly)-2)], pch=16, col="red", cex=2) # (Optional)
  #plot histogram instead of line graph
  if(t==2){
    hist(y, breaks = 100)
  }
}

#function to display time
date_function <- function(){
  date_time <- Sys.time()
  dateT <- substr(as.character(date_time), 1, 10)
  timeT <- substr(as.character(date_time), 11, 20)
  dayT <- weekdays(as.Date(dateT))
  print(paste(dayT, "||", dateT, "||", timeT, "GMT", sep=" "))}

#function to display tomorrow's day in the forecast panels
day_function <- function(){
  dateD <- Sys.Date() + 1
  dayT <- paste(weekdays(as.Date(dateD)), ", ", (Sys.Date()+1), sep = "")
  print(dayT)}
#print(date_time)

#----------------------------------------------------------

shinyServer(function(input, output, session){
  
  autoInvalidate1 <- reactiveTimer(5000)
  
  #display date and time on the header
  output$headersTime <- renderText({
    invalidateLater(1000, session)
    date_function()
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
    x <- 1:25
    set.seed(11)
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y)
  })
  
  output$afternoon_footfall <- renderPlot({
    x <- 1:25
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y)
  })
  
  output$evening_footfall <- renderPlot({
    x <- 1:25
    #generate some random number
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y)
  })
  
  output$all_footfall <- renderPlot({
    x <- 1:25
    #generate some random number
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y)
  })
  
  #plot footfall history
  output$footfall_history <- renderPlot({
    x <- 1:100
    #generate some random number
    set.seed(1)
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y)
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
  
  Cleaned_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/Cleaned_Dataset/input_Dataset.csv", sep=",", head=TRUE)
  
  Cleaned_footfall =   Cleaned_footfall[sample(nrow(  Cleaned_footfall), 1000),]
  
  output$rawdata <- DT::renderDataTable({
    DT::datatable(Cleaned_footfall[, input$show_vars, drop=FALSE])
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(Cleaned_footfall, options=list(orderClasses = TRUE))
    
  })
  
  #temperature trend
  output$temp_patterns <- renderPlot({
    x <- 1:100
    #generate some random number
    set.seed(1)
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y)
    autoInvalidate1()
    Sys.sleep(1)
  })
  
  #holiday
  output$holidays <- renderPlot({
    x <- 1:100
    set.seed(2)
    #generate some random number
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y)
    autoInvalidate1()
    Sys.sleep(1)
  })
  
  output$rainfall_patterns <- renderPlot({
    x <- 1:100
    set.seed(3)
    #generate some random number
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y)
    autoInvalidate1()
    Sys.sleep(1)
  })
  
  output$humidity_patterns <- renderPlot({
    x <- 1:100
    set.seed(4)
    #generate some random number
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y)
    autoInvalidate1()
    Sys.sleep(1)
  })
  
  output$wind_patterns <- renderPlot({
    x <- 1:100
    set.seed(5)
    #generate some random number
    y <- sample(x^2)
    par(mar=c(0,0,0,0)+0.0, mgp=c(0,0,0))
    auc_plot(y)
    autoInvalidate1()
    Sys.sleep(1)
  })
  #output$chart
  
})

