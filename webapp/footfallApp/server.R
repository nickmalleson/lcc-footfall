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


#initiali.
canceller = 0
#option(digits.secs = 1)
EventTime <- Sys.time() - 1*1

#functions
#function to display number with thousand separator
th_separator <- function (x) format(round(as.numeric(x), 1), nsmall=0, big.mark=",")

#function to plot line graph with filled area-under-curve
auc_plot <- function(y){
  x <- 1:length(y)
  n <- length(y)
  s = smooth.spline(x, y, spar=0.5)
  xy <- predict(s, seq(min(x), max(x), by=1)) # Some vertices on the curve
  m <- length(xy$x)                         
  x.poly <- c(xy$x, xy$x[m], xy$x[1])         # Adjoin two x-coordinates
  y.poly <- c(xy$y, 0, 0)                     # .. and the corresponding y-coordinates
  plot(range(x), c(0, max(y)), type='n', xlab="X", ylab="Y", axes=F)
  polygon(x.poly, y.poly, col="lightblue", border=NA)          # Show the polygon fill only
  lines(s, col="blue", lwd=2)
  points(x.poly[length(x.poly)-2], y.poly[length(y.poly)-2], pch=16, col="blue") # (Optional)
}


shinyServer(function(input, output, session){
  
  #every 2 seconds
  #autoInvalidate <- reactiveTimer(1000, session)
  autoInvalidate <- reactiveTimer(5000)
  
  #display date and time on the header
  # output$headersTime <- renderText({
  #   #using the zone
  #   date_time <- Sys.time()
  #   invalidateLater(1000, session)
  #   print(paste(as.character(date_time), "GMT", sep=" "))
  # })
  
  output$headersLogo <- renderText({
    #using the zone
    #date_time <- Sys.time()
    #invalidateLater(1000, session)
    #HTML(paste("<div class= man_made_class>","Last updated at", filetime, "</div>")) #
    print("Powered by:")
  })
  
  ##output$histogram <- renderPlot({
    #https://www.youtube.com/watch?v=KdvlxJaWWVQ 7:10
    #hist(faithful$eruptions, breaks = input$bins, main = "")
    ##autoInvalidate()
    #hist(rnorm(isolate(input$n)))
    #hist(rnorm(input$n))
    #hist(rnorm(input$n))
    ##hist(rnorm(30))
  ##})
  
  output$hour_footfall <- renderPlot({
    #https://www.youtube.com/watch?v=KdvlxJaWWVQ 7:10
    #hist(faithful$eruptions, breaks = input$bins, main = "")
    ##autoInvalidate()
    #hist(rnorm(isolate(input$n)))
    #hist(rnorm(input$n))
    #hist(rnorm(input$n))
    #hist(rnorm(30))
    x <- 1:25
    y <- x^2
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y)
  })
  
  output$daily_footfall <- renderPlot({
    #https://www.youtube.com/watch?v=KdvlxJaWWVQ 7:10
    #hist(faithful$eruptions, breaks = input$bins, main = "")
    ##autoInvalidate()
    #hist(rnorm(isolate(input$n)))
    #hist(rnorm(input$n))
    #hist(rnorm(input$n))
    #hist(rnorm(30))
    x <- 1:25
    y <- x^2
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y)
    #lines(x,y, cex = 1.5)
  })
  
  output$week_footfall <- renderPlot({
    #https://www.youtube.com/watch?v=KdvlxJaWWVQ 7:10
    #hist(faithful$eruptions, breaks = input$bins, main = "")
    ##autoInvalidate()
    #hist(rnorm(isolate(input$n)))
    #hist(rnorm(input$n))
    #hist(rnorm(input$n))
    #hist(rnorm(30))
    x <- 1:25
    y <- x^2
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot(y)
  })
  
  output$msgOutput = renderMenu({
    msgs <- apply(read.csv(file = "C:/Users/monsu/Desktop/RShinyDashboard/dash12/misc/messages.csv"), 1, function(row){
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  # output$approvedSales <- renderInfoBox({
  #   infoBox("Approval Sales", "10,000,000",
  #            icon = icon("bar-chart-o"))
  #  })
  
  #observe({
  # output$currentCount <- renderValueBox({
  #   #numberBleep <-  
  #   autoInvalidate2()
  #   Sys.sleep(1)
  #   valueBox(
  #     print(th_separator(30*200)), 
  #     "Current footfall count (forecast)", icon = icon("arrow-circle-down"), color = "green") #yellow #icon("street-view")
  #   
  # }) 
  #})
  
  # output$currentCount <- renderText({
  #   #numberBleep <-  
  #   autoInvalidate2()
  #   Sys.sleep(1)
  #   valueBox(
  #     print(th_separator(30*200)), 
  #     "Current footfall count (forecast)", icon = icon("arrow-circle-down"), color = "green") #yellow #icon("street-view")
  #   
  # }) 
  output$lastHourCount <- renderText({
    
    #numberBleep <-  
    autoInvalidate()
    #Sys.sleep(1)
    #valueBox(
    #print(th_separator(30*200)), 
    #"Current footfall count (forecast)", icon = icon("arrow-circle-down"), color = "green") #yellow #icon("street-view")
    paste(th_separator(30*200))
  }) 
  
  #to update
  output$lastDayCount <- renderText({
    
    #numberBleep <-  
    autoInvalidate()
    #Sys.sleep(1)
    #valueBox(
      #print(th_separator(30*200)), 
      #"Current footfall count (forecast)", icon = icon("arrow-circle-down"), color = "green") #yellow #icon("street-view")
    paste(th_separator(306*200))
  }) 
  
  output$lastWeekCount <- renderText({
    
    #numberBleep <-  
    autoInvalidate()
    #Sys.sleep(1)
    #valueBox(
    #print(th_separator(30*200)), 
    #"Current footfall count (forecast)", icon = icon("arrow-circle-down"), color = "green") #yellow #icon("street-view")
    paste(th_separator(3092*200))
  }) 
  

  
  # output$todayaverage <- renderValueBox({
  #   valueBox(th_separator(20*100), "Today's Average (forecast)", icon = icon("arrow-circle-up"), color = "blue")
  # }) 
  # 
  # output$eventTimeRemaining <- renderValueBox({  #renderText
  #   numberSequence <- rep(c(paste(rep(input$m:1, 1), "sec"), "forecast updated!"), 1000)
  #   invalidateLater(1000, session)
  #   time_to_update <- round(difftime(Sys.time(), EventTime, units='secs'))
  #   valueBox(
  #     print(paste(numberSequence[time_to_update], sep=" ")), icon = icon("clock-o"), paste("Remaining time to update forecast:", sep = " "), color = "blue")
  # })
  
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
  
  #output$chart
  
})

