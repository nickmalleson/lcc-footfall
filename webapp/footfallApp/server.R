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


#initiali.
canceller = 0

EventTime <- Sys.time() - 1*1

shinyServer(function(input, output, session){
  
  #every 2 seconds
  autoInvalidate <- reactiveTimer(1000, session)
  autoInvalidate2 <- reactiveTimer(5000)
  
   output$histogram <- renderPlot({
    #https://www.youtube.com/watch?v=KdvlxJaWWVQ 7:10
    #hist(faithful$eruptions, breaks = input$bins, main = "")
    ##autoInvalidate()
    #hist(rnorm(isolate(input$n)))
    #hist(rnorm(input$n))
    #hist(rnorm(input$n))
    hist(rnorm(30))
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

  observe({
  output$currentCount <- renderValueBox({
    autoInvalidate2()
    valueBox(20*100, "Current footfall count (predicted)", icon = icon("street-view"), color = "yellow")
   }) 
  })
  
  output$todayaverage <- renderValueBox({
      valueBox(20*100, "Today's Average", icon = icon("universal-access"))
  }) 

  output$eventTimeRemaining <- renderValueBox({  #renderText
    numberSequence <- rep(input$m:1, 1000)
    #time_to_update <- 10
    invalidateLater(1000, session)
    time_to_update <- round(difftime(Sys.time(), EventTime, units='secs'))
     valueBox(
     print(paste(numberSequence[time_to_update], "secs", sep=" ")), icon = icon("clock-o"), "Time to next update:")

      })
  
  output$busmap <- renderLeaflet({
    camera_loc =read.table("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/misc/camera_locs.csv", sep=",", head=TRUE)
    leaflet(data = camera_loc[1:20,]) %>% addTiles() %>%
      addMarkers (~X_Lon, ~Y_Lat, popup = ~as.character(Id))
    
  })

})

