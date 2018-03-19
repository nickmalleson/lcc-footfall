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

#initiali.
canceller = 0

EventTime <- Sys.time() - 1*1
#numberSequence <- rep(input$m:1, 1000)

shinyServer(function(input, output, session){
  
  # Anything that calls autoInvalidate will automatically invalidate
  #every 2 seconds
  autoInvalidate <- reactiveTimer(1000, session)
  
 # numberSequence <- rep(10:1, 1000)
  
  ##observe({
    
    ##autoInvalidate()
    
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    ##print(paste("The value of input$n is", isolate(input$n)))
 ## })
  
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
  
  output$approvedSales <- renderInfoBox({
    # invalidateLater causes this output to automatically
    # become invalidated when input$interval milliseconds
    # have elapsed
    #infoBox("Approval Sales", "10,000,000", icon = icon("bar-chart-o"))
    infoBox("Approval Sales", "10,000,000",
            #invalidateLater(as.integer(input$interval), session)
            #format(Sys.time()),
            #format(Sys.time()), 
            icon = icon("bar-chart-o"))
    #invalidateLater(as.integer(input$interval), session)
    
    #format(Sys.time())
  })
  
 
  output$itemRequested <- renderValueBox({
      valueBox(15*200, "item requested by employee", icon = icon("fire"), color = "yellow")
  }) 

  #output$item2 <- renderValueBox({
    #valueBox(input$m, "item requested 22", icon = icon("fire"), color = "yellow")
  #}) 
  
  
  output$eventTimeRemaining <- renderValueBox({  #renderText
    numberSequence <- rep(input$m:1, 1000)
    #time_to_update <- 10
    invalidateLater(1000, session)
    time_to_update <- round(difftime(Sys.time(), EventTime, units='secs'))
     valueBox(
     print(paste(numberSequence[time_to_update], "Secs", sep=" ")), icon = icon("clock-o"), "Time to next update:")
     #print(paste(rep(m:1, 1000)[time_to_update], "Secs", sep=" ")), icon = icon("clock-o"), "Time to next update:")
     #rep(10:1, 1000)
     
      })

})

