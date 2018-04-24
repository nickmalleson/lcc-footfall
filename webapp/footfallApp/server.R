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
library(shinyBS)
library(emojifont)
require("ggrepel")
library(maps)
library(owmr)
library(data.table)
library(dplyr)
library(randomForest)
library(ROpenWeatherMap)

# Manually set the time zone
Sys.setenv(TZ="Europe/London")

#owmr package allows talking to ROpenWeatherMap website

#api key for downloading weather forecast...(http://openweathermap.org/)

owmr_settings("c8a930a2e30b695551e57d375a67d76e")  #Open weather forecast api key 


ROOT_DIR = "C:/Users/monsu/Documents/GitHub/"
#ROOT_DIR = "/Users/nick/research_not_syncd/git_projects/"


#option(digits.secs = 1)
EventTime <- Sys.time() - 1*1

#functions
#function to display number with thousand separator
th_separator <- function (x) format(round(as.numeric(x), 1), nsmall=0, big.mark=",")
# 

#funtion to print message to catch..
myPeriodicFunction <- function(){
  for(i in 1:5){
    msg <- paste(sprintf("Step %d done.... \n",i))
    cat(msg)
    Sys.sleep(1)
  }
}

#to detect if time (i.e. 'Hour') field is in 'hh:mm' format. 
#If so, round up to the nearest hour, by removing the last three character from behind i.e. ':00' 
#Anytime a data is imported, check the time format and convert 
convert_Time_Format <- function(data){
  backup_Hour <- data$Hour
  Hour_New <- matrix(0, length(data$Hour),1)
  pattern <- ":"
  timeString <- as.character(data$Hour)
  pattern_Exist <- grepl(pattern, timeString)
  whichIsTrue <- which(pattern_Exist==TRUE)
  whichIsFalse <- which(pattern_Exist!=TRUE)
  HourCut <- as.numeric(as.vector(substr(timeString,1,2)))
  Hour_New[whichIsTrue,1] <- HourCut[whichIsTrue]
  Hour_New[whichIsFalse,1] <- backup_Hour[whichIsFalse]
  data$Hour <- Hour_New
}



#subset data, colleting the necessary fields: 'Date', 'Hour', 'Id' & 'LocationName'
subset_Dataset <- function(orig_Data, cameraLoc = "LocationName"){
  #create unique field for the dataset#
  #convert date to appropriate format
  orig_Data_Conv <- convert_Date(orig_Data, TimeField = TRUE) #### #head(orig_Data_Conv) #nrow(orig_Data_Conv)
  
  #create unique field (i.e. combination of date and time)
  unique_field <- matrix(paste(orig_Data_Conv$Date, orig_Data_Conv$Hour, sep="-"),,1)
  colnames(unique_field) <- c("Id")
  
  cam_ID <- which(colnames(orig_Data)==cameraLoc)
  #append to real data
  #orig_Data_sub <- cbind(orig_Data_Conv$Date, orig_Data$Hour, unique_field, orig_Data$InCount, as.character(orig_Data[,cam_ID]))   #head(length(which(orig_Data=="2011-01-01-0"))  
  orig_Data_sub <- cbind(orig_Data_Conv$Date, orig_Data_Conv$Hour, unique_field, orig_Data_Conv$InCount, as.character(orig_Data_Conv[,cam_ID]))   #head(length(which(orig_Data=="2011-01-01-0"))  
  #head(orig_Data_sub)
  orig_Data_sub<-as.data.frame(orig_Data_sub)
  colnames(orig_Data_sub) <- c("Date","Hour","Id","InCount","Loc_Id") #head(orig_Data_sub)
  return(orig_Data_sub)
}

#------------------------
#function to deal with typo in camera name


#---------------------------------------------------------
#function to correct typo in Camera's Location

# data_Location_Typo_removed <- function(data, lists_Loc_Correct){
#   
#   #are these all the camera locations expected
#   unique_Camera <- as.vector(unique(data$LocationName))  #head(data)
#   
#   matrix_Loc <- matrix(0, length(data$LocationName), 1)
#   
#   listId <- which(data$LocationName=="BriggateAtMcDs")
#   matrix_Loc[listId, 1] <- rep("Briggate at McDonalds", length(listId))
#   
#   listId <- which(data$LocationName=="Briggate at McDonalds\t")
#   matrix_Loc[listId, 1] <- rep("Briggate at McDonalds", length(listId))
#   
#   listId <- which(data$LocationName=="CommercialStLush")
#   matrix_Loc[listId, 1] <- rep("Commercial Street at Sharps", length(listId))
#   
#   listId <- which(data$LocationName=="CommercialStBarratts")
#   matrix_Loc[listId, 1] <- rep("Commercial Street at Barratts", length(listId))
#   
#   listId <- which(data$LocationName=="Dortmund Square\t")
#   matrix_Loc[listId, 1] <- rep("Dortmund Square", length(listId))
#   
#   listId <- which(data$LocationName=="DortmundSq")
#   matrix_Loc[listId, 1] <- rep("Dortmund Square", length(listId))
#   
#   listId <- which(data$LocationName=="AlbionStNorth")
#   matrix_Loc[listId, 1] <- rep("Albion Street North", length(listId))
#   
#   listId <- which(data$LocationName=="AlbionStSouth")
#   matrix_Loc[listId, 1] <- rep("Albion Street South", length(listId))
#   
#   listId <- which(data$LocationName=="Briggate")
#   matrix_Loc[listId, 1] <- rep("Briggate", length(listId))
#   
#   listId <- which(data$LocationName=="Briggate at McDonalds")
#   matrix_Loc[listId, 1] <- rep("Briggate at McDonalds", length(listId))
#   
#   listId <- which(data$LocationName=="Commercial Street at Sharps")
#   matrix_Loc[listId, 1] <- rep("Commercial Street at Sharps", length(listId))
#   
#   listId <- which(data$LocationName=="Commercial Street at Barratts")
#   matrix_Loc[listId, 1] <- rep("Commercial Street at Barratts", length(listId))
#   
#   listId <- which(data$LocationName=="Headrow")
#   matrix_Loc[listId, 1] <- rep("Headrow", length(listId))
#   
#   listId <- which(data$LocationName=="Dortmund Square")
#   matrix_Loc[listId, 1] <- rep("Dortmund Square", length(listId))
#   
#   listId <- which(data$LocationName=="Albion Street South")
#   matrix_Loc[listId, 1] <- rep("Albion Street South", length(listId))
#   
#   listId <- which(data$LocationName=="Albion Street North")
#   matrix_Loc[listId, 1] <- rep("Albion Street North", length(listId))
#   
#   matrix_Loc <- as.data.frame(matrix_Loc)
#   colnames(matrix_Loc) <- c("LocationName")
#   
#   #append to the real data
#   data[,"LocationName"] <- matrix_Loc
#   
#   return(data)
# }


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
  
  for(i in 1:length(uniqId)){ #i<-2
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
    #flush.console()
    #print(paste("point2", i, length(uniqId)))
  }
  
  #  write.table(loc_agg_data, file="backuploc_agg_data.csv", sep=",", row.names=FALSE) 
  
  #clean it up
  loc_agg_data <- loc_agg_data[-which(loc_agg_data[,1]=="0"),]  #head(loc_agg_data)
  if(nrow(loc_agg_data)!=0){
    rownames(loc_agg_data) <- 1:nrow(loc_agg_data)
    loc_agg_data <- as.data.frame(loc_agg_data)
    colnames(loc_agg_data) <- c("Date","Hour","Id","InCount")
    return(loc_agg_data)}
  
  if(nrow(loc_agg_data)==0){
    loc_agg_data = 1
    return(loc_agg_data)}
  
}

#-----------------------------------


#Given a footfall dataset (containing a column 'Id' - concatenation of unique day-time), sum all 'InCount' by unique 'Hour' of the day
footfall_by_time_of_the_Day <- function(loc_agg_data, time_aggre){
  #---------------------allows all dates to be seen
  #create list of all days between two range (i.e. start and end date of historical footfall dataset)
  start_date <- min(uniq_Dates(loc_agg_data)) #library(lubridate) #suppress warning...
  end_date <- max(uniq_Dates(loc_agg_data))
  
  allDays_listed <- seq(as.Date(start_date), as.Date(end_date), by=1)
  
  #how do you approximate it..forward or back
  #create list of days and time, covering the entire study period (start of the footfall database and its end).
  allDays_Time_listed <- merge(allDays_listed, c(0:23), all = TRUE, sort = FALSE)#time_aggre
  allDays_Time_listed <- allDays_Time_listed[order(allDays_Time_listed[,1]),]
  
  #combine date and time to create a unique field
  unique_allDays_Time_listed <- paste(allDays_Time_listed$x, allDays_Time_listed$y, sep="-")
  
  unique_allDays_Time_listed_join <- cbind(allDays_Time_listed, unique_allDays_Time_listed)  #head(orig_Data)
  unique_allDays_Time_listed_join <- as.data.frame(unique_allDays_Time_listed_join)
  colnames(unique_allDays_Time_listed_join) <- c("Date","Hour","Id")
  
  #Join 'InCount' values from 'orig_Data' to 'unique_allDays_Time_listed_join', using the 'Id' fields
  merge_Data <- merge(x = unique_allDays_Time_listed_join, y = loc_agg_data, by = "Id", all.x = TRUE)
  merge_Data <- as.data.frame(merge_Data)
  
  #create a subset of the data with the fields: 'Date.x', 'Hour.x', and 'InCount'
  data_subset <- merge_Data[, c("Date.x", "Hour.x", "InCount")]   
  data_subset  <- as.data.frame(data_subset)
  colnames(data_subset) <- c("Date","Hour","InCount")   #head(data_subset[1:10,])
  
  #create aggregated data to plot i.e. for each time scale; 24-hours, morning, evening & morning.
  unique_dates <- unique(data_subset$Date)
  unique_Times <- time_aggre 
  
  
  time_agg_data <-  matrix(0, 1, 2)
  rownames(time_agg_data) <- length(unique_Times)
  row.N <- "100" #just any number different from 'length(unique_Times)' 
  appd_Row <- matrix(0, 1, 2)
  rownames(appd_Row) <- length(unique_Times)
  
  for(i in 1:length(unique_dates)){ #i<-1
    timeT <- data_subset[which(data_subset$Date==unique_dates[i]),c("Hour")]
    totalCt_allDay <- data_subset[which(data_subset$Date==unique_dates[i]),]
    #sum the 'InCount values for the time selected aggregate'
    totalCt <- sum(as.numeric(as.vector(totalCt_allDay[which(totalCt_allDay$Hour%in%unique_Times),c("InCount")])))
    
    #check if all the unique_times are present
    true_Ct <- length((unique_Times%in%totalCt_allDay$Hour)=="TRUE")
    combine_Result <- cbind(as.character(unique_dates[i]), totalCt)
    time_agg_data[which(rownames(time_agg_data)==length(unique_Times)),] <- as.vector(combine_Result)
    rownames(time_agg_data) <- rep("", nrow(time_agg_data))
    
    time_agg_data <- rbind(time_agg_data, appd_Row)
  }
  #clean it up
  time_agg_data <- time_agg_data[-which(time_agg_data[,1]=="0"),]
  rownames(time_agg_data) <- 1:nrow(time_agg_data)
  time_agg_data <- as.data.frame(time_agg_data)
  colnames(time_agg_data) <- c("Date","InCount")
  
  return(time_agg_data)
}
#------------------------------------------

#function to identify outliers
outliers <- function(data=aggregate_time_of_the_Day){
  x<-data
  hold_result <- matrix(0, nrow(x), 1)
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

# plot function for the 4 prediction panels
auc_plot <- function(y, plotStyle=1){
  
  x <- 1:length(y)
  n <- length(y)
  #using ggplot2
  if(plotStyle==1){
    xy_1 <- as.data.frame(cbind(x, y))
    xy_1Type <- rep(1, nrow(xy_1))
    #xy_1Type[length(xy_1Type)] <- 2  #changing the type of the last point, so that it can be colored differently
    xy_1 <- data.frame(xy_1Type,  xy_1)
    # a <- ggplot(data=xy_1, aes(x=x, y=y)) + geom_line() + geom_point()   # Left (to compare)  #gam for n > 1000.
    # print(a)
    #which(is.na(xy_1$InCount))
    
    #plot(xy_1$Date, xy_1$InCount)  
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

# plot function for the big HF panel
auc_plot2 <- function(data, HF_startDate, plot_StartDate = 0, predicted_Point = y_new, addTrend = FALSE, chartType="Dot"){
  
  #create list of all days between the start date HF data collection and the current time
  start_date <- HF_startDate
  #end_date <- Sys.Date()
  end_date <- as.Date("2019-12-31")
  
  allDays_inbetween <- matrix(as.character(seq(as.Date(start_date), as.Date(end_date), by=1)),,1)
  colnames(allDays_inbetween) <- c("Date")  #mode(allDays_inbetween) #mode(data)
  
  #Join 
  merged_Datasetd <- merge(x = allDays_inbetween, y = data, by = "Date", all.x = TRUE, all.y = TRUE)
  merged_Datasetd  <- as.data.frame(merged_Datasetd)
  
  #flush.console()
  #print(merged_Datasetd)
  
  #combine historical data and predicted.
  x <- c(as.character(as.Date(merged_Datasetd$Date)), as.character(as.Date(predicted_Point$Date)))
  y <- c(merged_Datasetd$InCount, predicted_Point$InCount)
  Outliers <- c(merged_Datasetd$outlier, predicted_Point$outlier)
  
  x_backup <- x
  dateLabels = seq(as.Date("2009/01/01"), as.Date("2019-01-01"), by = "year")
  
  #using ggplot2
    xy_1 <- as.data.frame(cbind(x, y))  
    xy_1Type <- rep(1, nrow(xy_1))
    xy_1Type[length(xy_1Type)] <- 2  #changing the type of the last point, so that it can be colored differently
    xy_1 <- data.frame(xy_1Type,  xy_1)
    
 
    colnames(xy_1) <- c("Type","x","y")   #, "Outliers")

    x<-xy_1$x
    y<-xy_1$y
    #to adjust the start of plot

    Type <- as.numeric(xy_1$Type)[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
    Date <-as.numeric(xy_1$x)[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
    current_Date_Index <- as.numeric(Sys.Date() - HF_startDate)
    flush.console()
    print(current_Date_Index)
    print("...........................................")
    #x <- as.Date(as.vector(xy_1$x))
    InCount <- as.numeric(as.vector(xy_1$y))[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
    #Outliers <-  as.numeric(as.vector(xy_1$Outliers))[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
    
    xy_1 <- data.frame(Type, Date, InCount) #, Outliers)

    #overall mean of the time series
    mean_InCount <-  mean(xy_1$InCount)
    
    getUnique_year <- as.vector(allDays_inbetween)
    sub_getUnique_year <- as.vector(substr(data$Date, 1, 4))
    most_recent_year <- max(unique(sub_getUnique_year))
    indEX1 <- which(substr(getUnique_year, 1,4) == most_recent_year)[1]
    indEX2 <- length(allDays_inbetween)
    ind_Foot <- data[which(substr(data$Date[order(data$Date)], 1,4) == most_recent_year), c("InCount")]
    percentiles <- round(as.vector(quantile(ind_Foot, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE)), digits=0)   # quartile
    
    flush.console()
    print(ind_Foot[1])
    print("==============================================GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG==============================================")
    
if(chartType=="Dot"){
  if(addTrend==FALSE){
    print(ggplot(xy_1, aes(Date, InCount, group=Type)) +

            geom_vline(xintercept = min(Date),
                       color = "grey", size=1.5) +
            #geom_vline(xintercept = 2000, linetype="dotted",
            #color = "red", size=1.5) +
            geom_hline(yintercept=0,
                       color = "grey", size=1.5) +

            geom_vline(xintercept = min(Date), linetype="dashed",
                       color = "brown", size=0.5) + #current date

            geom_vline(xintercept = current_Date_Index, linetype="dashed",
                       color = "grey", size=1) + #current date
            
            geom_point(color= c(rep("blue", (length(Type)-1)), "red") , size = c(rep(1, (length(Type)-1)), 4)  ) +
            
            geom_hline(aes(yintercept = mean(InCount, na.rm = T)), linetype="dashed",
                       color = "green", size=1) +
            
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[1],yend=percentiles[1]), linetype="dashed", color = "red", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[2],yend=percentiles[2]), linetype="dashed", color = "orange", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[3],yend=percentiles[3]), linetype="dashed", color = "yellow", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[4],yend=percentiles[4]), linetype="dashed", color = "orange", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[5],yend=percentiles[5]), linetype="dashed", color = "red", size=0.5) +

            scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
          #scale_x_discrete(labels = x_backup)
    ) }

  if(addTrend==TRUE){
    print(ggplot(xy_1, aes(Date, InCount, group=Type)) +
 
            geom_vline(xintercept = min(Date),
                       color = "grey", size=1.5) +
            #geom_vline(xintercept = 2000, linetype="dotted",
            #color = "red", size=1.5) +
            geom_hline(yintercept=0,
                       color = "grey", size=1.5) +

            geom_vline(xintercept = min(Date), linetype="dashed",
                       color = "brown", size=0.5) + #current date

            geom_vline(xintercept = current_Date_Index, linetype="dashed",
                       color = "grey", size=1) + #current date
            
            geom_point(color= c(rep("blue", (length(Type)-1)), "red") , size = c(rep(1, (length(Type)-1)), 4)  ) +
            
            geom_hline(aes(yintercept = mean(InCount, na.rm = T)), linetype="dashed",
                       color = "green", size=1) +

            geom_smooth(method = "lm", se=FALSE, color="red", lwd = 2) +
            
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[1],yend=percentiles[1]), linetype="dashed", color = "red", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[2],yend=percentiles[2]), linetype="dashed", color = "orange", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[3],yend=percentiles[3]), linetype="dashed", color = "yellow", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[4],yend=percentiles[4]), linetype="dashed", color = "orange", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[5],yend=percentiles[5]), linetype="dashed", color = "red", size=0.5) +

            scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
          #scale_x_discrete(labels = x_backup)
    ) }
}

  #to generate regular plot
  if(chartType=="Line"){
    if(addTrend==FALSE){
      print(ggplot(xy_1, aes(Date, InCount, group=Type)) +
              geom_line(color="blue", size = 0.5) +
  
              geom_vline(xintercept = min(Date),
                         color = "grey", size=1.5) +
              #geom_vline(xintercept = 2000, linetype="dotted",
              #color = "red", size=1.5) +
              geom_hline(yintercept=0,
                         color = "grey", size=1.5) +

              geom_vline(xintercept = min(Date), linetype="dashed",
                         color = "brown", size=0.5) + #current date

              geom_vline(xintercept = current_Date_Index, linetype="dashed",
                         color = "grey", size=1) + #current date
              
              geom_point(color= c(rep("blue", (length(Type)-1)), "red") , size = c(rep(1, (length(Type)-1)), 4)  ) +
              
              geom_hline(aes(yintercept = mean(InCount, na.rm = T)), linetype="dashed",
                         color = "green", size=1) +
              
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[1],yend=percentiles[1]), linetype="dashed", color = "red", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[2],yend=percentiles[2]), linetype="dashed", color = "orange", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[3],yend=percentiles[3]), linetype="dashed", color = "yellow", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[4],yend=percentiles[4]), linetype="dashed", color = "orange", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[5],yend=percentiles[5]), linetype="dashed", color = "red", size=0.5) +
              
              scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
            #scale_x_discrete(labels = x_backup)
      ) }

    if(addTrend==TRUE){
      print(ggplot(xy_1, aes(Date, InCount, group=Type)) +
              geom_line(color="blue", size = 0.5) +
 
              geom_vline(xintercept = min(Date),
                         color = "grey", size=1.5) +
              #geom_vline(xintercept = 2000, linetype="dotted",
              #color = "red", size=1.5) +
              geom_hline(yintercept=0,
                         color = "grey", size=1.5) +

              geom_vline(xintercept = min(Date), linetype="dashed",
                         color = "brown", size=0.5) + #current date

              geom_vline(xintercept = current_Date_Index, linetype="dashed",
                         color = "grey", size=1) + #current date
              
              geom_point(color= c(rep("blue", (length(Type)-1)), "red") , size = c(rep(1, (length(Type)-1)), 4)  ) +
              
              geom_hline(aes(yintercept = mean(InCount, na.rm = T)), linetype="dashed",
                         color = "green", size=1) +

              geom_smooth(method = "lm", se=FALSE, color="red", lwd=1) +
              
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[1],yend=percentiles[1]), linetype="dashed", color = "red", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[2],yend=percentiles[2]), linetype="dashed", color = "orange", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[3],yend=percentiles[3]), linetype="dashed", color = "yellow", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[4],yend=percentiles[4]), linetype="dashed", color = "orange", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[5],yend=percentiles[5]), linetype="dashed", color = "red", size=0.5) +

              scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
            #scale_x_discrete(labels = x_backup)
      ) }
    
 
    }
  }
  
 # }
#}

#function to plot...points and lines for forecast
auc_plot3 <- function(y){ #, chartType="Dot"
  
  xy_1 <- y
  
  print(xy_1)
  print("GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGDDDDDDDDDDDDDDDDDDDDDDDWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW")
  
  labs <- data.frame(xy_1Type=c(1, 2),
                     label = fontawesome(c('fa-arrow-circle-up','fa-arrow-circle-down'))  )
  
  d <- merge(xy_1, labs, by="xy_1Type")[order(merge(xy_1, labs, by="xy_1Type")[,2]),]
  d$Perc =  d$Perc * -1
  dateLabels = seq(Sys.Date()+1, (Sys.Date()+1 +(nrow(d)-1)), by = "day")
  dayLabels =   weekdays(as.Date(dateLabels))

  d <- cbind(d, dateLabels)

    
  #if(chartType=="Line-Dot"){  #https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
    print(ggplot(d, aes(Date, InCount)) + #ylim(-1,max(50)) +
      geom_point(aes(Date, InCount, color=factor(xy_1Type)), size = 1) +
      theme(legend.position=" ") +
      geom_ribbon(aes(ymin=0, ymax=InCount), alpha=0.1, fill="blue") +
      geom_line(color="blue", size = 1.5)+ 
      geom_point(aes(Date, InCount, color=factor(xy_1Type)), size = 11) +
      geom_text(aes(Date, InCount,label=label),family='fontawesome-webfont', size=c(9)) + #nudge_x=0, nudge_y=0
        scale_x_discrete(limits=d$Date,labels=dateLabels) + 
        
      annotate(geom = "text", x = d$Date, y = (min(d$InCount)-(min(d$InCount)/3)), label = dayLabels, size = 4) +
        
      coord_cartesian(ylim = c((min(d$InCount)-(min(d$InCount)/3)), (max(d$InCount)+(max(d$InCount)/8)))) + 
        
      geom_text_repel(
       aes(Date, InCount, color=factor(xy_1Type), label=paste(Perc,"%", sep="")),
       size = 5,
       nudge_x = 0, nudge_y = 0.5,
       fontface = 'bold',
       box.padding=0.5, point.padding = 1.6, segment.size = 0)
    )
}


#------------------------
#function to calculate percentage increase in prediction
vector_perc_diff <- function(data){
  table_R <- matrix(0, length(data)-1, 4)
  for (i in 2:length(data)){ #i<-2
    table_R[i-1,1] <- i-1
    table_R[i-1,2] <- data[i]
    table_R[i-1,3] <- round(((data[i-1]-data[i])/data[i-1])*100, digits=0)
    if(table_R[i-1,3]<=0){table_R[i-1,4] = 1}
    if(table_R[i-1,3]>0){table_R[i-1,4] = 2}
    #table_R[i-1,5] <- as.character(Sys.Date()+(i-1))
  }
  table_R <- data.frame(table_R)
  colnames(table_R) <- c("Date", "InCount", "Perc", "xy_1Type")#, "Date")
  return(table_R)
}

# labs <- data.frame(var=c("xy_1Type"),
#                    label = fontawesome(c('fa-arrow-circle-up'))  )

# labs <- data.frame(var=c("var1", "var2"),
#                    label = fontawesome(c('fa-arrow-circle-up','fa-arrow-circle-down'))  )
# d <- merge(data, labs, by.x="var", by.y="var")

# ggplot(d,aes(x=id,y=count,color=var))+
#   geom_text(aes(label=label),family='fontawesome-webfont', size=8)+
#   #geom_label_repel(aes(label = count,
#   #fill = factor(var)), color = 'white',
#   #size = 3.5) +
#   #theme(legend.position = "bottom")+
#   geom_line()+
#   geom_text_repel(
#     data = subset(d, var == "var1"), aes(label = count),
#     size = 5,
#     #box.padding = unit(0.5, "lines"),
#     point.padding = unit(0.3, "lines")
#   )

# Set up API key
# library(owmr)
# owmr_settings("c8a930a2e30b695551e57d375a67d76e")
# get_forecast("leeds", cnt = 120)
# get_forecast("london", cnt = 100)

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
  dateT <- Sys.Date() - 1
  #date_time2 <- Sys.Time()
  # <- substr(as.character(date_time), 1, 10)
  timeT <- substr(as.character(dateT), 11, 20)
  dayT <- weekdays(as.Date(dateT))
  print(paste(dayT, ", ", dateT, sep=""))}

#function to display tomorrow's day in the forecast panels
day_function <- function(){
  dateD <- Sys.Date()
  dayT <- paste(weekdays(as.Date(dateD)), ", ", (Sys.Date()), sep = "")
  print(dayT)}


#function to list the gaps in the dates field of a datasets
missingData <- function(data, indicatorField = FALSE){
  #to subset the dataset first based on field on interest (specifically to deal with 'predictors' table)
  if(indicatorField==TRUE){
    data = data[which(as.vector(data$Date) < Sys.Date()),]
    #data = data[which(as.vector(data$Date) < Sys.Date()),]
  }
  dataValues <- data$Date
  DateFromData <- as.character(dataValues)
  dateField <- matrix(DateFromData,,1)
  colnames(dateField) <- c("Date") 
  #to detect dates not in right format (i.e. yyyy-mm-dd)
  converDate1 <- as.Date(parse_date_time(dateField,"dmy"))
  listInconvertible <- which(!is.na(converDate1))
  dateField[listInconvertible] <- as.character(converDate1[listInconvertible])   #data[89480:89559,]
  #append back to the dataset
  #dataValues <- cbind(min(dateField), "missing dates(months)")
  dataValues <- dateField   
  dataValues <-   matrix(dataValues[order(dataValues[,1]),],,1)
  #append current date to the list..
  dataValues <- rbind(dataValues, as.character(Sys.Date()))
  ##dataValues <- c(dataValues, as.character(Sys.Date()))
  
  #for 'predictor' table, remove those whose 'temp' and 'rain' column have been updated (i.e. with entry '1')
  if(indicatorField==TRUE){
    dataIndd = which(data$status == 0)
    dataValues = dataValues[-dataIndd,] #isolate the 'yet-to-be-updated' dates
  }
  
  #to identify gaps in the dataset.
  DF <- as.Date(dataValues)
  DF_Dates <- diff(DF)
  #missing_Dates <-   data.frame(from = (DF[DF_Dates>1]+1), to = (DF[c(1, DF_Dates)>1]-1), No_of_days = (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]))
  #missing_Dates <-   data.frame(from = (DF[DF_Dates>1]+1), to = (DF[c(1, DF_Dates)>1]-1), No_of_days = (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]))
  missing_Dates <-  data.frame((DF[DF_Dates>1]+1), (DF[c(1, DF_Dates)>1]-1), (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]))
  colnames(missing_Dates) <- c("from","to","No_of_days")
  appdI <- matrix("2000-03-03",1,3)
  colnames(appdI) <- c("from","to","No_of_days")
  missing_Dates <- rbind(missing_Dates, appdI)
  #remove dates with less than one day
  missing_Dates <- missing_Dates[which(as.vector(missing_Dates[,3]) != 1),]
  #if number of rows of the table is greater than 3, remove the last row
  
  if(nrow(missing_Dates)==1){ #>2
    missing_Dates = matrix(0, 0, 3)
    colnames(missing_Dates) <- c("from","to","No_of_days")
    missing_Dates <- as.data.frame(missing_Dates)
  }
  
  if(nrow(missing_Dates)>2){ #>2
    missing_Dates = missing_Dates[-nrow(missing_Dates),]
    ##missing_Dates = missing_Dates[(4:nrow(missing_Dates)),]
  }
  #if 'missing_Dates' is one row, 
  if(nrow(missing_Dates)==2){ #>2
    missing_Dates <- as.data.frame(missing_Dates[1,])
  }
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

#function to convert list of dates to format 'yyyy-mm-dd'. Returns the dataset, and with date converted to date type
convert_Date <- function(data, TimeField = FALSE){
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
  data$Date <- dateField
  if(TimeField==TRUE){
    #ensure "Hour" field is in right format i.e. 0, 1, 2, ....23.
    dataValues_t <- data$Hour
    hourField <- as.numeric(substr(dataValues_t, 1, 2)) 
    data$Hour <- hourField
  }
  return(data)
}



#LIST FOR FUNCTIONS TO CHECK ERRORS IN THE DATASET
#function to check that uploaded contains the three fields, "Date","Hour","InCount", "LocationName"
uploaded_fieldnames <- function(data, essential_Fields){
  #essential_Fields <- c("Date","Hour","InCount", "LocationName")
  names_uploaded <- essential_Fields %in% colnames(data)
  leng_name <- length(which(names_uploaded=="TRUE"))
  return(leng_name)
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

#function to check whether there is overlap in the dates:
dateOverlap_Checker <- function(history_footfall, data){
  #unique dates in the footfall (database) data
  uniqueDate_footfallDatabase <- uniq_Dates(history_footfall)
  #unique dates in the uploaded data
  uniqueDate_uploaded <- uniq_Dates(data)
  #check that there is no overlap between the dates dates
  overlap_Dates <- which(uniqueDate_uploaded %in% uniqueDate_footfallDatabase)
  return(length(overlap_Dates))
}

#to detect if time (i.e. 'Hour') field is in 'hh:mm' format. 
#If so, return a error warning
detect_Time_Format_Error <- function(data){
  backup_Hour <- data$Hour
  Hour_New <- matrix(0, length(data$Hour),1)
  pattern <- ":"
  timeString <- as.character(data$Hour)
  pattern_Exist <- grepl(pattern, timeString)
  whichIsTrue <- which(pattern_Exist==TRUE)
  if(length(whichIsTrue)==0){timeF = 0}
  if(length(whichIsTrue)>0){timeF = 1}
  return(timeF)
}

# #function to remove trailing and leading whitespace in the names of camera location
# check_typo_in_Camera_Name <- function(data, lists_Loc_Correct){
#   #are these all the camera locations expected
#   unique_Camera_Loc <- as.vector(lists_Loc_Correct)  #head(orig_Data_sub)
#   #remove whitespaces in the location names  
#   vec_Name <- trimws(as.vector(data$LocationName), which="right") #trailing whitespace
#   vec_Name <- trimws(vec_Name, which="left") #leading whitespace
#   unique_Camera_Loc_from_Data <- unique(vec_Name)
#   check_Loc <- length(which((unique_Camera_Loc_from_Data%in%unique_Camera_Loc)==FALSE))
#   if(check_Loc==0){issue0=0}
#   if(check_Loc!=0){issue0=1}
#   return(issue0)
# }
#

#function to remove whitespace in the names of camera location
remove_whiteSpace_in_Camera_Name <- function(data, lists_Loc_Correct){
  vec_Name <- trimws(as.vector(data$LocationName), which="right") #trailing whitespace
  vec_Name <- trimws(vec_Name, which="left") #leading whitespace
  vec_Name <- matrix(vec_Name,,1)
  colnames(vec_Name) <- "LocationName"
  data$LocationName <- vec_Name
  return(data)
}


#function to detect typo in the list of camera location
check_typo_in_Camera_Name <- function(data, lists_Loc_Correct){
  #are these all the camera locations expected
  unique_Camera_Loc <- as.vector(lists_Loc_Correct)  #head(orig_Data_sub)
  #remove whitespaces in the location names  
  vec_Name <- trimws(as.vector(data$LocationName), which="right") #trailing whitespace
  vec_Name <- trimws(vec_Name, which="left") #leading whitespace
  unique_Camera_Loc_from_Data <- unique(vec_Name)
  check_Loc <- length(which((unique_Camera_Loc_from_Data%in%unique_Camera_Loc)==FALSE))
  if(check_Loc==0){issue0=0}
  if(check_Loc!=0){issue0=1}
  return(issue0)
}

#function to remove trailing and leading white spaces in column ('LocationName')
remove_whitespace <- function(data){                           #head(data)
  vec_Name <- as.vector(data$LocationName)
  vec_Name <- trimws(vec_Name, which="right")
  vec_Name <- trimws(vec_Name, which="left")
  vec_Name <- matrix(vec_Name,,1)
  colnames(vec_Name) <- c("LocationName")
  data[, "LocationName"] <- vec_Name
  return(data)
}


#Preparing dataset for training
#function to merge footfall aggregate with predictors 
data_Preparation_for_training <- function(aggre_footfall, predictors, modelName ="randomForest", training_length_in_yrs = 3) {
  
  # extract days with status 1 (i.e. rows with weather information)
  predictors_info_extract <- predictors[which(predictors$status==1),]  
  
  #convert dates to right format
  predictors_info_extract <- convert_Date(predictors_info_extract, TimeField = FALSE)	
  
  #order by dates and extract the mode recent last three years
  predictors_info_extract_subset <- predictors_info_extract[order(predictors_info_extract$Date, decreasing = TRUE),]
  predictors_info_extract_subset <- predictors_info_extract_subset[1:(training_length_in_yrs*365), ] 
  
 
  #remove the outlier and "NA", and drop the outlier 'column'
  dayTime_HF_aggre_MINUS_outlier <- aggre_footfall[which(aggre_footfall$outlier==2),]
  dayTime_HF_aggre_MINUS_outlier <- subset(dayTime_HF_aggre_MINUS_outlier, select=-c(outlier))
  
  #To ensure that the 'Date' column in both datasets (predictor dataset and Footfal datasets)are in the right format
  dayTime_HF_aggre_MINUS_outlier <- convert_Date(dayTime_HF_aggre_MINUS_outlier, TimeField = FALSE)
  predictors_info_extract_subset <- convert_Date(predictors_info_extract_subset, TimeField = FALSE)
  
  #now merge both datasets using the 'Date' column
  setDT(dayTime_HF_aggre_MINUS_outlier)
  setDT(predictors_info_extract_subset)
  
  joined_dataset_for_Training <- dayTime_HF_aggre_MINUS_outlier[predictors_info_extract_subset, on = c('Date','Date')]
  
  #remove records with InCount == "NA"
  joined_dataset_for_Training <- joined_dataset_for_Training[which(joined_dataset_for_Training$InCount!="NA"),]
  
  #drop "Date.1" & "status" columns
  joined_dataset_for_Training <- subset(joined_dataset_for_Training, select=-c(Date, Date.1, status))
  
  return(joined_dataset_for_Training)
}


#function to convert temp from Kelvin to Celcius
Data_kelvin_to_celsius <- function(data) {
  #Converts Kelvin to Celsius
  data$main.temp <- data$main.temp - 273.15
  return(data)
}



#to restrict the file upload size to 120MB
options(shiny.maxRequestSize=200*1024^2) 

#----------------------------------------------------------

shinyServer(function(input, output, session){

  #set current date for the calender
  # output$change_Date <- renderText({ 
  #   return(Sys.Date())
  #   })
  
  output$dateText  <- renderText({
    paste("input$date is", as.character(input$date))
  })

  lists_Loc_Correct <- c("Briggate", "Briggate at McDonalds", "Commercial Street at Sharps",
                         "Commercial Street at Barratts", "Headrow", "Dortmund Square",
                         "Albion Street South", "Albion Street North")
  
  
  #start date of HF data collection
  HF_startDate <- as.Date("2009-01-01")
  
  #setting the directories
  #directory for the historical HF
  HF_directory = paste0(ROOT_DIR,"lcc-footfall/webapp/downloaded_footfall dataset/historical_HF/")
  #directory for the aggregated HF
  file_here <- paste0(ROOT_DIR,"lcc-footfall/webapp/downloaded_footfall dataset/aggregated_historical_HF/")
  #parameter file directory
  parameter_directory <- paste0(ROOT_DIR,"lcc-footfall/webapp/downloaded_footfall dataset/")
  
  #directory for other items
  other_dir <- paste0(ROOT_DIR, "lcc-footfall/webapp/misc/")
  
  #IMPORTING DATASETS
  #history_footfall <- do.call("rbind", lapply(list.files(HF_directory,
                                                  #full=TRUE),read.csv, header=TRUE))
  history_footfall <- read.table(file=paste(HF_directory, "subset_historical_HF_DoNot_REMOVE_or_ADD_ToThisDirectory", ".csv", sep=""), sep=",", head=TRUE) 
  
  #import the predictor information
  predictors_info <- read.table(file=paste(parameter_directory, "predictors_INFO/", "predictors_info", ".csv", sep=""), sep=",", head=TRUE) 
  
  #extract the predictors info that have weather information.
  predictors_info_extract <- predictors_info[which(predictors_info$status==1),]  #head(predictors_info_extract)
  
  
  #dayTime_HF_aggre <- read.table(file=paste(file_here, "dayTimeAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)
  #dayTime_HF_aggre <- convert_Date(dayTime_HF_aggre)
  #eveningTime_HF_aggre <- read.table(file=paste(file_here, "eveningTimeAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)
  #nightTime_HF_aggre <- read.table(file=paste(file_here, "nightTimeAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)
  twentyFourHours_HF_aggre <- read.table(file=paste(file_here, "twentyFour_HoursAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)
  twentyFourHours_HF_aggre <- convert_Date(twentyFourHours_HF_aggre) 
  
 
    #reverse the table
  hist_table <- apply(history_footfall, 2, rev)
  
  output$history <- renderDataTable(hist_table)
  # historical_footfall <- read.table(file=ROOT_DIR+"/lcc-footfall/webapp/downloaded_footfall dataset/historical_footfall/historical_footfall_up_to_31_12_2016.csv", sep=",", head=TRUE)
  # history_footfall <- historical_footfall
  

  #HISTORICAL HF VISUALISATION
  # output$dayTimeData <- DT::renderDataTable({
  #    dayTIme_HT_Table <- DT::datatable(dayTime_HF_aggre)
  #    return(dayTIme_HT_Table)
  #  })
  

  # output$eveningTimeData <- DT::renderDataTable({
  #   eveningTimeData <- DT::datatable(eveningTime_HF_aggre)
  #   return(eveningTimeData)
  # })
  # 
  # output$nightTimeData <- DT::renderDataTable({
  #   nightTimeData <- DT::datatable(nightTime_HF_aggre)
  #   return(nightTimeData)
  # })

  output$twentyFourHoursData <- DT::renderDataTable({
    twentyFourHours_HT_Table <- DT::datatable(twentyFourHours_HF_aggre)
    return(twentyFourHours_HT_Table)
  })

  output$cameraTitle = renderText({paste("<b> Names of Camera Location")})
  
  output$cameraLocation = renderText({paste("1.   Albion Street North", "<br>", "2.   Albion Street South", "<br>", "3.   Briggate", "<br>", "4.   Briggate at McDonalds",
                                             "<br>", "5.   Commercial Street at Barratts", "<br>","6.   Commercial Street at Sharps","<br>","7.   Dortmund Square","<br>",
                                            "8.   Headrow")})

  output$warning_cameraLocation = renderText({paste("Note: Before uploading any files to either replace the existing HF records or update the records", "<br>", 
                                                    "in the 'Historical Footfall (HF)' and 'Update HF' tabs respectively, ensure that the spellings of the camera (location) names","<br>",
                                                    "are exactly as typed here. Also, watch out for leading and trailing whitespaces in the names.")})

  
  #tab 'Update Predictors'
  
  #Summary of missing Predictors information (Specifically weather Information)
  
  
  #create a list dates occuring in the dataset
  missData_Predictors <- missingData(data=predictors_info, indicatorField = TRUE)
  
  flush.console()
  print(missData_Predictors)
  
  print("jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj")
  
  print(missData_Predictors)
  #Note: the appended row in missingData function has to be removed. 
  if(length(missData_Predictors)==3){
    missData_Predictors #####address tis.... are you returning the entire table... need to resolve
  }
  
 
  #message for "missing data" warning
  if(nrow(missData_Predictors)==0){
    output$msg_pred <- renderText({
      paste("<b>Predictor Info. is up-to-date!")
    })
    ##shinyjs::hide("file1")  
    ##shinyjs::hide("progressingbar2")  
  }
  
  #missing dates in the predictors information
  if(nrow(missData_Predictors)>0){
    output$missed_Pred_Info <- DT::renderDataTable({
      #DT::datatable(apply(missData_Predictors[2:nrow(missData_Predictors)], 2, rev), options = list(lengthMenu = c(5, 10), pageLength = 5))
      DT::datatable(apply(missData_Predictors, 2, rev), options = list(lengthMenu = c(5, 10), pageLength = 5))
      })
    
    #to hide "missing data" warning
    #if(nrow(missData_Predictors)>0){
    output$notify_pred <- renderText({ print("Issues")  }) 
    #}
    
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
    

  #------------------------checking.., uploading and appending predictor information
     #uploading file

    
    observe({
    
      shinyjs::hide("append_file3")  
      shinyjs::hide("issues3")
      shinyjs::hide("fields_absent3")
      shinyjs::hide("fall_outside_daterange3")
      shinyjs::hide("date_Overlapping3")
      #shinyjs::hide("timeFormatWrong3")
      #shinyjs::hide("typo_camera_Name3")
      shinyjs::hide("resolve_issue3")
      shinyjs::hide("Re-train Prediction Model")
      
      req(input$file3)
      #To check the gaps that an uploaded file fill
      uploaded_file3 <- read.csv(input$file3$datapath,
                                header = TRUE,
                                sep = ",")#,
      
      #import the predictor information
      predictors_info <- read.table(file=paste(parameter_directory, "predictors_INFO/", "predictors_info", ".csv", sep=""), sep=",", head=TRUE) 
      #extract the predictors info that have weather information.
      predictors_info_extract <- predictors_info[which(predictors_info$status==1),]  #head(predictors_info_extract)
      predictors_info_extract_Current <- predictors_info_extract[which(predictors_info_extract$Date <= Sys.Date()),] #
      
      startTimeC <- Sys.time()
      
      #for(i in 1:100){
      observe({
      timeUpd <- (as.numeric(round(Sys.time()-startTimeC, digits=1))*50)
      updateProgressBar(session = session, id = "pb3", value = timeUpd) #input$i
      invalidateLater(1000, session)
      })
      #}
 
    #initialisation
    total_issues3 = 0
    issue1_3 = 0
    issue2_3 = 0
    issue3_3 = 0
    #issue4_3 = 0
    #issue5_3 = 0
    
    #checking the uploaded file
    leng_name3 <- uploaded_fieldnames(uploaded_file3, essential_Fields =  c("Date","mean_temp","rain")) #checking essential field names
    out_Len3 <- dateRange_Checker(predictors_info, uploaded_file3) #checking if dates falls outsides desired range 
    overlap_Dates3 <- dateOverlap_Checker(predictors_info_extract_Current, uploaded_file3) #checking whether any of the uploaded record overlap with the dates in the database 
    #Inspect_Time_Format3 <- detect_Time_Format_Error(uploaded_file3)#checking the date format

    #check_typo_in_Camera_Name3 <- check_typo_in_Camera_Name(data=uploaded_file3, lists_Loc_Correct3)
    
    essential_Fields <- c("Date", "mean_temp", "rain")
    
    if(as.numeric(leng_name3)!=length(essential_Fields)){
      issue1_3<-1}
    
    if(out_Len3>0){
      issue2_3<-1}
    
    if(overlap_Dates3>0){
      issue3_3<-1}
    
    #if(Inspect_Time_Format3>0){
      #issue4_3<-1
    #}
    #if(check_typo_in_Camera_Name3>0){
      #issue5_3<-1
    #}
    
    # 
    total_issues3 <- issue1_3 + issue2_3 + issue3_3 #+ issue4_3 #+ issue5_3
    
    #if there is no issues, then show "Upload" button
    if(total_issues3==0){
      #turn off
      shinyjs::hide("issues3")
      shinyjs::hide("fields_absent3")
      shinyjs::hide("fall_outside_daterange3")
      shinyjs::hide("date_Overlapping3")
      #shinyjs::hide("timeFormatWrong3")
      #shinyjs::hide("typo_camera_Name3")
      shinyjs::hide("resolve_issue3")
      shinyjs::hide("taskCompleted3")
      shinyjs::hide("restart_app3")
      
      #turn on
      output$Uploaded_file_checks_Passed3 <- renderText({paste("<b>File checks completed! Click the button below to update predictors info.")})
      shinyjs::show("append_file3")
      shinyjs::show("append_button_Descrip3")
      shinyjs::hide("Re-train Prediction Model")
      
      #aggregated the data and preview
      
    }
    
    if(total_issues3!=0){
      
     # shinyjs::hide("processingbar1")
      shinyjs::hide("Uploaded_file_checks_Passed3")
      shinyjs::hide("append_file3")
      shinyjs::hide("append_button_Descrip3")
      
      shinyjs::show("issues3")
      shinyjs::show("fields_absent3")
      shinyjs::show("fall_outside_daterange3")
      shinyjs::show("date_Overlapping3")
      #shinyjs::show("timeFormatWrong3")
      #shinyjs::hide("typo_camera_Name3")
      shinyjs::show("resolve_issue3")
      shinyjs::hide("taskCompleted3")
      shinyjs::hide("restart_app3")
      shinyjs::hide("Re-train Prediction Model")
      
      output$issues3 <- renderText({paste("<b>ISSUES IDENTIFIED:", "<br>")})
      if(issue1_3==1){
        output$fields_absent3 <- renderText({print("*  One or more of the essential fieldnames missing: 'Date', 'mean_temp', 'rain'")})}
      if(issue2_3==1){
        output$fall_outside_daterange3 <- renderText({print("*  One or more of the uploaded dates fall outside the expected date range (i.e. earliest date in the already loaded data and the current date")})}
      if(issue3_3==1){
        output$date_Overlapping3 <- renderText({print("*  Weather info. for one or more of the uploaded dates have previously been uploaded!")})}
      #if(issue4_3==1){
        #output$timeFormatWrong3 <- renderText({print("*  One or more of the 'Hour' entries  are in the format 'hh:mm'. Please, change to them 0, 1, 2,..., 23, to represent hours of 00:00, 01:00, ..... 23:00, respectively. Use MS Excel to accomplish this by creating a new column ('Hour'), set the column as numeric and return values (hh:mm x 24). Remove the original 'Hour' column")})}
      #if(issue5_3==1){
        #output$typo_camera_Name3 <- renderText({paste("*  Errors detected in the name(s) of camera location. Check that there are no typo errors in the names of camera locations. Check 'Parameter' tab for correct spellings of location names.")})
     #}
      
      output$resolve_issue3 <- renderText({paste("<b>Please, resolve issues and re-upload file.....")})
    }
    
    })
    
    
    observeEvent(input$append_file3, {
      
      shinyjs::hide("Re-train Prediction Model")
      
      req(input$file3)
      #To check the gaps that an uploaded file fill
      #Check whether this is necessary again!
      uploaded_file3 <- read.csv(input$file3$datapath,
                                 header = TRUE,
                                 sep = ",")#,
      
      
      #weatherInfo <- read.table("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/predictors_INFO/to be uploaded/weather_info.csv", sep=",", head=TRUE)
      #head(data) #data[2130,]
      
      #convert date to appropriate format
      predictors_info <- convert_Date(predictors_info, TimeField = FALSE) ##  predictors_info[1:5, 1:5]
      weatherInfo <- convert_Date(uploaded_file3, TimeField = FALSE) ##weatherInfo
      
      id_to_update <- which(predictors_info$Date %in% weatherInfo$Date)
      
      #for(h in 1:nrow(weatherInfo)){ #h<-1
      predictors_info[id_to_update, c("mean_temp")] <- weatherInfo$mean_temp
      predictors_info[id_to_update, c("rain")] <- weatherInfo$rain
      predictors_info[id_to_update, c("status")] <- 1
      #}
      
      write.table(predictors_info, file=paste(parameter_directory, "predictors_INFO/", "predictors_info", ".csv", sep=""), sep=",", row.names=FALSE)
      
      shinyjs::show("taskCompleted3")
   
      output$taskCompleted3 <- renderText({paste(tags$p(tags$b(h4("The Weather information for the specified date(s) have been updated successfully!"))))})  #renderText({paste(tags$p(tags$b(h3("Replacing the Existing Raw HF Dataset"))))})
      
      output$restart_app3 <- renderText({paste(tags$p(tags$b(h4("Re-training completed!  Please, restart app. to effect these changes."))))}) 
      
      shinyjs::hide("append_file3")
      
      shinyjs::show("Re-train Prediction Model")
    })
    
    
    
    #procedure to re-train prediction model
    observeEvent(input$train_Prediction_Model, priority=10, {
      
      #re-import the updated data to retrain the model.
      predictors <- read.table(file=paste(parameter_directory, "predictors_INFO/", "predictors_info", ".csv", sep=""), sep=",", head=TRUE)	
      aggre_footfall <- read.table(file=paste(file_here, "dayTimeAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)
      
      cleaned_data_for_training <- data_Preparation_for_training(aggre_footfall=aggre_footfall, predictors=predictors, modelName ="randomForest", training_length_in_yrs = 3)
      
      #train the model  - regression
      #pred_model <- lm(InCount ~ ., data = cleaned_data_for_training)
      pred_model <- randomForest(InCount ~., data=cleaned_data_for_training)
      
      #save the model
      save(pred_model, file = paste(other_dir, "random_forest_model.rda", sep="")) 
      ##Using randomForest algorithm
      #randomForest <- randomForest(y ~., data=train)
      
      
      
      shinyjs::show("restart_app3")
      
    })
  
  
    
#processing bar for updating historical datasets
#disable("slider")
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
  
  
  #processing bar for uploading historical datasets
  #disable("slider")
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

  
  
# #monitors file upload
#   observeEvent(input$file2, priority=10, {
#     js$play()
#   })
#   output$processingbar1 = renderUI({
#     shinyjs::hide("processingbar1")
#     sliderInput("slider", label = "", width = '300px',min = 0, max = 99,value = 0, step = 1, post="%  Done",
#                 animate = animationOptions(
#                   interval = (8*8), #5 seconds
#                   playButton = "",
#                   pauseButton = ""))})
  
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
  output$today <- renderText({
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
  
  
  
  
#to forecast next 5-days footfall  
  output$forecasted_footfall <- renderPlot({
 
    #streaming 3-hourly aggregates of weather forecast from "https://sci.ncas.ac.uk/leedsweather/", for next 5-days
    weather_forecast <- get_forecast(lat = 53.8013, lon = -1.548567, cnt = 40) #coord of city centre
    
    Date <- substr(weather_forecast$list$dt_txt, 1, 10)
    uniqueDates <- data.frame(unique(Date))
    colnames(uniqueDates) <- "Date"
    
    weather_forecast <- cbind(Date, weather_forecast$list[, c("main.temp","rain.3h")])
    
    #aggregate
    temp_fiveDays <- aggregate(main.temp ~ Date, data = weather_forecast, FUN = mean, drop = FALSE)
    temp_fiveDays <- Data_kelvin_to_celsius(temp_fiveDays)
    #where temp is "NA" (due to instrument breakdown), substitute "5"
    temp_fiveDays <- merge(x = uniqueDates, y = temp_fiveDays, by = "Date", all.x = TRUE, all.y = TRUE)
    temp_fiveDays$main.temp[is.na(temp_fiveDays$main.temp)] <- 5
    
    #converting the 3 hours rain values to its corresponding 5 minutes aggregates (i.e. equivalence of values from 'http://sci.ncas.ac.uk/leedsweather/')
    weather_forecast$rain.3h <- weather_forecast$rain.3h * 3/5
    
    rain_fiveDays <- aggregate(rain.3h ~ Date, data = weather_forecast, FUN = sum, na.action = na.omit)
    #where rain is "NA" (due to instrument breakdown), substitute "0"
    rain_fiveDays <- merge(x = uniqueDates, y = rain_fiveDays, by = "Date", all.x = TRUE, all.y = TRUE)
    rain_fiveDays$rain.3h[is.na(rain_fiveDays$rain.3h)] <- 0

    #import othe predictors for the date specified
    #import the predictor information
    predictors_info <- read.table(file=paste(parameter_directory, "predictors_INFO/", "predictors_info", ".csv", sep=""), sep=",", head=TRUE) 
    predictors_info <- convert_Date(predictors_info, TimeField = FALSE)  
    
    #extract the independent variables of today and the next five days.
    x_new_5_days <- predictors_info[which(as.character(predictors_info$Date) %in% as.vector(temp_fiveDays$Date)),]            #head(predictors_info_subset)

    print(x_new_5_days)
    print(temp_fiveDays)
    print(rain_fiveDays)
    
    print("===============rrrrrrrrr================")
    #add weather temp and rain data
    x_new_5_days$mean_temp <- temp_fiveDays$main.temp
    x_new_5_days$rain <- rain_fiveDays$rain.3h
    
    
    #print(x_new_5_days)
    
    #drop "Date", "status" columns 
    x_new_5_days <- subset(x_new_5_days, select = -c(Date, status))
    
    #load prediction model
    load(paste(other_dir, "random_forest_model.rda", sep=""))
    
    #predict footfall rate for the selected Date, temperature and rain values
    y_new_5_days <- as.vector(round(predict(pred_model, x_new_5_days), digits = 0))
    
    
    print(y_new_5_days)
    print("===============")

    y_new_5_days <- vector_perc_diff(y_new_5_days)
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot3(y=y_new_5_days) #, chartType = input$forecast_chartType
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
 #   c <- 1:100

    #initialisation
    temp_Value <- 0
    rain_Value <- 0
    #http://www.holiday-weather.com/leeds/averages/
    #non_Selectable_Date <- c(as.Date("2018-05-15"), as.Date("2018-12-25"), as.Date("2019-01-01"),as.Date("2019-12-25"))
    
    #prediction parameters from UI
    input_dateToForecast = as.Date(input$dateToForecast)
    input_temp_level = as.character(input$temp_level)
    input_rain_level = as.character(input$rainfall_level)
    
    #initialise temperature and rainfall value
    
    
    #import othe predictors for the date specified
    #import the predictor information
    predictors_info <- read.table(file=paste(parameter_directory, "predictors_INFO/", "predictors_info", ".csv", sep=""), sep=",", head=TRUE) 
    predictors_info <- convert_Date(predictors_info, TimeField = FALSE)  
    #extract the predictors info for the specified Date
    x_new <- predictors_info[which(predictors_info$Date == input_dateToForecast), ]                #head(predictors_info_subset)
    
    #return the value for the corresponding temperature selected
    if(input_temp_level=="Very Low"){temp_Value=5} 
    else if(input_temp_level=="Low"){temp_Value=10}
    else if(input_temp_level=="Moderate"){temp_Value=15}
    else {temp_Value=28}
    
    print(temp_Value)
    print("==============================")
    
    x_new$mean_temp <- temp_Value
    
    #return the value for the corresponding rain level selected
    if(input_rain_level=="None"){rain_Value=0} 
    else if(input_rain_level=="Light"){rain_Value=0.5}
    else {rain_Value=10}
    
    print(rain_Value)
    print("==============================")
    
    x_new$rain <- rain_Value
    
    #drop "Date", "status" columns 
    x_new <- subset(x_new, select = -c(Date, status))
    
    #load prediction model
    load(paste(other_dir, "random_forest_model.rda", sep=""))
    
    #predict footfall rate for the selected Date, temperature and rain values
    y_new <- round(predict(pred_model, x_new), digits = 0)
    
    Type_dummy <- 2
    outlier_dummy <-2
    
    y_new <- data.frame(Type_dummy, input_dateToForecast, y_new, outlier_dummy)
    colnames(y_new) <- c("Type","Date","InCount", "Outlier")   #data.frame(Type, Date, InCount, Outliers)
    # 
    print(temp_Value)
    print(rain_Value)
    print(x_new)
    print(y_new)
    print("==============================")   
  #to set chart type
  chartType = input$chartType
  #to set time segmentation to plot
  plotOptn = input$timeOftheDayInput

#if(chartType=="Dot"){  
  #if(plotOptn=="Whole Day"){
  data <- convert_Date(twentyFourHours_HF_aggre, TimeField = FALSE)     
    par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
    auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), y_new, addTrend = input$trendLine, chartType=input$chartType)
  #} 
  
  # else if (plotOptn=="Daytime"){
  #   data <- convert_Date(dayTime_HF_aggre, TimeField = FALSE)     
  #   par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
  #   auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), predicted_Point = y_new, addTrend = input$trendLine, chartType=input$chartType)
  #} 
  
#   else if(plotOptn=="Evening"){
#     data <- convert_Date(eveningTime_HF_aggre)     
#     par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
#     auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), predicted_Point = y_new, addTrend = input$trendLine, chartType=input$chartType)
#   }else if(plotOptn=="Night"){
#     data <- convert_Date(nightTime_HF_aggre)     
#     par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
#     auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), predicted_Point = y_new, addTrend = input$trendLine, chartType=input$chartType)
#   }
# #} 

# if(chartType=="Line"){  
#   if(plotOptn=="Whole Day"){
#     data <- convert_Date(twentyFourHours_HF_aggre)     
#     par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
#     auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), addTrend = input$trendLine, plotStyle=1)
#   } else if(plotOptn=="Daytime"){
#     data <- convert_Date(dayTime_HF_aggre)     
#     par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
#     auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), addTrend = input$trendLine, plotStyle=1)
#   }else if(plotOptn=="Evening"){
#     data <- convert_Date(eveningTime_HF_aggre)     
#     par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
#     auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), addTrend = input$trendLine, plotStyle=1)
#   }else if(plotOptn=="Night"){
#     data <- convert_Date(nightTime_HF_aggre)     
#     par(mar=c(0,0,0,0)+0.1, mgp=c(0,0,0))
#     auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), addTrend = input$trendLine, plotStyle=1)
#   }
# } 
  
  })
  
 
  output$todaysfootfallCount <- renderText({
    paste(th_separator(3634*200))
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
  
  #Leeds = map('world', fill = TRUE, plot = FALSE, region = "UK", exact=TRUE)
  
  # Add a default minZoom and maxZoom of the same value so that the map does not zoom
  # output$mapLeeds <- renderLeaflet({
  #   leaflet(Leeds) %>% addTiles(options=tileOptions(minZoom=4, maxZoom=4)) %>%
  #     fitBounds(Leeds$range[1], Leeds$range[3], Leeds$range[2], Leeds$range[4]) %>%
  #     addPolygons(fillOpacity = 0.6,  smoothFactor = 0.5, stroke = TRUE, weight = 1)               
  # })
  
  output$mapLeeds <- renderLeaflet({
    crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    city_central =read.table(paste0(ROOT_DIR,"/lcc-footfall/webapp/misc/city_central.csv"), sep=",", head=TRUE)
    val=2
    city_Boundary = readShapePoly(paste0(ROOT_DIR,"/lcc-footfall/webapp/misc/leeds_City.shp"))
    data <- as.data.frame(city_central[1:nrow(city_central),])
    leaflet(data = data) %>% addTiles() %>%
      addMarkers (~X_Lon, ~Y_Lat, popup = ~as.character(Id)) %>% addPolygons(data= city_Boundary, color = "black", fill=FALSE) %>%
      addCircles(data=data, ~X_Lon, ~Y_Lat, popup = ~as.character(Id),  stroke = TRUE, radius=1500)
  })
  # 
  # # Change a reactive value depending on mouse click
  # zoom <- reactiveValues(level=4)
  # # This records mouse clicks outside polygons
  # observeEvent(input$mapLeeds_click, {
  #   zoom$level = 20
  # })
  # # This records mouse clicks inside polygons
  # observeEvent(input$mapLeeds_shape_click, {
  #   zoom$level = 20
  # })
  # # Change zoom level of the map
  # observe({
  #   if (zoom$level == 20) {
  #     leafletProxy("mapLeeds") %>% clearTiles() %>%
  #       addTiles(options=tileOptions(minZoom=4, maxZoom=20))
  #   }
  # })
  # 
    
  output$mytable1_1 <- DT::renderDataTable({
    DT::datatable(history_footfall[, input$show_vars2, drop=FALSE])
  })
  
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
  print(missData)
  #Note: the appended row in missingData function has to be removed. 
  if(length(missData)==3){
    missData
    
  }
  
  #to hide "missing data" warning
  if(nrow(missData)>0){
    output$notify <- renderText({
      print("Issues")
    }) 
  }
   
  #message for "missing data" warning
  if(nrow(missData)==0){
    output$msg <- renderText({
      paste("<b>No date is completely missed across all cameras!")
    })
    shinyjs::hide("file1")  
    #shinyjs::hide("progressingbar1")  
    shinyjs::hide("progressingbar2")  
  }
  
  #run this if there are missing dataset
  if(nrow(missData)>0){
    output$missed_Foot <- DT::renderDataTable({
      #DT::datatable(historical_footfall[,c("Date","Hour","InCount")])
      DT::datatable(apply(missData, 2, rev))
      #DT::datatable(missing_dates) #apply(history_footfall, 2, rev)
    })
    
  output$testHTML1 <- renderText({paste("<b>Above table shows the list of periods in which footfall data are missing.", "<br>")})
  output$text2 <- renderText({paste("Search for the missing data from either of the following sources:")})
  output$testHTML3 <- renderText({paste("<b>1. https://datamillnorth.org/dataset/leeds-city-centre-footfall-data")})
  output$testHTML4 <- renderText({paste("<b>2. https://data.gov.uk/dataset/leeds-city-centre-footfall-data")})

  output$otherInfo <- renderText({paste("Note: Ensure that the file to be uploaded contains the following three columns:","<br>",
                                        "(a) 'Date' - in any of the following formats: 'dd/mm/yyyy', 'dd-mm-yyyy', 'yyyy/mm/dd', OR 'yyyy-mm-dd'","<br>",
                                        "(b) 'Hour' - 'Hour of the day', either in the format 'hh:mm', or i.e. 0, 1, 2, .... 23.", 
                                        "(c) 'InCount' - Hourly aggregate of footfall count", "<br>",
                                        "(d) 'LocationName' - Containing the names assigned to camera locations", "<br>",
                                        "<br>",
                                        "Upload a .csv file to update the database", "<br>", 
                                        "An 'upload' button will appear after a valid file has been uploaded")})

  output$HF_directory <- renderText({paste("**The actual historical HF .csv file can be found in the directory:", HF_directory, sep=" ")})
  output$HF_view <- renderText({paste("<b>The corresponding aggregated HF can be viewed on the 'DASHBOARD' page'; and the actual .csv files can be found in the 'aggregated_historical_HF' directory")})
  
  output$why_re_gen_HF <- renderText({paste(tags$p(tags$b(h3("Replacing the Existing Raw HF Dataset"))))})#tags$p(tags$b(h4
  output$why_re_gen_HF2 <- renderText({paste("CAUTION: This means that you want to replace the existing historical HF file, generate new aggregates of the historical HF, and re-train the prediction. The results are used to generate the HF profiles on the 'DASHBOARD' page")})
  
  output$regen_HF_warning <- renderText({paste("<b>Warning: If you want to replace the historical HF file, to generate new aggregates and training the prediction model can take up to two and half hours!")})
  output$append_button_Descrip <- renderText({paste("<b> By clicking the 'append' button, the different aggregates, based on two time segmentations i.e. c(0:23) and c(8:17), representing the 'whole day' and 'daytime' footfall respectively, will be generated")})
  
  }

 
  #-----------------------------------------------------------------SECTION: deals with UPDATING historical footfall
  observe({
    #to hide upload button
    shinyjs::hide("append")
    shinyjs::hide("append_button_Descrip") #   
    shinyjs::hide("confirm_Append")
    shinyjs::hide("reload_APP")
    #shinyjs::hide("aggre_HF_processing")
    })

  observe({

    req(input$file1)
    #To check the gaps that an uploaded file fill
    uploaded_file <- read.csv(input$file1$datapath,
                              header = TRUE,
                              sep = ",")#,
    
    #to take care of data with null records (new/latest footfall data downloads)
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
    #issue4 = 0
    issue5 = 0
   
    #checking whether the uploaded file contain essential fields
    leng_name <- uploaded_fieldnames(uploaded_file, essential_Fields = c("Date","Hour","InCount", "LocationName")) #checking essential field names
    out_Len <- dateRange_Checker(history_footfall, uploaded_file) #checking if dates falls outsides desired range 
    overlap_Dates <- dateOverlap_Checker(history_footfall, uploaded_file) #checking whether any of the uploaded record overlap with the dates in the database 
    #Inspect_Time_Format <- detect_Time_Format_Error(uploaded_file)
    check_typo_in_Camera_Name <- check_typo_in_Camera_Name(data=uploaded_file, lists_Loc_Correct)
    

    essential_Fields <- c("Date","Hour","InCount", "LocationName")
    
    if(as.numeric(leng_name)!=length(essential_Fields)){
      issue1<-1}
    
    if(out_Len>0){
      issue2<-1}
    
    if(overlap_Dates>0){
      issue3<-1}
    
    #if(Inspect_Time_Format>0){
      #issue4<-1
    #}
    
    if(check_typo_in_Camera_Name>0){
      issue5<-1
    }
    
    #total_issues <- issue1 + issue2 + issue3 + issue4 + issue5
    total_issues <- issue1 + issue2 + issue3 + issue5 #+ issue5
    
    #if there is no issues, then show "Upload" button
    if(total_issues==0){
      #turn off
      shinyjs::hide("issues")
      shinyjs::hide("fields_absent")
      shinyjs::hide("fall_outside_daterange")
      shinyjs::hide("date_Overlapping")
      #shinyjs::hide("timeFormatWrong")
      shinyjs::hide("typo_camera_Name")
      shinyjs::hide("resolve_issue")
      shinyjs::hide("reload_APP")
      
      #turn on
      output$Uploaded_file_checks_Passed <- renderText({paste("<b>File checks completed!")})
      shinyjs::show("append")
      shinyjs::show("append_button_Descrip")
      
      #aggregated the data and preview
      
      }
    
    if(total_issues!=0){
      
      shinyjs::hide("processingbar1")
      shinyjs::hide("Uploaded_file_checks_Passed")
      shinyjs::hide("append")
      shinyjs::hide("append_button_Descrip")
      shinyjs::hide("reload_APP")
      
      output$issues <- renderText({paste("<b>ISSUES IDENTIFIED:", "<br>")})
      if(issue1==1){
      output$fields_absent <- renderText({print("*  One or more of the essential fieldnames missing: 'Date', 'Hour', 'InCount', 'LocationName'")})}
      if(issue2==1){
        output$fall_outside_daterange <- renderText({print("*  One or more of the uploaded dates fall outside the expected range (i.e. earliest date in the footfall (database) and the current date")})}
      if(issue3==1){
        output$date_Overlapping <- renderText({print("*  Some dates in the uploaded file overlap with dates in the existing footfall database")})}
      #if(issue4==1){
        #output$timeFormatWrong <- renderText({print("*  One or more of the 'Hour' entries  are in the format 'hh:mm'. Please, change to them 0, 1, 2,..., 23, to represent hours of 00:00, 01:00, ..... 23:00, respectively. Use MS Excel to accomplish this by creating a new column ('Hour'), set the column as numeric and return values (hh:mm x 24). Remove the original 'Hour' column")})}
      if(issue5==1){
        output$typo_camera_Name <- renderText({paste("*  Errors detected in the name(s) of camera location. Check that there are no typo errors in the names of camera locations. Check 'Parameter' tab for correct spellings of location names.")})
      }
      
      output$resolve_issue <- renderText({paste("<b>Please, resolve issues and re-upload file.....")})
    }
    
  })
  
  #perform the following action upon clicking 'append' button
  observeEvent(input$append, {
    #create two files
    #historicalData_Subset <- history_footfall[,c("Date","Hour","InCount", "LocationName")]
    #print(historicalData_Subset)
    shinyjs::show("confirm_Append")
    
  })
  
    observeEvent(input$confirm_Append, {
    

    req(input$file1)
    #To check the gaps that an uploaded file fill
    uploaded_file <- read.csv(input$file1$datapath,
                              header = TRUE,
                              sep = ",")#,
    
    #to take care of data with null records (new/latest footfall data downloads)
    uploaded_file <- uploaded_file[which(!is.na(uploaded_file$InCount)),]
    
    shinyjs::show("processingbar1")
    
    # print("line2")
    # #subset the data for only the necessary fields
    uploadedData_Subset <- uploaded_file[,c("Date","Hour","InCount", "LocationName")]
     
     #removing possible whitespaces in the names of camera location
    uploadedData_Subset <- remove_whiteSpace_in_Camera_Name(uploadedData_Subset)
  
    # #get the most recent date from the uploaded dataset
     max_Date <- max(uniq_Dates(uploadedData_Subset))
    #
     
    # #specifying the temporal segmentations to use for the data aggregation
     print(max_Date)
     #hours_of_the_Day <- list(c(0:23), c(8:17), c(18:21), c(22,23,0, 1, 2, 3, 4, 5, 6, 7))
     
     #hours_of_the_Day <- list(c(0:23), c(8:17))  #, c(18:21), c(22,23,0, 1, 2, 3, 4, 5, 6, 7))
     hours_of_the_Day <- list(c(0:23))  #, c(18:21), c(22,23,0, 1, 2, 3, 4, 5, 6, 7))
    #
     print("200000")
    #
    # #labels of time aggregation
     #time_aggregation <- c("twentyFour_Hours", "dayTime", "eveningTime","nightTime")
     #time_aggregation <- c("twentyFour_Hours", "dayTime")
     time_aggregation <- c("twentyFour_Hours")
    #
    # #first aggregating HF count across stations for each hour of the day
     result1 <- subset_Dataset(orig_Data = uploadedData_Subset, cameraLoc = "LocationName")
    # print("300000")
    
    print("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
    print(result1)
    # print("3500000")
    #
    # print("line3")
    aggregate_Location <- aggregate_Location(orig_Data_sub = result1)
    # #-----------------------------------
    
    #'Hour' field check
    HourField <- as.vector(is.na(aggregate_Location$Hour[1]))
    
    print(aggregate_Location)
    print("l;jskdfaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa;")
    print(HourField)
    print(length(HourField))

  if(nrow(aggregate_Location)>1){
    for(j in 1:length(hours_of_the_Day)){ #i<-1   #length(hours_of_the_Day )
       print (hours_of_the_Day[[j]])
      
      if(HourField==FALSE){
       aggregate_time_of_the_Day <- footfall_by_time_of_the_Day(loc_agg_data=aggregate_Location, time_aggre = hours_of_the_Day[[j]])}
      
      if(HourField==TRUE){
        aggregate_time_of_the_Day <- subset(aggregate_Location, select=c(Date, InCount)) 
        colnames(aggregate_time_of_the_Day) <- c("Date", "InCount")
      }
      
       
      print(aggregate_time_of_the_Day)
      print("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")
          #   #identify outliers ("0" - NULL data point, "1" - outliers, "2" - not outliers)
       outlier_events <- outliers(data=aggregate_time_of_the_Day)
    #   #append the outlier list to the result
       update_aggregate <- cbind(aggregate_time_of_the_Day, outlier_events)
       colnames(update_aggregate)<- c("Date","InCount","outlier")
      #Import the existing corresponding aggregate file
      existing_time_aggre_HF <- read.table(file=paste(file_here, time_aggregation[j], "Aggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep = ",", head=TRUE)
      existing_time_aggre_HF_Updated <- as.data.frame(rbind(existing_time_aggre_HF, update_aggregate))
      #drop the outlier field and re-compute a new one using the new data
      existing_time_aggre_HF_Updated <- subset(existing_time_aggre_HF_Updated, select = c("Date", "InCount"))
      #recompute the outlier
      outlier_events <- outliers(data=existing_time_aggre_HF_Updated)
      #append the outlier list to the result
      aggregates_updated <- cbind(existing_time_aggre_HF_Updated, outlier_events)
      colnames(aggregates_updated)<- c("Date","InCount","outlier")
      aggregates_updated <- aggregates_updated[order(aggregates_updated$Date),]
      aggregates_updated <- convert_Date(aggregates_updated)
      #writing the data aggregates based on four time segmentations
      
      write.table(aggregates_updated, file=paste(file_here, time_aggregation[j], "Aggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", row.names=FALSE) 
      
      #write.table(aggregates_updated, file=paste(file_here, time_aggregation[j], "Aggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",") 
      #update the existing raw HF dataset too..-----------------------------
      #read the existing one and append the subset of updated one to it
      shinyjs::hide("processingbar1")
      #generate the aggregation of uploaded historical HF separately and appened to the existing updated.-----
      output$aggre_HF_file_updated <- renderText({paste("<b> The aggregated HF files have been generated from the uploaded file and appended to the existing aggregated files accordingly!")})
      output$reload_HF_update <- renderText({paste(tags$p(tags$b(h2("Please, re-load the application to see changes made. Thanks."))))}) # have to check this
   }

    existing_Raw_HF <- read.table(file=paste(HF_directory, "subset_historical_HF_DoNot_REMOVE_or_ADD_ToThisDirectory", ".csv", sep=""), sep=",", head=TRUE) 
    #append the uploaded file
    new_Raw_HF <- rbind(existing_Raw_HF, uploadedData_Subset)
    
    #new_Raw_HF <- new_Raw_HF[order(new_Raw_HF$Date),]
    
    new_Raw_HF <- convert_Date(new_Raw_HF)
    
    #history_footfall <- write.table(new_Raw_HF, file=paste(HF_directory, "subset_historical_HF_DoNot_REMOVE_or_ADD_ToThisDirectory", ".csv", sep=""), sep=",", row.names=FALSE) 
    
    write.table(new_Raw_HF, file=paste(HF_directory, "subset_historical_HF_DoNot_REMOVE_or_ADD_ToThisDirectory", ".csv", sep=""), sep=",", row.names=FALSE) 

    shinyjs::hide("append_button_Descrip")
    shinyjs::hide("append")
    shinyjs::hide("confirm_Append")
  }
   
    #when no update is made (i.e. occurs in the case where one or more camera is down for the entire dataset to be uploaded) 
    if(aggregate_Location==1){
  
      existing_Raw_HF <- read.table(file=paste(HF_directory, "subset_historical_HF_DoNot_REMOVE_or_ADD_ToThisDirectory", ".csv", sep=""), sep=",", head=TRUE) 
      #append the uploaded file
      new_Raw_HF <- rbind(existing_Raw_HF, uploadedData_Subset)
      
      write.table(new_Raw_HF, file=paste(HF_directory, "subset_historical_HF_DoNot_REMOVE_or_ADD_ToThisDirectory", ".csv", sep=""), sep=",", row.names=FALSE) 
      
      shinyjs::hide("append_button_Descrip")
      shinyjs::hide("append")
      shinyjs::hide("confirm_Append")   
      
      showModal(modalDialog(
        title = "Missing Data!",
        "Data from one or more camera location is missing. The aggregated time series will not be updated! However, HF file would be updated. Please, wait while the processing is completed!",
        easyClose = FALSE
      ))
    }
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

  
#-----------------------------------------------------------------SECTION: deals with UPLOADING a fresh historical file
observe({
  
    req(input$file2)
  
     #To check the gaps that an uploaded file fill
    #Check whether this is necessary again!
    uploaded_file2 <- read.csv(input$file2$datapath,
                          header = TRUE,
                          sep = ",")#,
  
    #uploading file
    startTimeC <- Sys.time()
  
    observe({
      #for(i in 1:100){
        timeUpd <- (as.numeric(round(Sys.time()-startTimeC, digits=1))*50)
        updateProgressBar(session = session, id = "pb2", value = timeUpd) #input$i
        invalidateLater(1000, session)
      #}
    })
    
    #shinyjs::show("aggre_HF_processing")
    
    #to remove whitespace in teh location name column
    uploaded_file2 <- remove_whitespace(uploaded_file2)
    
    #initialisation
    total_issues_1 = 0
    issue1_1 = 0
    #issue2 = 0
    #issue3 = 0
    #issue4_1 = 0
    issue5_1 = 0

    #checking whether the uploaded file contain essential fields
    leng_name <- uploaded_fieldnames(uploaded_file2, essential_Fields = c("Date","Hour","InCount", "LocationName")) #checking essential field names

    #Inspect_Time_Format <- detect_Time_Format_Error(uploaded_file2)
    check_typo_in_Camera_Name <- check_typo_in_Camera_Name(data=uploaded_file2, lists_Loc_Correct)
    
    
    essential_Fields <- c("Date","Hour","InCount", "LocationName")
    
    if(as.numeric(leng_name)!=length(essential_Fields)){
      issue1_1<-1}
    
    #if(out_Len>0){
      #issue2<-1}
    
    #if(overlap_Dates>0){
      #issue3<-1}
    
    #if(Inspect_Time_Format>0){
      #issue4_1<-1
    #}
    
    if(check_typo_in_Camera_Name>0){
      issue5_1<-1
    }
    
    #total_issues_1 <- issue1_1 + issue4_1 + issue5_1  #issue2 + issue3 + 
    total_issues_1 <- issue1_1 + issue5_1  #issue2 + issue3 + 
    
    #if there is no issues, then show "Upload" button
    if(total_issues_1==0){
      #turn off
      output$issues_1 <- renderText({paste(" ")})
      output$fields_absent_1 <- renderText({print(" ")})
      #output$timeFormatWrong_1 <- renderText({paste(" ")})
      output$typo_camera_Name_1 <- renderText({paste(" ")})
      output$resolve_issue_1 <- renderText({paste(" ")})
      
      #turn on
      output$Uploaded_file_checks_Passed_1 <- renderText({paste("<b>File checks completed!")})
 
      shinyjs::show("aggre_HF")
      shinyjs::hide("aggre_HF_confirm")

    }

    #To show or hide 'aggre_HF_confirm' 
    if(total_issues_1!=0){
      #turn off
      shinyjs::hide("processingbar2")
      output$Uploaded_file_checks_Passed_1 <- renderText({paste(" ")})
      
      #turn on
      output$issues_1 <- renderText({paste("<b>ISSUES IDENTIFIED:", "<br>")})
      if(issue1_1==1){
        output$fields_absent_1 <- renderText({print("*  One or more of the essential fieldnames missing: 'Date', 'Hour', 'InCount', 'LocationName'")})}
      #if(issue4_1==1){
        #output$timeFormatWrong_1 <- renderText({print("*  One or more of the 'Hour' entries  are in the format 'hh:mm'. Please, change to them 0, 1, 2,..., 23, to represent hours of 00:00, 01:00, ..... 23:00, respectively. Use MS Excel to accomplish this by creating a new column ('Hour'), set the column as numeric and return values (hh:mm x 24). Remove the original 'Hour' column")})}
      if(issue5_1==1){
        output$typo_camera_Name_1 <- renderText({paste("*  Errors detected in the name(s) of camera location. Check that there are no typo errors in the names of camera locations. Check under 'Parameter' tab for correct location names.")})
      }
      
      output$resolve_issue_1 <- renderText({paste("<b>Please, resolve the issues and re-upload file.....")})
      
      shinyjs::hide("aggre_HF")
      shinyjs::hide("aggre_HF_confirm")
 
    }
    
  })
  
#export appended data
observeEvent(input$aggre_HF, {
  
      shinyjs::show("aggre_HF_confirm")
    
    showModal(modalDialog(
      title = "Generate new data aggregates and re-train the model",
      "This process might take several hours to complete!",
      easyClose = FALSE
    ))
  })
  

observeEvent(input$aggre_HF_confirm, {
     
   req(input$file2)
   #To check the gaps that an uploaded file fill
   #Check whether this is necessary again!
   uploaded_file2 <- read.csv(input$file2$datapath,
                             header = TRUE,
                             sep = ",")#,
  
   shinyjs::show("processingbar2")
  
   output$processing_append <- renderText({print("Processing....")}) 
   #output$aggre_HF_processing <- renderText({paste(tags$p(tags$b("Processing....")))}) 
   #output$aggre_HF_processing <- renderText({paste("<b>Please, resolve the issues and re-upload file.....")})
   
   #to remove whitespace in teh location name column
   uploaded_file2 <- remove_whitespace(uploaded_file2)
  

   shinyjs::hide("aggre_HF_confirm")
   shinyjs::hide("aggre_HF")
   shinyjs::hide("Uploaded_file_checks_Passed_1")
 
     #subset the data for only the necessary fields
    orig_Data_Subset <- uploaded_file2[,c("Date","Hour","InCount", "LocationName")]
    head(orig_Data_Subset)
     
     #get the most recent date from the uploaded dataset
     max_Date <- max(uniq_Dates(orig_Data_Subset))

    print("100000")
    max_Date <- max(uniq_Dates(orig_Data_Subset))

    #to generate aggregated dataset at varying temporal scales
    print(max_Date)
    
    #create a list of time aggregate
    #hours_of_the_Day <- list(c(0:23), c(8:17), c(18:20), c(21,22,23, 0, 1, 2, 3, 4, 5))
    #hours_of_the_Day <- list(c(0:23), c(8:17), c(18:21), c(22,23,0, 1, 2, 3, 4, 5, 6, 7))
    
    #hours_of_the_Day <- list(c(0:23), c(8:17)) #, c(18:21), c(22,23,0, 1, 2, 3, 4, 5, 6, 7))
    hours_of_the_Day <- list(c(0:23)) #, c(18:21), 
    
    print("200000")
    
    #time_aggregation <- c("twentyFour_Hours", "dayTime", "eveningTime","nightTime")
    #time_aggregation <- c("twentyFour_Hours", "dayTime")
    time_aggregation <- c("twentyFour_Hours")
      
 
        result1 <- subset_Dataset(orig_Data=orig_Data_Subset, cameraLoc = "LocationName")
        print("300000")
        
        print("3500000")
        
        #removes location typo in the dataset
        aggregate_Location <- aggregate_Location(orig_Data_sub=result1)        
        #-----------------------------------         
        
        
        for(j in 1:length(hours_of_the_Day)){ #i<-1   #length(hours_of_the_Day )
          
        #if(i==1){
        print (hours_of_the_Day[[j]])
        #aggregate_time_of_the_Day <- footfall_by_time_of_the_Day(loc_agg_data=aggregate_across_location_by_Date, time_aggre = hours_of_the_Day[[j]])
        aggregate_time_of_the_Day <- footfall_by_time_of_the_Day(loc_agg_data=aggregate_Location, time_aggre = hours_of_the_Day[[j]])
        print("500000")

        #identify outliers ("0" - NULL data point, "1" - outliers, "2" - not outliers)
        outlier_events <- outliers(data=aggregate_time_of_the_Day)
        #append the outlier list to the result
        finalresult <- cbind(aggregate_time_of_the_Day, outlier_events)
        colnames(finalresult)<- c("Date","InCount","outlier")
        
        #file_here <- ROOT_DIR+"/lcc-footfall/webapp/downloaded_footfall dataset/aggregated_historical_HF/"
        write.table(finalresult, file=paste(file_here, time_aggregation[j], "Aggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", row.names=FALSE) 

        write.table(orig_Data_Subset, file=paste(HF_directory, "subset_historical_HF_DoNot_REMOVE_or_ADD_ToThisDirectory", ".csv", sep=""), sep=",", row.names=FALSE) 
      
        print("300000")
        }
        
        shinyjs::hide("processingbar2")
        output$taskCompleted <- renderText({paste(tags$p(tags$b(h4("Task Completed! New time series aggregates generated and predictive model re-trained. The data aggregates created can be found in the dir:"))))})  #renderText({paste(tags$p(tags$b(h3("Replacing the Existing Raw HF Dataset"))))})
        output$data_aggre_dir <- renderText({paste(tags$p(tags$b(file_here)))}) # have to check this!
        output$reload_HF <- renderText({paste(tags$p(tags$b(h2("Please, re-load the application to see changes made. Thanks."))))}) 
        shinyjs::hide("processing_append")
   })
    
  }) 
    
