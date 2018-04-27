#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#http://shiny.rstudio.com/

#install libraries
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

# Manually set the time zone
Sys.setenv(TZ="Europe/London")

#api key for downloading weather forecast...(http://openweathermap.org/)
owmr_settings("c8a930a2e30b695551e57d375a67d76e")  #Open weather forecast api key 

#set the root directory
ROOT_DIR = "C:/Users/geomad/Documents/GitHub/"
#ROOT_DIR = "C:/Users/monsu/Documents/GitHub/"
#ROOT_DIR = "/Users/nick/research_not_syncd/git_projects/"

EventTime <- Sys.time() - 1*1

#function to display number with thousand separator
th_separator <- function (x) format(round(as.numeric(x), 1), nsmall=0, big.mark=",")
 

#function to detect if time field (i.e. 'Hour') in a dataset is in 'hh:mm' format. 
#If so, round up to the nearest hour (e.g. 00:34 --> 0, 01:45 --> 1).
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

#function to subset data (Subset to contain fields: 'Date', 'Hour', 'Id' & 'LocationName'
subset_Dataset <- function(orig_Data, cameraLoc = "LocationName"){
  #create unique field for the dataset#
  #convert date to appropriate format
  orig_Data_Conv <- convert_Date(orig_Data, TimeField = TRUE)
  #create a unique field, combining the 'date' and 'time' field)
  unique_field <- matrix(paste(orig_Data_Conv$Date, orig_Data_Conv$Hour, sep="-"),,1)
  colnames(unique_field) <- c("Id")
  cam_ID <- which(colnames(orig_Data)==cameraLoc)
  #append back to the real data
  orig_Data_sub <- cbind(orig_Data_Conv$Date, orig_Data_Conv$Hour, unique_field, orig_Data_Conv$InCount, as.character(orig_Data_Conv[,cam_ID])) 
  orig_Data_sub<-as.data.frame(orig_Data_sub)
  colnames(orig_Data_sub) <- c("Date","Hour","Id","InCount","Loc_Id")
  return(orig_Data_sub)
}

#function to aggregate footfall dataset across camera location  
aggregate_Location <- function(orig_Data_sub){
  cameraLoc <- as.vector(unique(orig_Data_sub$Loc_Id))
  orig_Data_agg_Loc <- NULL
  uniqId <- unique(orig_Data_sub$Id)
  #this is to ensure that all cameras are represented, otherwise "NA" is reported
  loc_agg_data <-  matrix(0, 1, 4)
  rownames(loc_agg_data) <- length(cameraLoc)
  row.N <- "100" 
  appd_Row <- matrix(0, 1, 4)
  rownames(appd_Row) <- length(cameraLoc)
  for(i in 1:length(uniqId)){ #i<-2
    data_Sub <- orig_Data_sub[which(orig_Data_sub$Id==uniqId[i]),]
    InCountN <- sum(as.numeric(as.vector(orig_Data_sub[which(orig_Data_sub$Id==uniqId[i]),c("InCount")])))
    #check if all the unique_times are present
    true_Ct <- length((data_Sub$Loc_Id%in%cameraLoc)=="TRUE")
    orig_Data_agg_Loc <- rbind(orig_Data_agg_Loc, cbind(as.character(data_Sub$Date[1]), as.numeric(as.vector(data_Sub$Hour[1])), as.character(uniqId[i]), InCountN))
    combine_Result <- cbind(as.character(data_Sub$Date[1]), as.numeric(as.vector(data_Sub$Hour[1])), as.character(uniqId[i]), InCountN)
    loc_agg_data[which(rownames(loc_agg_data)==true_Ct),] <- as.vector(combine_Result)
    rownames(loc_agg_data) <- rep("100", nrow(loc_agg_data))
    loc_agg_data <- rbind(loc_agg_data, appd_Row)
  }
  #clean it up
  loc_agg_data <- loc_agg_data[-which(loc_agg_data[,1]=="0"),] 
  if(nrow(loc_agg_data)!=0){
    rownames(loc_agg_data) <- 1:nrow(loc_agg_data)
    loc_agg_data <- as.data.frame(loc_agg_data)
    colnames(loc_agg_data) <- c("Date","Hour","Id","InCount")
    return(loc_agg_data)}
  if(nrow(loc_agg_data)==0){
    loc_agg_data = 1
    return(loc_agg_data)}
}

#aggregate footfall dataset by time of the day
#Given a footfall dataset (containing a column 'Id' - concatenation of unique day-time), sum all 'InCount' by unique 'Hour' of the day
footfall_by_time_of_the_Day <- function(loc_agg_data, time_aggre){
  #create list of all days between two range (i.e. start and end date of historical footfall dataset)
  start_date <- min(uniq_Dates(loc_agg_data)) #library(lubridate) #suppress warning...
  end_date <- max(uniq_Dates(loc_agg_data))
  allDays_listed <- seq(as.Date(start_date), as.Date(end_date), by=1)
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
  colnames(data_subset) <- c("Date","Hour","InCount")
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

#function to identify outliers in the footfall datasets - add an outlier field indicating outliers as "1"
outliers <- function(data){ 
  x<-data
  hold_result <- matrix(0, nrow(x), 1)
  x<-as.numeric(as.vector(data$InCount)) 
  ind_hold.na <- which(is.na(x))
  ind_hold.not.na  <- which(!is.na(x))
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


# plot function for the historical footfall dataset (also add 'trend line' and select plot types ['Line' or 'Dot'])
auc_plot2 <- function(data, HF_startDate, plot_StartDate = 0, predicted_Point = y_new, addTrend = FALSE, chartType="Dot"){
  #create list of all days between the start date HF data collection and the current time
  start_date <- HF_startDate
  #end_date <- Sys.Date()
  end_date <- as.Date("2019-12-31")
  allDays_inbetween <- matrix(as.character(seq(as.Date(start_date), as.Date(end_date), by=1)),,1)
  colnames(allDays_inbetween) <- c("Date")  #mode(allDays_inbetween) #mode(data)
  merged_Datasetd <- merge(x = allDays_inbetween, y = data, by = "Date", all.x = TRUE, all.y = TRUE)
  merged_Datasetd  <- as.data.frame(merged_Datasetd)
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
    Type <- as.numeric(xy_1$Type)[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
    Date <-as.numeric(xy_1$x)[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
    current_Date_Index <- as.numeric(Sys.Date() - HF_startDate)
    InCount <- as.numeric(as.vector(xy_1$y))[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
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

#to generate dot plot    
if(chartType=="Dot"){
  if(addTrend==FALSE){
    print(ggplot(xy_1, aes(Date, InCount, group=Type)) +
            geom_vline(xintercept = min(Date),
                       color = "grey", size=1.5) +
            geom_hline(yintercept=0,
                       color = "grey", size=1.5) +
            geom_vline(xintercept = min(Date), linetype="dashed",
                       color = "brown", size=0.5) + #current date
            geom_vline(xintercept = current_Date_Index, linetype="dashed",
                       color = "grey", size=1) + #current date
            geom_point(color= c(rep("blue", (length(Type)-1)), "red") , size = c(rep(2, (length(Type)-1)), 4)  ) +
            geom_hline(aes(yintercept = mean(InCount, na.rm = T)), linetype="dashed",
                       color = "green", size=1) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[1],yend=percentiles[1]), linetype="dashed", color = "red", size=0.5) +
            #geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[1],yend=percentiles[1]), label=paste("Min.", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[2],yend=percentiles[2]), linetype="dashed", color = "orange", size=0.5) +
            geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[2],yend=percentiles[2]), label=paste("25th %tile", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[3],yend=percentiles[3]), linetype="dashed", color = "yellow", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[4],yend=percentiles[4]), linetype="dashed", color = "orange", size=0.5) +
            geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[4],yend=percentiles[4]), label=paste("75th %tile", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[5],yend=percentiles[5]), linetype="dashed", color = "red", size=0.5) +
            scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
    ) }
  if(addTrend==TRUE){
    print(ggplot(xy_1, aes(Date, InCount, group=Type)) +
            geom_vline(xintercept = min(Date),
                       color = "grey", size=1.5) +
            geom_hline(yintercept=0,
                       color = "grey", size=1.5) +
            geom_vline(xintercept = min(Date), linetype="dashed",
                       color = "brown", size=0.5) + #current date
            geom_vline(xintercept = current_Date_Index, linetype="dashed",
                       color = "grey", size=1) + #current date
            geom_point(color= c(rep("blue", (length(Type)-1)), "red") , size = c(rep(2, (length(Type)-1)), 4)  ) +
            geom_hline(aes(yintercept = mean(InCount, na.rm = T)), linetype="dashed",
                       color = "green", size=1) +
            geom_smooth(method = "lm", se=FALSE, color="red", lwd = 2) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[1],yend=percentiles[1]), linetype="dashed", color = "red", size=0.5) +
            #geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[1],yend=percentiles[1]), label=paste("Min.", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[2],yend=percentiles[2]), linetype="dashed", color = "orange", size=0.5) +
            geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[2],yend=percentiles[2]), label=paste("25th %tile", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[3],yend=percentiles[3]), linetype="dashed", color = "yellow", size=0.5) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[4],yend=percentiles[4]), linetype="dashed", color = "orange", size=0.5) +
            geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[4],yend=percentiles[4]), label=paste("75th %tile", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
            geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[5],yend=percentiles[5]), linetype="dashed", color = "red", size=0.5) +
            scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
    ) }
}
  #to generate line plot
  if(chartType=="Line"){
    if(addTrend==FALSE){
      print(ggplot(xy_1, aes(Date, InCount, group=Type)) +
              geom_line(color="blue", size = 0.5) +
              geom_vline(xintercept = min(Date),
                         color = "grey", size=1.5) +
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
              #geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[1],yend=percentiles[1]), label=paste("Min.", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[2],yend=percentiles[2]), linetype="dashed", color = "orange", size=0.5) +
              geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[2],yend=percentiles[2]), label=paste("25th %tile", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[3],yend=percentiles[3]), linetype="dashed", color = "yellow", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[4],yend=percentiles[4]), linetype="dashed", color = "orange", size=0.5) +
              geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[4],yend=percentiles[4]), label=paste("75th %tile", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[5],yend=percentiles[5]), linetype="dashed", color = "red", size=0.5) +
              scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
      ) }

    if(addTrend==TRUE){
      print(ggplot(xy_1, aes(Date, InCount, group=Type)) +
              geom_line(color="blue", size = 0.5) +
              geom_vline(xintercept = min(Date),
                         color = "grey", size=1.5) +
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
              #geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[1],yend=percentiles[1]), label=paste("Min.", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[2],yend=percentiles[2]), linetype="dashed", color = "orange", size=0.5) +
              geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[2],yend=percentiles[2]), label=paste("25th %tile", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[3],yend=percentiles[3]), linetype="dashed", color = "yellow", size=0.5) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[4],yend=percentiles[4]), linetype="dashed", color = "orange", size=0.5) +
              geom_text(aes(x=indEX2-100,xend=indEX2,y=percentiles[4],yend=percentiles[4]), label=paste("75th %tile", " (",most_recent_year,")", sep=""), size=3, colour = 'gray', alpha=0.9) +
              geom_segment(aes(x=indEX1,xend=indEX2,y=percentiles[5],yend=percentiles[5]), linetype="dashed", color = "red", size=0.5) +
              scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
      ) }
    }
  }
  

#function to plot the 5-day predictions (and also their corresponding previous weeks predictions)
auc_plot3 <- function(y, y_past=NULL){ 
  xy_1 <- y
  labs <- data.frame(xy_1Type=c(1, 2),
                     label = fontawesome(c('fa-arrow-circle-up','fa-arrow-circle-down'))  )
  d <- merge(xy_1, labs, by="xy_1Type")[order(merge(xy_1, labs, by="xy_1Type")[,2]),]
  d$Perc =  d$Perc * -1
  dateLabels = seq(Sys.Date()+1, (Sys.Date()+1 +(nrow(d)-1)), by = "day")
  dayLabels =   weekdays(as.Date(dateLabels))
  d <- cbind(d, dateLabels)
  #prepare the previous weeks data
  y_m <- melt(y_past)
  colfunc <- colorRampPalette(c("black", "grey"))
  colfunc <- colfunc(length(unique(y_m$Var2)))
  Ids <- NULL
  Ids_col <- NULL
  for(g in 1:length(unique(y_m$Var2))){   #g<-1
    Ids <- c(Ids, rep(g, length(unique(y_m$Var1))))
    Ids_col <- c(Ids_col, rep(colfunc[g], length(unique(y_m$Var1))))
  } 
  y_m <- cbind(y_m, Ids, Ids_col)
  colnames(y_m) <- c("Var1","Var2","value","Var3", "Var4")
  y_m <- as.data.frame(y_m)
    print(ggplot(d, aes(Date, InCount)) + #ylim(-1,max(50)) +
      geom_point(aes(Date, InCount, color=factor(xy_1Type)), size = 1) +
      theme(legend.position=" ") +
      geom_ribbon(aes(ymin=0, ymax=InCount), alpha=0.1, fill="blue") +
      geom_line(color="blue", size = 1.5)+ 
      geom_point(aes(Date, InCount, color=factor(xy_1Type)), size = 11) +
      geom_text(aes(Date, InCount,label=label),family='fontawesome-webfont', size=c(9)) + #nudge_x=0, nudge_y=0
        scale_x_discrete(limits=d$Date,labels=dateLabels) + 
      geom_point(data = y_m, aes(x = Var3, y = value, size = (abs(y_m$Var1-length(unique(y_m$Var1)))+1), group = Var3, color=Var4)) +
      annotate(geom = "text", x = y_m$Var3, y = y_m$value, label = y_m$Var1, size = 2) +
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

#function to calculate percentage increase/decrease a footfall prediction compared to previous (same weekdays) predictions
vector_perc_diff <- function(data){
  table_R <- matrix(0, length(data)-1, 4)
  for (i in 2:length(data)){ #i<-2
    table_R[i-1,1] <- i-1
    table_R[i-1,2] <- data[i]
    table_R[i-1,3] <- round(((data[i-1]-data[i])/data[i-1])*100, digits=0)
    if(table_R[i-1,3]<=0){table_R[i-1,4] = 1}
    if(table_R[i-1,3]>0){table_R[i-1,4] = 2}
  }
  table_R <- data.frame(table_R)
  colnames(table_R) <- c("Date", "InCount", "Perc", "xy_1Type")#, "Date")
  return(table_R)
}

#function to display date/time on the title bar of the dashboard
date_function <- function(){
  date_time <- Sys.time()
  dateT <- substr(as.character(date_time), 1, 10)
  timeT <- substr(as.character(date_time), 11, 20)
  dayT <- weekdays(as.Date(dateT))
  print(paste(dayT, ", ", dateT, ", ", sep=""))}

#date/time function 2 
date_function2 <- function(){
  date_time <- Sys.time()
  timeT <- substr(as.character(date_time), 11, 20)
  print(paste(" ",timeT, "GMT", sep=" "))}

#date/time function 3 
date_function3 <- function(){
  dateT <- Sys.Date() - 1
  #date_time2 <- Sys.Time()
  # <- substr(as.character(date_time), 1, 10)
  timeT <- substr(as.character(dateT), 11, 20)
  dayT <- weekdays(as.Date(dateT))
  print(paste(dayT, ", ", dateT, sep=""))}

#date/time function 4 
day_function <- function(){
  dateD <- Sys.Date()
  dayT <- paste(weekdays(as.Date(dateD)), ", ", (Sys.Date()), sep = "")
  print(dayT)}


#function to identify gaps in a 'Date' field of a dataset
missingData <- function(data, indicatorField = FALSE){
  #to subset the dataset first based on field on interest (specifically to deal with 'predictors' table)
  if(indicatorField==TRUE){
    data = data[which(as.vector(data$Date) < Sys.Date()),]
  }
  dataValues <- data$Date
  DateFromData <- as.character(dataValues)
  dateField <- matrix(DateFromData,,1)
  colnames(dateField) <- c("Date") 
  #to detect dates not in right format (i.e. yyyy-mm-dd)
  converDate1 <- as.Date(parse_date_time(dateField,"dmy"))
  listInconvertible <- which(!is.na(converDate1))
  dateField[listInconvertible] <- as.character(converDate1[listInconvertible])  
  #append back to the dataset
  dataValues <- dateField   
  dataValues <-   matrix(dataValues[order(dataValues[,1]),],,1)
  #append current date to the list..
  dataValues <- rbind(dataValues, as.character(Sys.Date()))
  #for 'predictor' table, remove those whose 'temp' and 'rain' column have been updated (i.e. with entry '1')
  if(indicatorField==TRUE){
    dataIndd = which(data$status == 0)
    dataValues = dataValues[-dataIndd,] #isolate the 'yet-to-be-updated' dates
  }
  DF <- as.Date(dataValues)
  DF_Dates <- diff(DF)
  missing_Dates <-  data.frame((DF[DF_Dates>1]+1), (DF[c(1, DF_Dates)>1]-1), (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]))
  colnames(missing_Dates) <- c("from","to","No_of_days")
  appdI <- matrix("2000-03-03",1,3)
  colnames(appdI) <- c("from","to","No_of_days")
  missing_Dates <- rbind(missing_Dates, appdI)
  #remove dates with less than one day
  missing_Dates <- missing_Dates[which(as.vector(missing_Dates[,3]) != 1),]
  #if number of rows of the table is greater than 3, remove the last row
  #if 'missing_Dates' is one row, 
  if(nrow(missing_Dates)==2){ #>2
    missing_Dates <- as.data.frame(missing_Dates[1,])
  }
  
  if(nrow(missing_Dates)==1){ #>2
    missing_Dates = matrix(0, 0, 3)
    colnames(missing_Dates) <- c("from","to","No_of_days")
    missing_Dates <- as.data.frame(missing_Dates)
  }
  
  if(nrow(missing_Dates)>2){ #>2
    missing_Dates = missing_Dates[-nrow(missing_Dates),]
    ##missing_Dates = missing_Dates[(4:nrow(missing_Dates)),]
  }
  return(missing_Dates)
}


#function to return list of unique dates (in the 'Date' field) of a dataset
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

#function to convert 'Date' field in any format to format 'yyyy-mm-dd'.
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

#function to check whether the uploaded dataset contains the four important fields: "Date","Hour","InCount", "LocationName". Returns "1" if true and "0" if false
uploaded_fieldnames <- function(data, essential_Fields){
  names_uploaded <- essential_Fields %in% colnames(data)
  leng_name <- length(which(names_uploaded=="TRUE"))
  return(leng_name)
}

#function to check that all the uploaded records fall within appropriate time range i.e. start date of the historical data and the current time. Returns "1" if true and "0" if false
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

#function to check whether there is date overlap between historical data and an uploaded record. Returns "1" if true and "0" if false
dateOverlap_Checker <- function(history_footfall, data){
  #unique dates in the footfall (database) data
  uniqueDate_footfallDatabase <- uniq_Dates(history_footfall)
  #unique dates in the uploaded data
  uniqueDate_uploaded <- uniq_Dates(data)
  #check that there is no overlap between the dates dates
  overlap_Dates <- which(uniqueDate_uploaded %in% uniqueDate_footfallDatabase)
  return(length(overlap_Dates))
}


#function to remove whitespace in the 'LocationName' field of a footfall dataset
remove_whiteSpace_in_Camera_Name <- function(data, lists_Loc_Correct){
  vec_Name <- trimws(as.vector(data$LocationName), which="right") #trailing whitespace
  vec_Name <- trimws(vec_Name, which="left") #leading whitespace
  vec_Name <- matrix(vec_Name,,1)
  colnames(vec_Name) <- "LocationName"
  data$LocationName <- vec_Name
  return(data)
}


#function to detect typo in the list of camera location. Returns "1" if true and "0" if false
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

#Preparing dataset for training
#function to merge footfall aggregate with its corresponding predictors records
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
  
  #setting up the directories
  #directory for the aggregated HF
  file_here <- paste0(ROOT_DIR,"lcc-footfall/webapp/downloaded_footfall dataset/aggregated_historical_HF/")
  #parameter file directory
  parameter_directory <- paste0(ROOT_DIR,"lcc-footfall/webapp/downloaded_footfall dataset/predictors_INFO/")
  #directory for other items
  other_dir <- paste0(ROOT_DIR, "lcc-footfall/webapp/misc/")
  
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
    load(paste(other_dir, "random_forest_model.rda", sep=""))
    
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
    auc_plot2(data, HF_startDate=HF_startDate, plot_StartDate=(input$earliestDate*12), y_new, addTrend = input$trendLine, chartType=input$chartType)
  })
  

  #----------------------
  #plotting the map
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
    load(paste(other_dir, "random_forest_model.rda", sep=""))
    
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
  twentyFourHours_HF_aggre <- read.table(file=paste(file_here, "twentyFour_HoursAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)
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
      aggre_footfall <- read.table(file=paste(file_here, "twentyFour_HoursAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)
      aggre_footfall <- convert_Date(aggre_footfall)
      
      #to match the footfall data with the predictors data and subset the last 3 years of the dataset
      cleaned_data_for_training <- data_Preparation_for_training(aggre_footfall=aggre_footfall, predictors=predictors, modelName ="randomForest", training_length_in_yrs = 3)
      #re-train the model
      pred_model <- randomForest(InCount ~., data=cleaned_data_for_training)
      
      #save the predictive model (parameters)
      save(pred_model, file = paste(other_dir, "random_forest_model.rda", sep="")) 
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
      existing_time_aggre_HF <- read.table(file=paste(file_here, time_aggregation[j], "Aggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep = ",", head=TRUE)
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
      write.table(aggregates_updated, file=paste(file_here, time_aggregation[j], "Aggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", row.names=FALSE) 
      
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
    
