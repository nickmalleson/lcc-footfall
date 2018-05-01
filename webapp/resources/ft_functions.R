#List of functions

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
  
  actual_Input_from_Slider <- round(plot_StartDate/365, digits = 0)
  print(actual_Input_from_Slider)
  print("OOOOOOOOOOOOOOOOOOOOOOOOOOO")
  
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
    #print(Type)
    
    Date <-as.numeric(xy_1$x)[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
    #print(Date)
    sub_allDays_inbetween <- as.vector(allDays_inbetween)[which(as.vector(xy_1$x)==HF_startDate) + plot_StartDate:(nrow(xy_1)-1)]
    #print(sub_allDays_inbetween)
    label_Ind <- which(as.vector(allDays_inbetween)%in%as.character(dateLabels)[(actual_Input_from_Slider+1):length(as.character(dateLabels))])  #this was the toughest one!
    #print(label_Ind)
    label_to_show <- as.character(dateLabels)[which(as.character(dateLabels) %in% sub_allDays_inbetween)]
    #print(label_to_show)
    
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
            #scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
            scale_x_discrete(limits=label_Ind , labels = label_to_show)
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
            #scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
            scale_x_discrete(limits=label_Ind , labels = label_to_show)
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
              #scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
              scale_x_discrete(limits=label_Ind , labels = label_to_show)
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
              #scale_x_discrete(limits=Date[which(as.character(x_backup)%in%as.character(dateLabels))], labels = x_backup[which(as.character(x_backup)%in%as.character(dateLabels))])
              scale_x_discrete(limits=label_Ind , labels = label_to_show)
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
