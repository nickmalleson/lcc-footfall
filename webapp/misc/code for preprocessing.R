
#setwd("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/format1/")
setwd("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/")

#code for combining  multiple files (.dbf or .csv) in a directory - Note, the head must be the same.
head(data)
#this code combines the the necessary fields from the two files "combined.csv" and "combined2.csv"
#The above .csv files are downloaded from the website "https://datamillnorth.org/dataset/leeds-city-centre-footfall-data"
#Two sets of headers were identify in the files, and are combined separately as "combined.csv" and "combined2.csv"

#aggregate the daily scale
#selecting important (common) fields from both file format to generate a single file.
#the fields of interest are:
install.packages("lubridate")
library(lubridate)

1.#"Date" - date of the count
2. #"Hour" - hour of the day  (In "combined2.csv", the "Hour" field is not complete)
3. #"InCount" - count of footfall

#As a result #2 above, we will focus on daily aggregation for now.
#aggregate the data into daily scale, across all the stations.
#First, convert multiple date format to uniform format

#data <- read.table(file="combined.csv", sep=",", head=TRUE)  #head(data)
#data <- read.table(file="monthly-data-feed-oct-2014-20141107.csv", sep=",", head=TRUE)
#data <- as.character(c("2015-03-12","14-03-2015","2015-04-23","23-04-2015"))

data <- read.table(file="footfall.csv", sep=",", head=TRUE)

#To ensure "Date" column conform to the format "yyyy-mm-dd"
#dateField <- matrix(data,,1)
dateField <- matrix(data$Date,,1)
colnames(dateField) <- c("Date") # data[1:10000,] head(data)
#to detect dates not in right format (i.e. yyyy-mm-dd)
converDate1 <- as.Date(parse_date_time(dateField,"dmy"))
listInconvertible <- which(!is.na(converDate1))
dateField[listInconvertible] <- as.character(converDate1[listInconvertible])   #data[89480:89559,]
#append back to the dataset
data$Date <- dateField
#subsetting the needed dataset
data <- cbind(data$Date, data$Hour, data$InCount)
colnames(data) <- c("Date","Hour","InCount")  ##head(data)

#NEXT:
#To ensure that "Hour" column conform to the format "0, 1, 2, 3....hours
subsetmatrix(data$Hour)

#To remove any data with no "Hour" information.. 


seq(as.Date("2018-03-20"), Sys.Date(), by="day")



seq(as.Date("2000/1/1"), as.Date("2003/1/1"), by = "quarter")
missingDate <- as.character(c("01/02/2001"))
#colnames(missingDate) <- c("Date") 
converDate1 <- as.Date(parse_date_time((missingDate),"ymd"))

#data <- as.character(c("2015-03-12","14-03-2015","2015-04-23","23-04-2015"))

data <- read.table(file="footfall.csv", sep=",", head=TRUE)

data <- as.character(c("2015-03-12","14-03-2015","2015-04-23","23-04-2015"))

data <- 


dataValues <- "01/02/2001"
missingDate <- as.character(dataValues)
#To ensure "Date" column conform to the format "yyyy-mm-dd"
dateField <- matrix(missingDate,,1)
colnames(dateField) <- c("Date") # data[1:10000,] head(data)
#to detect dates not in right format (i.e. yyyy-mm-dd)
converDate1 <- as.Date(parse_date_time(dateField,"dmy"))
listInconvertible <- which(!is.na(converDate1))
dateField[listInconvertible] <- as.character(converDate1[listInconvertible])   #data[89480:89559,]
#append back to the dataset
dataValues <- dateField



library(data.table) 
setDT(df1)[,.(date = seq(as.Date("2015-03-12"), as.Date("2015-04-23"), by = "1 day")) ,.(id1, id2)]

as.Date(as.Date("2011-12-30"):as.Date("2012-01-04"), origin="1970-01-01")

find.missing.dates<-function(data){
out<-sapply(2:nrow(data),function(i){data$sdate[i]-data$sdate[i-1]})
out<-c(NA,out)
out
}


 #detecting missing data
  historical_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/footfall_31_12_2016.csv", sep=",", head=TRUE)
  #create a list dates occuring in the dataset
  dataValues <- historical_footfall$Date
  DateFromData <- as.character(dataValues)
  #To ensure "Date" column conform to the format "yyyy-mm-dd"
  dateField <- matrix(DateFromData,,1)
  colnames(dateField) <- c("Date") # data[1:10000,] head(data)
  #to detect dates not in right format (i.e. yyyy-mm-dd)
  converDate1 <- as.Date(parse_date_time(dateField,"dmy"))
  listInconvertible <- which(!is.na(converDate1))
  dateField[listInconvertible] <- as.character(converDate1[listInconvertible])   #data[89480:89559,]
  #append back to the dataset
  dataValues <- dateField   
  #append current date to the list..
  dataValues <- rbind(dataValues, as.character(Sys.Date()))
  #to identify gaps in the dataset.
  DF <- as.Date(dataValues)
  DF_Dates <- diff(DF)

  data.frame(from = (DF[DF_Dates>1]+1), to = (DF[c(1, DF_Dates)>1]-1), 
                                      No_of_days missing = (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]+1))
  ) 

 #adding text to describe the missing data 

 output$testHTML <- renderText({
    
    paste("<b>The table above shows the list of data ranges in which footfall data are missing.", "<br>", 

  output$testHTML <- renderText({paste("<b>The table above shows the list of data ranges in which footfall data are missing.", "<br>")})
  output$text2 <- renderText({paste("Search for the missing data in either of the following sources:")})
  output$text3 <- renderText({paste("1. https://datamillnorth.org/dataset/leeds-city-centre-footfall-data")})
  output$text4 <- renderText({paste("2. https://data.gov.uk/dataset/leeds-city-centre-footfall-data")})
  output$text5 <- renderText({paste("Note: Ensure that the data contain the following three columns:")})
  output$text6 <- renderText({paste("(a) 'Date' - in either of these formats: 'dd/mm/yyyy' OR 'yyyy-mm-dd'")})
  output$text7 <- renderText({paste("(b) 'Hour' - 'Hour of the day', i.e. 0, 1, 2, .... 23.")})
  output$text8 <- renderText({paste("(c) 'InCount' - Hourly aggregate of footfall count")})
  output$text9 <- renderText({paste("Upload a .csv file to update the database")})  
  
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

getwd()
setwd("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/")

historical_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/footfall_31_12_2016.csv", sep=",", head=TRUE)
#data <- read.table(file="Copy of Monthly Data Feed-January 2017 - 20170208.csv", sep=",", head=TRUE)
data <- read.table(file="copyy_222.csv", sep=",", head=TRUE) #3623 & 3624

#file_For_Missing_Data2 

head(data)

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
		names_uploaded <- c("Date","Hour","InCount") %in% names(data)
		leng_name <- length(which(names_uploaded=="TRUE"))
		return(leng_name)
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

#function to check that uploaded contains the three fields, "Date","Hour","InCount"
uploaded_fieldnames <- function(data){
  names_uploaded <- c("Date","Hour","InCount") %in% colnames(data)
  leng_name <- length(which(names_uploaded=="TRUE"))
  #return(leng_name)
}


#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
#Removing the outliers using Median absolute deviation method
outliersMAD <- function(data, MADCutOff = 3.5, replace = NA, values = FALSE, bConstant = 1.4826, digits = 2) {
    #compute number of absolute MADs away for each value
    #formula: abs( ( x - median(x) ) )/ mad(x)
    absMADAway <- abs((data - median(data, na.rm = T))/mad(data, constant = bConstant, na.rm = T))
    #subset data that has absMADAway greater than the MADCutOff and replace them with replace
    #can also replace values other than replace
    data[absMADAway > MADCutOff] <- replace
    
    if (values == TRUE) { 
        return(round(absMADAway, digits)) #if values == TRUE, return number of mads for each value
    } else {
        return(round(data, digits)) #otherwise, return values with outliers replaced
    }
}

http://cainarchaeology.weebly.com/r-function-for-univariate-outliers-detection.html
#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
outlier <- function (x,method="mean",addthres=FALSE){
  if (method=="mean") {
    avrg <- mean(x)
    stdev <-sd(x)
    dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=abs(x-avrg)>2*stdev)
    midp <<- avrg
    lower <<- avrg-2*stdev
    upper <<- avrg+2*stdev
    outliern <<- length(which(dtf=="TRUE"))
  } else {}
  if (method=="median") {
    med <- median(x)
    MAD <-median(abs(med-x))
    dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=abs(x-med)>2*(MAD/0.6745))
    midp <<- med
    lower <<- med-2*(MAD/0.6745)
    upper <<- med+2*(MAD/0.6745)
    outliern <<- length(which(dtf=="TRUE"))
    } else {}
  if (method=="boxplot") {
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    IntQ <-Q3-Q1
    dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=x<Q1-1.5*IntQ | x>Q3+1.5*IntQ)
    midp <<- median(x)
    lower <<- Q1-1.5*IntQ
    upper <<- Q3+1.5*IntQ
    outliern <<- length(which(dtf=="TRUE"))
    } else {}
  if (addthres==TRUE) {
    p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n number of outliers detected=", outliern, "\n( outlier detection method=", method, ")"), y="observation value") + geom_hline(yintercept = midp, colour="black", linetype = "longdash") + geom_hline(yintercept = lower, colour="black", linetype = "longdash") + geom_hline(yintercept = upper, colour="black", linetype = "longdash")
    } else {
  p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n( outlier detection method=", method, ")"), y="observation value") #requires 'ggrepel'
  }
  return(outliern)
}

require("ggrepel") 

x <- x$InCount

#---------------------------------
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

#function to identify outliers
outliers <- function(data=result1){
  x<-as.numeric(as.vector(data$InCount))  #median(x, na.rm=TRUE)
  ind_na <- which(is.na(x))
  ind_not_na <- which(!is.na(x))
  x_2 <- x[ind_not_na]
  med <- median(x_2)
  MAD <-median(abs(med-x_2))
  dtf <<- data.frame(ID=seq.int(length(x_2)), obs=x_2, outlier=abs(x_2-med)>3.5*(MAD/0.6745))
  #dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier= ((0.6745 * abs(x-med))/MAD>3.5))
  midp <<- med
  lower <<- med-2*(MAD/0.6745)
  upper <<- med+2*(MAD/0.6745)
  outliern <<- length(which(dtf=="TRUE"))
  return(dft)
} 

#function to convert list of dates to format 'yyyy-mm-dd'. Returns the dataset, but with date format changed
convert_Date <- function(data=orig_Data){
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


#---------------------------------
historical_footfall <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/footfall_31_12_2016.csv", sep=",", head=TRUE)

#create list of all days between two range (i.e. start and end date of historical footfall dataset)
#start_date <- min(uniq_Dates(historical_footfall)) #library(lubridate)
#end_date <- max(uniq_Dates(historical_footfall))

#allDays_listed <- seq(as.Date(start_date), as.Date(end_date), by=1)


#Given any dataset, generate plottable data.frames for different time aggregation

orig_Data <- read.table(file="file_3daysData.csv", sep=",", head=TRUE)  #head(orig_Data)#=## 
time_aggre = hours_of_the_Day[[2]]
cameraLoc = "LocationName"

#------------------------
aggregate_Data_for_Plot <- function(orig_Data, cameraLoc = "LocationName", time_aggre){ #if there are multiple camera Locations

selected_fields <- function()
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

}



#aggregate 'InCount' values across all cameral location for each day and hour of the day, (i.e. using the 'Id' field). 
#detect whether any 

aggregate_by_Location <- function(orig_Data_sub = data_Subset){

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
		}

		#clean it up
		loc_agg_data <- loc_agg_data[-which(loc_agg_data[,1]=="0"),]
		rownames(loc_agg_data) <- 1:nrow(loc_agg_data)
		loc_agg_data <- as.data.frame(loc_agg_data)
		colnames(loc_agg_data) <- c("Date","Hour","Id","InCount")
return(loc_agg_data)

}

#Given a footfall dataset (containing a column 'Id' - concatenation of unique day-time), sum all 'InCount' by unique 'Hour' of the day

footfall_by_time_of_the_Day <- function(aggregate_by_Location){
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

	#create aggregated data to plot i.e. for each time scale; whole day, morning, evening & morning.
	unique_dates <- unique(data_subset$Date)
	unique_Times <- time_aggre 


	time_agg_data <-  matrix(0, 1, 2)
	rownames(time_agg_data) <- length(unique_Times)
	row.N <- "100" #just any number different from 'length(unique_Times)' 
	appd_Row <- matrix(0, 1, 2)
	rownames(appd_Row) <- length(unique_Times)

	for(i in 1:length(unique_dates)){ #i<-1
		timeT <- data_subset[which(data_subset$Date==unique_dates[i]),c("Hour")]
		#InCt <- data_subset[which(data_subset$Date==unique_dates[i]),]
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
#--------------------------------------
#--------------------------------------


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

aggregate_Location <- function(orig_Data_sub = data_Subset){

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
		}

		#clean it up
		loc_agg_data <- loc_agg_data[-which(loc_agg_data[,1]=="0"),]
		rownames(loc_agg_data) <- 1:nrow(loc_agg_data)
		loc_agg_data <- as.data.frame(loc_agg_data)
		colnames(loc_agg_data) <- c("Date","Hour","Id","InCount")
return(loc_agg_data)

}


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
    print("point2")
  }
  
  #clean it up
  loc_agg_data <- loc_agg_data[-which(loc_agg_data[,1]=="0"),]
  rownames(loc_agg_data) <- 1:nrow(loc_agg_data)
  loc_agg_data <- as.data.frame(loc_agg_data)
  colnames(loc_agg_data) <- c("Date","Hour","Id","InCount")
  return(loc_agg_data)
  
}


#Given a footfall dataset (containing a column 'Id' - concatenation of unique day-time), sum all 'InCount' by unique 'Hour' of the day
footfall_by_time_of_the_Day <- function(aggregate_by_Location){
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

	#create aggregated data to plot i.e. for each time scale; whole day, morning, evening & morning.
	unique_dates <- unique(data_subset$Date)
	unique_Times <- time_aggre 


	time_agg_data <-  matrix(0, 1, 2)
	rownames(time_agg_data) <- length(unique_Times)
	row.N <- "100" #just any number different from 'length(unique_Times)' 
	appd_Row <- matrix(0, 1, 2)
	rownames(appd_Row) <- length(unique_Times)

	for(i in 1:length(unique_dates)){ #i<-1
		timeT <- data_subset[which(data_subset$Date==unique_dates[i]),c("Hour")]
		#InCt <- data_subset[which(data_subset$Date==unique_dates[i]),]
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
  return(missing_Dates)
}



orig_Data <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/historical_HF/footfall_31_12_2016.csv", sep=",", head=TRUE)
head(orig_Data)

newww <- missingData(orig_Data)

as.data.frame(newww)

newwwww <- apply(newww, 2, rev)



missingData <- missing_Dates

as.data.frame(newww)
matrix(newww,,3)

data_Subset <- subset_Dataset(orig_Data)
aggregate_across_location_by_Date <- aggregate_Location(data_Subset)
aggregate_time_of_the_Day <- footfall_by_time_of_the_Day(aggregate_across_location_by_Date)


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
#------------------------
i<-1

#write.table(aggregate_time_of_the_Day, file=paste("beforeOutlier", length(hours_of_the_Day[[i]]),".csv", sep=""), sep=",") 





	  aggregate_time_of_the_Day <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/aggregated_historical_HF/twentyFour_Hours.csv", sep=",", head=TRUE)

	  #identify outliers ("0" - NULL data point, "1" - outliers, "2" - not outliers)
        outlier_events <- outliers(aggregate_time_of_the_Day)
        #append the outlier list to the result
        result1 <- cbind(aggregate_time_of_the_Day, outlier_events)
        colnames(result1)<- c("Date","InCount","outlier")

	  write.table(result1, file="filell.csv", sep=",", row.names=FALSE)

	  read.table(file="filell.csv", sep=",", head=TRUE, row.names=TRUE)

 HF_directory = "C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/historical_footfall/"  
  write.table(aggregate_across_location_by_Date, file=paste(HF_directory,"aggregate/","wholeDay.csv", sep=""), sep=",") 

aggregated_Data <- aggregate_by_Location(data_Subset)

paste(HF_directory,"aggregate","/","twentyFour_Hours.csv", sep="")


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

tableinv <- function(x){
      y <- x[rep(rownames(x),nrow(x)),1:(ncol(x)-1)]
      rownames(y) <- c(1:nrow(y))
      return(y)}
survivors <- as.data.frame(iris)
surv.invtab <- tableinv(survivors)

 hist_table <- apply(history_footfall, 2, rev)
  
getwd()

library(foreign)

library(foreign)
#setwd("c:/temp/help/")

files <- list.files(pattern="\\.dbf$")
all.the.data <- lapply(files, read.dbf, as.is=FALSE)
DATA <- do.call("rbind",all.the.data)

write.table(file="stss_burg_CM_500.csv", sep=",",)

all.the.files <- list.files("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/most_updated_historical/",full=TRUE)


DATA <- do.call("rbind", lapply(list.files("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/most_updated_historical/",
	full=TRUE),read.csv, header=TRUE))

head(DATA)


head(DATA)




length(hours_of_the_Day)

for(i in 1:4){ #i<-1

file_names <- "C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/most_updated_historical/"
file_names <- dir() #where you have your files

your_data_frame <- do.call(rbind,lapply(file_names,read.csv))


inputData <- read.table(file="file_3daysData.csv", sep=",", head=TRUE)  #head(orig_Data)
orig_Data <- inputData
hours_of_the_Day <- list(c(0:23), c(8:17), c(18:20), c(21,22,23, 0, 1, 2, 3, 4, 5))

if(i==1){
result1 <- aggregate_Data_for_Plot(orig_Data, cameraLoc = "LocationName", time_aggre = hours_of_the_Day[[i]])
#remove outliers
outliersLoc <- outliers(result1) 
#append the outlier list to the result
result1 <- cbind(result1, outliersLoc)
colnames(result1)<- c("Date","InCount","outlier")
write.table(result1, file="wholeDay.csv", sep=",")
}

if(i==2){
result1 <- aggregate_Data_for_Plot(orig_Data, cameraLoc = "LocationName", time_aggre = hours_of_the_Day[[i]])
#remove outliers
outliersLoc <- outliers(result1) 
#append the outlier list to the result
result1 <- cbind(result1, outliersLoc)
colnames(result1)<- c("Date","InCount","outlier")
write.table(result1, file="dayTime.csv", sep=",")
}

if(i==3){
result1 <- aggregate_Data_for_Plot(orig_Data, cameraLoc = "LocationName", time_aggre = hours_of_the_Day[[i]])
#remove outliers
outliersLoc <- outliers(result1) 
#append the outlier list to the result
result1 <- cbind(result1, outliersLoc)
colnames(result1)<- c("Date","InCount","outlier")
write.table(result1, file="eveningTime.csv", sep=",")
}

if(i==4){
result1 <- aggregate_Data_for_Plot(orig_Data, cameraLoc = "LocationName", time_aggre = hours_of_the_Day[[i]])
#remove outliers
outliersLoc <- outliers(result1) 
#append the outlier list to the result
result1 <- cbind(result1, outliersLoc)
colnames(result1)<- c("Date","InCount","outlier")
write.table(result1, file="nightTime.csv", sep=",")
}

}


      if(i==2){
          result1 <- aggregate_Data_for_Plot(orig_Data=updated_FootfallDataset, cameraLoc = "LocationName", time_aggre = hours_of_the_Day[[i]])
          #remove outliers
          outliersLoc <- outliers(result1) 
          #append the outlier list to the result
          result1 <- cbind(result1, outliersLoc)
          colnames(result1)<- c("Date","InCount","outlier")
          write.table(result1, file="dayTime.csv", sep=",")
          print(time_aggre)
        }
        
        if(i==3){
          result1 <- aggregate_Data_for_Plot(orig_Data=updated_FootfallDataset, cameraLoc = "LocationName", time_aggre = hours_of_the_Day[[i]])
          #remove outliers
          outliersLoc <- outliers(result1) 
          #append the outlier list to the result
          result1 <- cbind(result1, outliersLoc)
          colnames(result1)<- c("Date","InCount","outlier")
          write.table(result1, file="eveningTime.csv", sep=",")
          print(time_aggre)
        }
        
        if(i==4){
          result1 <- aggregate_Data_for_Plot(orig_Data=updated_FootfallDataset, cameraLoc = "LocationName", time_aggre = hours_of_the_Day[[i]])
          #remove outliers
          outliersLoc <- outliers(result1) 
          #append the outlier list to the result
          result1 <- cbind(result1, outliersLoc)
          colnames(result1)<- c("Date","InCount","outlier")
          write.table(result1, file="nightTime.csv", sep=",")
          print(time_aggre)
        }
        


uniq_Dates(updated_FootfallDataset)

getwd()

hours_of_the_Day[]


dd[with(dd, order(-z, b)), ]





cbind(orig_Data, withID)


read footfall dataa.... , with no Hour, 


head(historical_footfall)




#for each day, create list of hours

days_time_merged <- NULL
for(i in 1:length(allDays_listed)){
	

}







#function 
footfall_data_listed <- seq(as.vector(min(uniq_Dates(historical_footfall))), as.vector(max(uniq_Dates(historical_footfall))), by="days")


seq(as.Date("2011-12-30"), as.Date("2012-01-04"), by="days")



ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier))


 if (addthres==TRUE) {#dev.new()
method="median"
    p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + 
		#geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", 
		#box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + 
		labs(x=paste("observation ID number\n number of outliers detected=", outliern, "\n( outlier detection method=", method, ")"), 
		y="observation value") + geom_hline(yintercept = midp, colour="black", linetype = "longdash") #+ 
		geom_hline(yintercept = lower, colour="black", linetype = "longdash") + 
		geom_hline(yintercept = upper, colour="black", linetype = "longdash")
    } else {
  p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + 
		geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", 
		box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + 
		labs(x=paste("observation ID number\n( outlier detection method=", method, ")"), 
		y="observation value") #requires 'ggrepel'
  }

#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
x <- read.table(file="file_with_issues.csv", sep=",", head=TRUE) #3623 & 3624
x <- read.table(file="footfall_31_12_2016.csv", sep=",", head=TRUE) #3623 & 3624


#x <- c(2,3,1,5,2,1,1.5,2,9,2,4,1,3,2,2,5,12,1,2,3,2,3,4,2,3,7,13,1,20)

outliersMAD(x) 
result <- outlier(x$InCount, method="median",addthres=TRUE)


names(result[1,])

aaaa <- uploaded_fieldnames(data)

if(length() <- length()
#if appended the resulting missing table is reduced to the following:



if((leng_name<-uploaded_fieldnames(data))!=3){issue1=1}

if((out_Len<-dateRange_Checker(historical_footfall, data))>0){issue2=1}

if((overlap_Dates<-dateOverlap_Checker(historical_footfall, data))>0){issue3=1}

if(issue1==1){
print("The uploaded file does not contain one of the following field names")
}

if(issue2==1){
print("One or some of the uploaded dates fall outside the expected range (i.e. earliest date of footfall (database) and the current date")
}

if(issue3==1){
print("Some dates in the uploaded file overlap with the footfall database")
}


  #uploaded data to fill gaps in the historical footfall record.....: Purpose: observe command is used where no output is returned.
  observe({
  req(input$file1)
  #To check the gaps that an uploaded file fill
  uploaded_file <- read.csv(input$file1$datapath,
                                    header = TRUE,
                                    sep = ",")#,
	shinyjs::hide("upload")

	if((leng_name<-uploaded_fieldnames(data))!=3){issue1=1}
	if((out_Len<-dateRange_Checker(historical_footfall, data))>0){issue2=1}
	if((overlap_Dates<-dateOverlap_Checker(historical_footfall, data))>0){issue3=1}

	if(issue1==1){
		print("The uploaded file does not contain one of the following field names")}
	if(issue2==1){
		print("One or some of the uploaded dates fall outside the expected range (i.e. earliest date of footfall (database) and the current date")}
	if(issue3==1){
		print("Some dates in the uploaded file overlap with the footfall database")}

	total_issues <- issue1 + issue2 + issue3

	#if there is no issues, then show "Upload" button 
	if(total_issues==0)
      	shinyjs::show("upload")

	})


#NOW CODES TO PLOT..
#-----------------------------------------#RSHINY.....
  #to upload the file...
  observe({
    shinyjs::hide("upload")
    
    if(issue1==0) #locationSpecified()
      shinyjs::show("upload")
  })
    


Date ranges where "Hourly" footfall aggregation is not available.. 

if(issue1
...THE UPLOAD CANNOT BE PROCESSED!







#check that the uploaded data contains the three basic field
names()

Check that the #

which(uniqueDate_footfallDatabase %in% uniqueDate_uploaded)

a<-c("a","b","c","d","e")
b<-c("a","f","g")

a%in%b

b%in%a




Final resolve:
All the overlapping dates from the uploaded data (if any) will be ignored, 
while the non-overlap dates will be ... 








#collate the unique dates


#combining all missing dates from our footfall database..







dataValues[length(dataValues)]

aaaa[length(dataValues)-1]



DF <- matrix(as.character(c("2015-03-12","2015-04-23","2015-04-24","2015-06-09")),,1)
colnames(DF)<- c("V2")
DF <- as.Date(dataValues)    #diff(DF)  head(dataValues)
diff(DF$V2) 

dfgaps <-data.frame(start= DF[c(1, diff(dataValues))>1, ]$V2, end=   
DF[diff(DF$V2)>1, ]$V2) 



>

At this point an alternative approach: 

# Scan for differences > 1 
 > 
Time differences in days 
  [1]  1  1  1  1  1  1  1  1  1  8 61  1  1  1 

#Records at the start of gaps 
 > dtdta[diff(dtdta)>1, ] 
      V1         V2   V3 
10 2925 2002-02-26 20.5 
11 2926 2002-03-06  0.0 

$Records at the end of gaps 
 > dtdta[c(1, diff(dtdta$V2))>1, ] 
      V1         V2 V3 
11 2926 2002-03-06  0 
12 2927 2002-05-06  0 

#Gap dataframe 
 > dfgaps <-data.frame( start= DF[c(1, diff(DF$V2))>1, ]$V2, end=   
DF[diff(DF$V2)>1, ]$V2) 
 > dfgaps 




df1 %>%
   rowwise() %>% 
   do(data.frame(.[1:2], date = seq(.$min_date, .$max_date, by = "1 day")))




uniqueDates <- as.vector(unique(dateField))

databaseFootfall <- NULL

for(i in 1:length(uniqueDates)){ #i<-5   117   #subsetD$InCount  #as.Date(subsetD$Date[1])+30  head(dateField)
   
	subsetD <- data[which(as.Date(dateField) == uniqueDates[i]),]       
	total_Foot <- sum(subsetD$InCount)
	databaseFootfall <- rbind(databaseFootfall, cbind(as.data.frame(uniqueDates[i]), total_Foot))
}


#Aggregate it by hour first, then generate days aggregate 
morning aggregated 6-9, 10-4, 5-6 night whole day... 



begin <- "10:31:00"

Hour

end <- gsub("[: -]", "-2" , begin, perl=TRUE)


##now work on the one with hour minutes and seconds..
databaseFootfall <- databaseFootfall[order(as.matrix(databaseFootfall[,1])),]

combined_dailyAgg<-databaseFootfall 
write.table(combined_dailyAgg, file="combined_dailyAgg.csv", sep=",")


head(databaseFootfall)







as.Date(ddd)+30

converDate2 <- as.Date(parse_date_time(dateD,"ymd"))


dateD[listInconvertible] 


ddd <- c("2015-03-12","14-03-2015","2015-04-23","23-04-2015")

converDate1 <- which(!is.na(as.Date(parse_date_time(ddd,"dmy")))) #to detect dates not in the right format (i.e. yyyy-mm-dd)

formatDate <- c(as.data.frame(as.Date(parse_date_time(ddd[converDate1],"dmy"))))





uniqueDates <- unique(ddd$Date)

databaseFootfall <- NULL

for(i in 1:length(uniqueDates)){ #i<-5   117   #subsetD$InCount  #as.Date(subsetD$Date[1])+30
   
	subsetD <- ddd[which(as.vector(ddd$Date) == uniqueDates[i] ),]  #head(subsetD) #as.Date(parse_date_time(as.vector(uniqueDates[i]),"dmy"))
      
	total_Foot <- sum(subsetD$InCount)
	databaseFootfall <- rbind(databaseFootfall, cbind(as.data.frame(uniqueDates[i]), total_Foot) )
}


nzd$date <- as.Date(nzd$date, format = "%d/%m/%Y")

dateD <- data$Date  #data$Date[89661]  #"2014-11-02"
as.numeric(as.Date(ddd[2]))


 + 30

mode(dateD)# as.Date(dateD[1])+30

ddd <- as.data.frame(matrix(c("2015-03-12","14-03-2015","2015-04-23","23-04-2015"),,1))
colnames(ddd) <- c("Date")

as.Date(ddd) + 30



as.numeric(ddd[1])

#to excel format
as.Date(42705, origin = "1899-12-30")

helpData[["ExcelDate"]] <- 
  as.POSIXct(helpData[["ExcelNum"]] * (60*60*24)
    , origin="1899-12-30"
    , tz="GMT")


converDate1 <- as.Date(parse_date_time(dateD,"dmy"))


df <- as.data.frame(matrix(c("2015-03-12","14-03-2015","2015-04-23","23-04-2015"),,1))




as.Date(parse_date_time(df[converDate1],"dmy"))






df <- structure(list(date = structure(c(12L, 8L, 18L, 6L, 7L, 4L, 14L, 
10L, 1L, 5L, 3L, 17L, 16L, 11L, 15L, 13L, 9L, 2L), .Label = c("13-11-1961", 
"13-Sep-97", "15-Sep-70", "16-Oct-63", "18 Feb 1987", "18 August 1992", 
"19 September 1995", "20 August 1974", "22 August 1989", "22 Jan 2008", 
"03/23/87", "25 February 1987", "26-10-1993", "30-Sep-65", "30 August 1988", 
"5 December 1984", "5 October 1994", "2014-03-02"), class = "factor")), .Names = "date", row.names = c(NA, 
-18L), class = "data.frame")

parse_date_time(x = df,
                orders = c("d m y", "d B Y", "m/d/y", "Y b d"),
                locale = "eng")









uniqueDates <- unique(ddd$Date)

databaseFootfall <- NULL

for(i in 1:length(uniqueDates)){ #i<-1   117
   
	subsetD <- ddd[which(as.vector(ddd$Date) == uniqueDates[i] ),]  #head(subsetD) #as.Date(parse_date_time(as.vector(uniqueDates[i]),"dmy"))

	total_Foot <- sum(subsetD$InCount)
	databaseFootfall <- rbind(databaseFootfall, cbind(as.data.frame(uniqueDates[i]), total_Foot) )
}

as.vector(uniqueDates[i])


as.Date(parse_date_time(as.vector(uniqueDates[i]),"dmy"))


converDate1 <- as.Date(parse_date_time(dateD,"dmy"))

converDate2 <- as.Date(parse_date_time(dateD,"ymd"))

listInconvertible <- which(!is.na(converDate))
dateD[listInconvertible] 


ddd <- c("2015-03-12","14-03-2015","2015-04-23","23-04-2015")

converDate1 <- which(!is.na(as.Date(parse_date_time(ddd,"dmy")))) #to detect dates not in the right format (i.e. yyyy-mm-dd)

formatDate <- c(as.data.frame(as.Date(parse_date_time(ddd[converDate1],"dmy"))))



as.Date(as.character("2015-03-12"), format = "%d/%m/%y")


as.Date("14-04-2015")

as.Date("2015-04-14")


d1 <- strptime("14-03-15", "%Y-%m-%d")
d2 <- strptime("2016-06-27", "%Y-%m-%d")


replace(ddd, converDate1, )

ddd[converDate1] <- c(as.data.frame(as.Date(parse_date_time(ddd[converDate1],"dmy"))))






as.Date(parse_date_time(ddd[converDate1],"dmy"))
[1] "2011-08-31"


#converDate1 <- which(!is.na(as.Date(parse_date_time(ddd,"ymd"))))

ddd[which(!is.na(converDate))] <- 


Sys.Date()

library(lubridate)
day<-"31/08/2011"
day<-"31-08-2011"
day<-"2011-08-31"




[1] "2011-08-31"



#Remove footfall data in which predictor variables are not available for.


#databaseFootfall <- databaseFootfall[order(as.matrix(databaseFootfall[,1])),]

head(databaseFootfall)









# convert date info in format 'mm/dd/yyyy'
strDates <- c("01/05/1965", "08/16/1975")
strDates <- c("01-05-1965", "18-08-1975")    #as.Date(strDates)
dates <- as.Date(strDates, "%m/%d/%Y")






multidate <- function(data, formats){
    a<-list()
    for(i in 1:length(formats)){
        a[[i]]<- as.Date(data,format=formats[i])
        a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
        }
    a[[1]]
    }


data <- data.frame(initialDiagnose = c("14.01.2009", "9/22/2005", 
        "4/21/2010", "28.01.2010", "09.01.2009", "3/28/2005", 
        "04.01.2005", "04.01.2005", "Created on 9/17/2010", "03 01 2010", as.character("2014-01-01")))

mdy <- mdy(data$initialDiagnose) 
dmy <- dmy(data$initialDiagnose) 
mdy[is.na(mdy)] <- dmy[is.na(mdy)] # some dates are ambiguous, here we give 
data$initialDiagnose <- mdy        # mdy precedence over dmy
data



data <- read.table(file="combined.csv", sep=",", head=TRUE)  #monthly-data-feed-oct-2014-20141107
#data <- read.table(file="monthly-data-feed-oct-2014-20141107.csv", sep=",", head=TRUE)

changedDate <- data$Date

data$Date <- multidate(changedDate, c("%m-%d-%Y","%d-%m-%Y","%d %b %y"))

head(data)


databaseFootfall <- NULL

sampD <- c("01-01-2014", "2014-01-01")
which(is.Date("01-01-2014"))
   #

is.Date(as.Date("2014-08-03"))

#----------------------------------------------------------------------------

data$initialDiagnose
[1] 14.01.2009 9/22/2005  12 Mar 97  4/21/2010  28.01.2010 09.01.2009 3/28/2005 
Levels: 09.01.2009 12 Mar 97 14.01.2009 28.01.2010 3/28/2005 4/21/2010 9/22/2005

multidate <- function(data, formats){
    a<-list()
    for(i in 1:length(formats)){
        a[[i]]<- as.Date(data,format=formats[i])
        a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
        }
    a[[1]]
    }

data$initialDiagnose <- multidate(data$initialDiagnose, 
                                  c("%m/%d/%Y","%d.%m.%Y","%d %b %y"))
data$initialDiagnose








#---------------------------------------------------------------------------

 # TRUE

ndate3 <- as.Date("01-01-2014", "%m-%d-%Y")
ndate3 <- as.Date(as.numeric"2014-01-01", "%m-%d-%Y")


as.Date(as.data.frame("01-01-2014"), "%d%m%Y")

ndates1 <- as.Date(dates1, "%d%B%Y"); ndates1

sampD <- c("01-01-2014", "2014-01-01")
ndates1 <- as.Date(sampD, "%d%B%Y")

[1] "2001-09-06" "2004-06-21" "2006-07-04" "2005-08-06"

sdate3 <- "12-15-2001"
ndate3 <- as.Date(sdate3, "%m-%d-%Y"); ndate3
[1] "2001-12-15"


#list all dates between a range
#listofDates <- seq(as.Date("01-01-2014"), as.Date("01-01-2017"), by="days")

#convert dates to a uniform format first i.e. "dd-mm-yyyy"
data22 <- gsub("[0-9]{2}([0-9]{2})$", "\\1", sampD)                                                                                                                                                                                  
data22$Date <- as.Date(data22, format = "%m/%d/%y")                                                                                                                                                                                        
head(data)  

dataDate <- as.Date(data$Date, format="%dd-%mm-%yy")

for(i in 1:length(unique(data$Date))){ #i<-1
   
	subsetD <- data[which(data$Date==as.Date(unique(data$Date)[i])),]  #head(subsetD)
	total_Foot <- sum(subsetD$InCount)
	databaseFootfall <- rbind(databaseFootfall, cbind(as.data.frame(unique(data$Date)[i]), total_Foot) )
}


#Remove footfall data in which predictor variables are not available for.


#databaseFootfall <- databaseFootfall[order(as.matrix(databaseFootfall[,1])),]

head(databaseFootfall)



unique(data$Date)