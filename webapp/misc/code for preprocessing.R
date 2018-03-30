
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
data <- read.table(file="Copy of Monthly Data Feed-January 2017 - 20170208.csv", sep=",", head=TRUE)

  file_For_Missing_Data2 

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
	overlap_Dates <- length(which(overlap_Dates=="TRUE"))
      return(overlap_Dates)
}


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