
	ROOT_DIR = "C:/Users/monsu/Documents/GitHub/"

	HF_directory = paste0(ROOT_DIR,"/lcc-footfall/webapp/downloaded_footfall dataset/historical_HF/")

  	history_footfall <- read.table(file=paste(HF_directory, "subset_historical_HF_DoNot_REMOVE_or_ADD_ToThisDirectory", ".csv", sep=""), sep=",") 
  
  	#import the predictor information
  	predictors_info <- read.table(file=paste(HF_directory, "predictors_info", ".csv", sep=""), sep=",") 
        
	head(predictors_info)


	#prepare training dataset


---
title: "LCC_Footfall data prediction"
author: "Mo"

# uncomment for markdown output:
output:
  md_document:
    variant: markdown_github
    
# uncomment for html output:
# output:
#  bookdown::html_document2:
#    number_sections: yes
#    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#importing libraries
library(MASS)
library(randomForest)
library(caret)
```

# Introduction
This script provides an R equivalence of the RandomForest algorithm in `LCC_Footfall.ipynb`, used in the predictive analysis of footfall datasets. 
The goal of this module is to compare the performance of the R version with the Python version, before its integration into a web-based tool designed for daily prediction of footfall data in Leeds City Centre.

This module jumped to the `Create Validation Data Set` section of the `LCC_Footfall.ipynb`. In other words, an already prepared dataset is used here.

```{r echo=TRUE}

#Importing cleaned (processed) datasets and preview
data <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/predictors_INFO/predictors_INFO.csv", sep=",",head=TRUE)

#previewing first 5 rows and 5 columns of the dataset
head(data)[1:5,1:5]


head(data)


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
  #missing_Dates <-   data.frame(from = (DF[DF_Dates>1]+1), to = (DF[c(1, DF_Dates)>1]-1), No_of_days = (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]))
  #missing_Dates <-   data.frame(from = (DF[DF_Dates>1]+1), to = (DF[c(1, DF_Dates)>1]-1), No_of_days = (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]))
  missing_Dates <-  data.frame((DF[DF_Dates>1]+1), (DF[c(1, DF_Dates)>1]-1), (DF[c(1, DF_Dates)>1]-1)-(DF[DF_Dates>1]))
  colnames(missing_Dates) <- c("from","to","No_of_days")
  appdI <- matrix("2000-03-03",1,3)
  colnames(appdI) <- c("from","to","No_of_days")
  missing_Dates <- rbind(missing_Dates, appdI)
  return(missing_Dates)
}



missingData(data)

data.frame(missData)


#create a list dates occuring in the dataset
  missData <- missingData(data)  #data(missData)
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
    



print(paste("Data summary: no. of rows = ",nrow(data),"; no. of column = ",ncol(data), sep = ""))

#subsetting the predictors
Xfull <- subset(data, select=-c(InCount))   #head(Xfull)
xname <- colnames(Xfull)

#subsetting the dependent variable (footfall count)
Yfull <- subset(data, select=c(Date, InCount))
yname <- c("y")

#merge as dataframe
XYfull <- as.data.frame(cbind(Xfull, Yfull))
colnames(XYfull) <- c(xname, yname)

#partitioning the dataset into training and testing sets
set.seed(123)
inTraining <- createDataPartition(XYfull$y, p = .66666, list = FALSE)
training_set <- XYfull[inTraining,] 
testing_set  <- XYfull[-inTraining,]

#Run a k-fold cross validation using the "training_set" (66.66% partition). 
#Combine k-1 subsets in turns, and predict the last (kth) subset
#Each time, compute the Median R2, Median, and predictive accuracy.

#First, randomise the dataset
training_set<- training_set[sample(nrow(training_set)),]

folds <- cut(seq(1,nrow(training_set)), breaks=10, labels=FALSE)
training_set$holdoutpred <- rep(0,nrow(training_set)) #nrow(training)

k =10;

result_List <- matrix(0, k, 6)

for(i in 1:k){ # i = 1

  subset_train <- subset(training_set, select=-c(holdoutpred)) 
  train <- subset_train[(folds != i), ] #Set the training set nrow(train)
  validation <- subset_train[(folds == i), ] #Set the validation set, #nrow(validation)

  #using Regression method
  regre_model <- lm(y~.,data=train) 
  mse_1 <- mean(regre_model$residuals^2)
  r2_1 <- summary(regre_model)$adj.r.squared
  #newpred <- predict(regre_model, newdata=validation)
  #corr_acc_1 <- cor(data.frame(cbind(validation$y, newpred)))
  result_List[i,1] <- mse_1
  result_List[i,2] <- r2_1
  #result_List[i,3] <- corr_acc_1[1,2]

  ##Using randomForest algorithm
  randomForest <- randomForest(y ~., data=train)
  mse_2 <- mean(randomForest$mse)
  r2_2 <- mean(randomForest$rsq)
  #predict_Random <- predict(randomForest, validation)
  #corr_acc_2 <- cor(data.frame(cbind(validation$y, as.vector(predict_Random))))
  result_List[i,4] <- mse_2
  result_List[i,5] <- r2_2
  #result_List[i,6] <- corr_acc_2[1,2]

 }

#Feature importance.

print(paste("Regression: mse = ", mean(result_List[i,1]), "r2 = ", mean(result_List[i,2]), "pred_acc = ", mean(result_List[i,3])) )
print(paste("RandomForest: mse = ", mean(result_List[i,4]), "r2 = ", mean(result_List[i,5]), "pred_acc = ", mean(result_List[i,6])) ) 

#fgl.res <- tuneRF(subset(train, select=-c(y)), train[,ncol(train)], stepFactor=1.5)
#randomForest <- randomForest(y ~., data=train, importance=TRUE)
#importance(randomForest, type=1)[1:10,]

```
Hypterparameter tuning to be included....

Work in progress....

library(lubridate)


dateLabels <- seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by="day")

Monday_through <- matrix(0, length(dateLabels), 52)


for(i in 1:length(dateLabels)){ #i<-1
	
	what_Day <- month(as.Date(dateLabels[i]))

	if(what_Day==12){
	Monday_through[i,1] <- 1
		}
	}

write.table(Monday_through, file="Monday.csv", sep=",")


#------------------------------------------------------------------------

seven_Ones <- c(1, 1, 1, 1, 1, 1, 1)


dateLabels <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="day")

Monday_through <- matrix(0, length(dateLabels), 52)

j<-0
k<-1

for(i in 1:52){ #i<-1
	
	j <- j + 1
	
	Monday_through[k:(k+6), j] <- seven_Ones   #Monday_through[1:30, 1:10]  #1:7

	k <- k + 7
	}

write.table(Monday_through, file="Monday.csv", sep=",")




write.table(Monday_through, file="Monday.csv", sep=",")





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
  colnames(missing_Dates) <- c("from","to","No_of_days")
  appdI <- matrix("2000-03-03",1,3)
  colnames(appdI) <- c("from","to","No_of_days")
  missing_Dates <- rbind(missing_Dates, appdI)
  return(missing_Dates)
}




#LIST FOR FUNCTIONS TO CHECK ERRORS IN THE DATASET
#function to check that uploaded contains the three fields, "Date","Hour","InCount", "LocationName"
uploaded_fieldnames <- function(data=, essential_Fields){
  #essential_Fields <- c("Date","mean_temp","rain")
  names_uploaded <- essential_Fields %in% colnames(data)
  leng_name <- length(which(names_uploaded=="TRUE"))
  return(leng_name)
}


#function to convert list of dates to format 'yyyy-mm-dd'. Returns the dataset, and with date converted to date type
convert_Date <- function(data){
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
  return(data)
}

uploaded_fieldnames()

new_uploadedd <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/predictors_INFO/to be uploaded/weather_info.csv", sep=",", head=TRUE)

leng_name3 <- uploaded_fieldnames(new_uploadedd, essential_Fields =  c("Date","mean_temp","rain")) #che



	predictors_info <- read.table(file="C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/predictors_INFO/predictors_info.csv", sep=",", head=TRUE)

	#extract the predictors info that have weather information.
  	predictors_info_extract <- predictors_info[which(predictors_info$status==1),]  #head(predictors_info_extract)

	weatherInfo <- read.table("C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/downloaded_footfall dataset/predictors_INFO/to be uploaded/weather_info.csv", sep=",", head=TRUE)
	#head(data) #data[2130,]

	#convert date to appropriate format
	predictors_info <- convert_Date(predictors_info) ##  predictors_info[1:5, 1:5]
	weatherInfo <- convert_Date(weatherInfo) ##weatherInfo
	
	id_to_update <- which(predictors_info$Date %in% weatherInfo$Date)

	#for(h in 1:nrow(weatherInfo)){ #h<-1
		predictors_info[id_to_update, c("mean_temp")] <- weatherInfo$mean_temp
		predictors_info[id_to_update, c("rain")] <- weatherInfo$rain
	#}
install.packages("glm")
library(glm)

predictors_info_extract[nrow(predictors_info_extract),]
data[1:5, 1:5]


ROOT_DIR = "C:/Users/monsu/Documents/GitHub/"
HF_directory = paste0(ROOT_DIR,"lcc-footfall/webapp/downloaded_footfall dataset/historical_HF/")
#directory for the aggregated HF
file_here <- paste0(ROOT_DIR,"lcc-footfall/webapp/downloaded_footfall dataset/aggregated_historical_HF/")
#parameter file directory
parameter_directory <- paste0(ROOT_DIR,"lcc-footfall/webapp/downloaded_footfall dataset/")
#directory for other items
other_dir <- paste0(ROOT_DIR, "lcc-footfall/webapp/misc/")
  

#action to re-train prediction model
observeEvent(input$Re-train_Prediction_Model, priority=10, {
    
	#re-import the updated data to retrain the model.
	predictors <- read.table(file=paste(parameter_directory, "predictors_INFO/", "predictors_info", ".csv", sep=""), sep=",", head=TRUE)	
	aggre_footfall <- read.table(file=paste(file_here, "dayTimeAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)

dayTime_HF_aggre_MINUS_outlier[1:10, 1:10]
joined_dataset_for_Training[1:10, 1:10]



#function to merge footfall aggregate with predictors and train the model

model <- function(aggre_footfall, predictors, modelName ="randomForest") {

	# extract days with status 1 (i.e. rows with weather information)
	predictors_info_extract <- predictors[which(predictors$status==1),]  

	#convert dates to right format
	predictors_info_extract <- convert_Date(predictors_info_extract)	

  	#order by dates and extract the mode recent last three years
	predictors_info_extract_subset <- predictors_info_extract[order(predictors_info_extract$Date, decreasing = TRUE),]
	predictors_info_extract_subset <- predictors_info_extract_subset[1:1095, ] #365 * 3

	#drop 'un-needed' fields (for re-train)
	###predictors_info_extract_subset_dropFields <- subset(predictors_info_extract_subset, select=-c("Date","status"))
  
	#import the footfall data (daily aggregates)

	#dayTime_HF_aggre <- read.table(file=paste(file_here, "dayTimeAggregation_DoNot_REMOVE_or_ADD_ToThisDirectory.csv", sep=""), sep=",", head=TRUE)

	#remove the outlier and "NA", and drop the outlier 'column'
	dayTime_HF_aggre_MINUS_outlier <- aggre_footfall[which(aggre_footfall$outlier==2),]
	dayTime_HF_aggre_MINUS_outlier <- subset(dayTime_HF_aggre_MINUS_outlier, select=-c(outlier))

	#To ensure that the 'Date' column in both datasets (predictor dataset and Footfal datasets)are in the right format
	dayTime_HF_aggre_MINUS_outlier <- convert_Date(dayTime_HF_aggre_MINUS_outlier)
	predictors_info_extract_subset <- convert_Date(predictors_info_extract_subset)

	#now merge both datasets using the 'Date' column
	#library(dplyr)	
	joined_Footfall_Predictors <- left_join(dayTime_HF_aggre_MINUS_outlier, predictors_info_extract_subset, by=c("Date","Date"))





head(joined_Footfall_Predictors)

	setDT(dayTime_HF_aggre_MINUS_outlier)
	setDT(predictors_info_extract_subset)

	joined_dataset_for_Training <- dayTime_HF_aggre_MINUS_outlier[predictors_info_extract_subset, on = c('Date','Date')]

df2[df1, on = c('id','dates')]
	merge_Dataset_to_Train <- [dayTime_HF_aggre_MINUS_outlier[, y = predictors_info_extract_subset, by = "Date", all.x = TRUE, all.y = TRUE)

#head(merge_Dataset_to_Train)
#head(dayTime_HF_aggre_MINUS_outlier)
#head(predictors_info_extract_subset)


	merge_Dataset_to_Train <- as.data.frame(merge_Dataset_to_Train)
	
	#predictive model
 	pred_model <- lm(y ~ ., data = merge_Dataset_to_Train)
	

	#export the model
	save(pred_model, file = "C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/misc/random_forest_model.rda")
	save(pred_model, file = paste(other_dir, "random_forest_model.rda")   

}

model(aggre_footfall, predictors)

	#apply linear regression

    shinyjs::show("restart_app3")
        
  })

#code to predict values of x for a certain date or range of dates

PREDICTING SECTION HAVING SET ALL PARAMETERS

from calender, select date range...

	#newdf <- data.frame(x = rnorm(20))
	## load the model
	load(file = "C:/Users/monsu/Documents/GitHub/lcc-footfall/webapp/misc/random_forest_model.rda")
	#load(file = paste(other_dir, "random_forest_model.rda"))
	#load(paste(other_dir, "random_forest_model.rda"))

	#load the predictors for the date specified, check if not available (current date ...) ...list the dates,, dates to predict(check against 
	#database and see if not avaialbe and return message to say, it is not available...)
	> ## predict for the new `x`s for the data specified
	
	merge_Dataset_to_Train                              bbbbbbbbbbb
	
	predict(m1, newdata = newdf)

	#after the prediction....store the prediction somewhere, to keep tract of the accuracy.


disable 25th of december.. 




#for every new day's prediction.... we have records kept.

> set.seed(345)
> df <- data.frame(x = rnorm(20))
> df <- transform(df, y = 5 + (2.3 * x) + rnorm(20))
> ## model
> m1 <- lm(y ~ x, data = df)
> ## save this model
> save(m1, file = "my_model1.rda")
> 
> ## a month later, new observations are available: 
> newdf <- data.frame(x = rnorm(20))
> ## load the model
> load("my_model1.rda")
> ## predict for the new `x`s in `newdf`
> predict(m1, newdata = newdf)





























getwd()

date_function <- function(){
  date_time <- Sys.time()
  dateT <- substr(as.character(date_time), 1, 10)
  timeT <- substr(as.character(date_time), 11, 20)
  dayT <- weekdays(as.Date(dateT))
  print(paste(dayT, ", ", dateT, ", ", sep=""))}





















