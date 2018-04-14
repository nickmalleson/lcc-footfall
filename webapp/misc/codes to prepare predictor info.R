
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


dateLabels <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="day")

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















getwd()

date_function <- function(){
  date_time <- Sys.time()
  dateT <- substr(as.character(date_time), 1, 10)
  timeT <- substr(as.character(date_time), 11, 20)
  dayT <- weekdays(as.Date(dateT))
  print(paste(dayT, ", ", dateT, ", ", sep=""))}





















