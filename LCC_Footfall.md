Introduction
============

This script provides an R equivalence of the RandomForest algorithm in `LCC_Footfall.ipynb`, used in the predictive analysis of footfall datasets. The goal of this module is to compare the performance of the R version with the Python version, before its integration into a web-based tool designed for daily prediction of footfall data in Leeds City Centre.

This module skipped to the `Create Validation Data Set` section of the `LCC_Footfall.ipynb`. In other words, an already prepared (cleaned) dataset is used here.

``` r
#Importing cleaned (processed) datasets and preview
data <- read.table(file="//ds.leeds.ac.uk/staff/staff7/geomad/GitHub/lcc-footfall/Cleaned_Dataset/input_Dataset.csv", sep=",",head=TRUE)

#previewing first 5 rows and 5 columns of the dataset
data[1:5,1:5]
```

    ##   InCount school_holiday uni_holiday bank_hols easter_sunday
    ## 1  115685              1           1         0             0
    ## 2  160658              1           1         1             0
    ## 3  165334              1           1         0             0
    ## 4  135127              0           1         0             0
    ## 5  148253              0           1         0             0

``` r
print(paste("Data summary: no. of rows = ",nrow(data),"; no. of column = ",ncol(data), sep = ""))
```

    ## [1] "Data summary: no. of rows = 2122; no. of column = 83"

``` r
#subsetting the predictors
Xfull <- subset(data, select=-c(InCount)) 
xname <- colnames(Xfull)
#subsetting the dependent variable (footfall count)
Yfull <-data[,1]
yname <- c("y")

#merge as dataframe
XYfull <- as.data.frame(cbind(Xfull, Yfull))
colnames(XYfull) <- c(xname, yname)

#partitioning the dataset into training and testing sets
set.seed(123)
inTraining <- createDataPartition(XYfull$y, p = .66666, list = FALSE)
training_set <- XYfull[inTraining,] 
testing_set  <- XYfull[-inTraining,]

#Run a 10-fold cross validation using the "training_set" (66% partition)
#evaluate sme
```
