Introduction
============

This script provides an R equivalence of the RandomForest algorithm in `LCC_Footfall.ipynb`, used in the predictive analysis of footfall datasets. The goal of this module is to compare the performance of the R version with the Python version, before its integration into a web-based tool designed for daily prediction of footfall data in Leeds City Centre.

This module skipped to the `Create Validation Data Set` section of the `LCC_Footfall.ipynb`. In other words, an already prepared (cleaned) dataset is used here.

``` r
#Importing cleaned (processed) datasets and preview
data <- read.table(file="//ds.leeds.ac.uk/staff/staff7/geomad/GitHub/lcc-footfall/Cleaned_Dataset/input_Dataset.csv", sep=",",head=TRUE)

#previewing the dataset
head(data)
```

    ##   InCount school_holiday uni_holiday bank_hols easter_sunday mean_temp
    ## 1  115685              1           1         0             0       0.0
    ## 2  160658              1           1         1             0       0.0
    ## 3  165334              1           1         0             0       5.7
    ## 4  135127              0           1         0             0       5.1
    ## 5  148253              0           1         0             0       4.5
    ## 6  113746              0           1         0             0       2.9
    ##   rain after_trinity_opened Monday Saturday Sunday Thursday Tuesday
    ## 1  0.0                    0      0        0      1        0       0
    ## 2  0.0                    0      1        0      0        0       0
    ## 3  0.0                    0      0        0      0        0       1
    ## 4  0.6                    0      0        0      0        0       0
    ## 5  0.1                    0      0        0      0        1       0
    ## 6  1.4                    0      0        0      0        0       0
    ##   Wednesday X.._2011... X.._2012... X.._2013... X.._2014... X.._2016...
    ## 1         0           1           0           0           0           0
    ## 2         0           1           0           0           0           0
    ## 3         0           1           0           0           0           0
    ## 4         1           1           0           0           0           0
    ## 5         0           1           0           0           0           0
    ## 6         0           1           0           0           0           0
    ##   February March April May June July August September October November
    ## 1        0     0     0   0    0    0      0         0       0        0
    ## 2        0     0     0   0    0    0      0         0       0        0
    ## 3        0     0     0   0    0    0      0         0       0        0
    ## 4        0     0     0   0    0    0      0         0       0        0
    ## 5        0     0     0   0    0    0      0         0       0        0
    ## 6        0     0     0   0    0    0      0         0       0        0
    ##   December X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15 X16 X17 X18
    ## 1        0  1  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0
    ## 2        0  1  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0
    ## 3        0  1  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0
    ## 4        0  1  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0
    ## 5        0  1  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0
    ## 6        0  1  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0   0
    ##   X19 X20 X21 X22 X23 X24 X25 X26 X27 X28 X29 X30 X31 X32 X34 X35 X36 X37
    ## 1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   X38 X39 X40 X41 X42 X43 X44 X45 X46 X47 X48 X49 X50 X51 X52
    ## 1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 5   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ## 6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   week_before_xmas week_after_xmas
    ## 1                0               0
    ## 2                0               0
    ## 3                0               0
    ## 4                0               0
    ## 5                0               0
    ## 6                0               0

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
