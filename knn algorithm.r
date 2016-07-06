##### Chapter 3: Classification using Nearest Neighbors --------------------
rm(list=ls())
## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ---- 

# import the CSV file
wbcd <- read.csv(file.choose(),header=T, stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$Class)



# recode diagnosis as a factor
wbcd$Class <- factor(wbcd$Class , levels = c("car", "fad","mas","gla","con","adi"),
labels = c("A", "B","C","D","E","F"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$class)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c(IO, PA500, HFS)] )
summary(wbcd$HFS)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
install.packages("caret")
library(caret)

preObj <- preProcess(wbcd[, -1], method=c("center", "scale"))
wbcd_n <- predict(preObj, wbcd[, -1])
wbcd_n <- as.data.frame(lapply(wbcd[2:106], normalize))

# confirm that normalization worked
summary(wbcd_n$IO)

# create training and test data
wbcd_train <- wbcd[1:80,]
wbcd_test <- wbcd[81:106, ]
dim(wbcd_train)
dim(wbcd_test)
head(wbcd_train)
# create labels for training and test data

wbcd_train_labels <- wbcd[1:80, 1]
wbcd_test_labels <- wbcd[81:106, 1]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)
is.Error <- is.na(wbcd_train_labels) || is.nan(wbcd_train_labels) || is.infinity(wbcd_train_labels)
is.Error <- is.na(wbcd_train) || is.nan(wbcd_train)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels ,k=3)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labe