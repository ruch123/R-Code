
## Step 2: Exploring and preparing the data ---- 

# import the CSV file
Iris<-read.table(file.choose(),header=T,sep=",")
# examine the structure of the wbcd data frame
str(Iris)

# table of Class 
table(Iris$Class)

# recode Class as a factor
Iris$Class <- factor(Iris$Class, levels = c("S", "C","V"),
                         labels = c("Setosa", "Versicolor","Virginica")
# table or proportions with more informative labels                                          
round(prop.table(table(Iris$Class)) * 100, digits = 1)

# summarize the Iris data
summary(Iris)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}

# normalize the wbcd data
Iris_n <- as.data.frame(lapply(Iris[1:4], normalize))

# confirm that normalization worked
summary(Iris_n)

# create training and test data
Iris_train <- Iris_n[1:100, ]
Iris_test <- Iris_n[101:150, ]

# create labels for training and test data
Iris_train_labels <- Iris[1:100,5]
Iris_test_labels <- Iris[101:150,5]

## Step 3: Training a model on the data ----

# Install and load the "class" library
install.packages("class") 
library(class)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
Iris_test_pred<-knn(train = Iris_train, test = Iris_test,cl=Iris_train_labels,k=3)
install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = Iris_test_labels, y = Iris_test_pred,prop.chisq=FALSE)


## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
Iris_z <- as.data.frame(scale(Iris[-5]))

# confirm that the transformation was applied correctly
summary(Iris$Petal_Length)

# create training and test datasets
Iris_train <- Iris_n[1:100, ]
Iris_test <- Iris_n[101:150, ]


# re-classify test cases
Iris_test_pred<-knn(train = Iris_train, test = Iris_test,cl=Iris_train_labels,k=1)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = Iris_test_labels, y = Iris_test_pred,prop.chisq=FALSE)


# try several different values of k,ie repeat 70 to 74 with different k values
Iris_train <- Iris_n[1:100, ]
Iris_test <- Iris_n[101:150, ]

Iris_test_pred<-knn(train = Iris_train, test = Iris_test,cl=Iris_train_labels,k=5)
CrossTable(x = Iris_test_labels, y = Iris_test_pred,prop.chisq=FALSE)


Iris_train <- Iris_n[1:100, ]
Iris_test <- Iris_n[101:150, ]

Iris_test_pred<-knn(train = Iris_train, test = Iris_test,cl=Iris_train_labels,k=5)
CrossTable(x = Iris_test_labels, y = Iris_test_pred,prop.chisq=FALSE)


