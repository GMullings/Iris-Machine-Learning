setwd("~/R/iris_ml")
.libPaths(c("C:/Users/geoff/Documents/R/win-library/3.6"))

# Ensure you have the latest versions of GGPlot2 and IPred installed, they're common hiccups

install.packages("caret")
library(caret)

# Easy Load:
# Attaching the iris dataset to the environment
data("iris")
# renaming the dataset
dataset <- iris

# CSV load
# Defining the filename
filename <- "iris.csv"

# Loading the downloaded CSV file (https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data)
# from the local directory

dataset <- read.csv(filename, header=FALSE)

# Setting the column names in the dataset

colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# Creating the training dataset of 80% of the rows in the original dataset.

validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)

# Selecting 20% of the data for validation.

validation <- dataset[-validation_index,]

# Turning "dataset" into the remaining 80% of data for model training and testing, to make things simple

dataset <- dataset[validation_index,]