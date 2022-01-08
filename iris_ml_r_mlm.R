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

# EDA

dim(dataset)
sapply(dataset, class)
head(dataset)

# Factor class has multiple labels

levels(dataset$Species) # Multinomial

# Summarizing class & attribute distribution

percentage <- prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species), percentage=percentage)
summary(dataset)

# Going to graph univariate plots to visualize variable distributions
# Splitting input and output variable columns for plotting

x <- dataset[,1:4]
y <- dataset[,5]

# Boxplot for each attribute on one image.

par(mfrow=c(1,4))
  for(i in 1:4) {
    boxplot(x[,i], main=names(iris)[i])
  }

# Barplot for "y" is going to reaffirm what was learned from species distribution. To see the plot de-hash the line below:
# plot(y)

# Multivariate plots ahoy! Scatter, Box and Whisker, and Density plots incoming.

featurePlot(x,y, plot="ellipse")
featurePlot(x,y, plot="box") # Each class value has different lengths but a lot of overlap in sepal width and length.
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x,y, plot="density", scales=scales)


