
# DATAMINING AND mACHINE LEARNING
#
# END TERM ASSESMENT- 
# K-MEANS 
# A00279933- K R ROOPESH BHARATWAJ 

# STEP 1 IMPORTING THE DATASET

#install.packages("gmodels")
library(gmodels)
#install.packages("class")
library(class)
#importing dataset
#install.packages("foreign")
library("foreign")


dataset1 <-read.csv ("data/Abalone1.csv")
head(dataset1)
tail(dataset1)
summary(dataset1)
str(dataset1)

# STEP 2 :
# creating the K centers and Normalization

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalization
datasetkmeans = as.data.frame(lapply(dataset1[2:9], normalize))

set.seed(60)

# STEP 3 MODEL FOR K-MEANS 

model = kmeans(datasetkmeans, 27)
model
model$cluster
model$tot.withinss # 23.353
model$centers

table(dataset1$Rings, model$cluster)

# STEP - 4 PLOTIING THE MODEL

##Plotting the cluster graph using 2 elements from the data frame

plot(datasetkmeans[c("Length", "Rings")], col = model$cluster)

# plot cluster centers

points(model$centers[,c("Length", "Rings")], col = 1:3, pch = 8, cex=2, )


model$withinss



