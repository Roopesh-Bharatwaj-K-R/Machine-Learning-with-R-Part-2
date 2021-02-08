
# DATA MINING AND MACHINE LEARNING,  END ASSESMENT
#
# KNN ALGORITHM
#
# BY- A00279933
# K R ROOPESH BHARATWAJ 


# STEP 1 - IMPORTING DATASET
# IMPORTING PACKAGES

#install.packages("gmodels")
library(gmodels)
#install.packages("class")
library(class)

# importing dataset

set.seed(200)
dataset1 <-read.csv ("data/Abalone1.csv")
dataset1$Rings = factor(dataset1$Rings)
head(dataset1)
tail(dataset1)
summary(dataset1)
str(dataset1)
sum(is.na(dataset1))


# step 1.1 shuffling the data 

shuffle_index <- sample(1:nrow(dataset1)) # shuffle
head(shuffle_index)
dataset1 <- dataset1[shuffle_index, ]
head(dataset1)

# STEP 1.2 Table of class
table(dataset1$Sex)
class(dataset1$Sex)

# STEP 1.3 Table of Proportions of the factor 
prop.table(table(dataset1$Sex))

# STEP 2: 
# Normalization function

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# STEP 2.1 Normalizing the data
data_n = as.data.frame(lapply(dataset1[2:8], normalize))

# Checkingk for data normalization

summary(data_n$Viscera.weight)
hist(data_n$Viscera.weight) # Data has been normalized


# STEP 2.2  Create the training and test data

dataset1_n = data_n[order(runif(2731)), ]
train = data_n[1:2185,] # train
test=data_n[2186:2731, ] # test
str(dataset1)
dim(train)
dim(test)

# STEP 2.3 labeling the dataset
train_labels = dataset1[1:2185,1]
test_labels = dataset1[2186:2731,1]

# STEP  3
# predict


# STEP 3.1 when k =5
predictions =  knn(train = train, test = test, cl = train_labels, k=5)
CrossTable(predictions, test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

cm=table(predictions,test_labels)
m=sum(diag(cm))/sum(cm)
m       #0.5311355
summary(m) # 

# STEP 3.2 when k =10
predictions1 =  knn(train = train, test = test, cl = train_labels, k=10)
CrossTable(predictions1, test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

cm=table(predictions1,test_labels)
m1=sum(diag(cm))/sum(cm)
m1     # 0.5586081  # best predicted model for KNN
summary(predictions1)

# STEP 3.3 k =20
predictions2 =  knn(train = train, test = test, cl = train_labels, k=50)
CrossTable(predictions2, test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

cm=table(predictions2,test_labels)
m2=sum(diag(cm))/sum(cm)
m2     #00.532967
summary(predictions2)






