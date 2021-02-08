
# DATA MINING AND MACHINE LEARNING,  END ASSESMENT
#
# DECISION TREES
#
# BY- A00279933
# K R ROOPESH BHARATWAJ 


# STEP 1 - IMPORTING DATASET
# IMPORTING PACKAGES

#install.package("caret")
#install.packages("C50")
#install.packages("gmodels")
library(C50)
library(gmodels)
library(caret)

# STEP 1.1 importing dataset

set.seed(200)
dataset1 <-read.csv ("data/CAR.csv", header= TRUE, sep=",", stringsAsFactors = TRUE)
dataset1
head(dataset1)
tail(dataset1)
summary(dataset1)

# STEP 1.2 shuffle the data 

shuffle_index <- sample(1:nrow(dataset1)) # shuffle
head(shuffle_index)
dataset1 <- dataset1[shuffle_index, ]
head(dataset1)

# STEP 2 
# splitting the dataset #training and testing (from total 299 records, 239 for training and 60 for testing)

dataset1=dataset1[order(runif(1728)), ]
train=dataset1[1:1382, ] # train
test=dataset1[1383:1728, ] # test
str(dataset1)
dim(train)
dim(test)

# STEP 2.1 prop.Tabel
prop.table(table(train$class))
prop.table(table(test$class))
help(table)


# STEP 3 building model

model=C5.0(class ~.,data=train)
summary(model)
plot(model)

#STEP 3.1 Boosting  model with trials =10

model1 <- C5.0(class ~ ., data=train,
                 trials = 10)
summary(model1)
plot(model1)

# STEP 4  prediction

# STEP 4.1 prediction for Model 1

predictions = predict(model, test)

CrossTable(predictions,test$class,prop.chisq = FALSE,
           prop.c=FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))
cm=table(predictions)
sum(diag(cm))/sum(cm) #  1
confusionMatrix(predictions, test$class)

# STEP 4.2 prediction for model 2:

predictions1 = predict(model1, test)
CrossTable(predictions1,test$class,prop.chisq = FALSE,
           prop.c=FALSE,prop.r = FALSE,
           dnn=c('predicted','actual'))
cm=table(predictions1)
sum(diag(cm))/sum(cm) #1
confusionMatrix(predictions1, test$class)


