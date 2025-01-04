#Section3:
library(caret)
library(dplyr)
library(MLeval)
library(class)
library(ggplot2)

#prepare train set and test set for used:
#get data from csv file
data.larger <- read.csv('all_features.csv', sep='\t', header=FALSE)
#change the name of V1 to label 
names(data.larger)[1] <- 'label'

#two data set
trainIndex.larger <- createDataPartition(data.larger$label, p=0.8, times=1, list=FALSE)
#train set
train.larger <- data.larger[trainIndex.larger,]
#test set
test.larger  <- data.larger[-trainIndex.larger,]


#Section3.1:
#reference:https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
#reference: https://rpubs.com/phamdinhkhanh/389752
#In manually tunning we continualy keep caret because its result is aligned with previous model and provide a feasible comparision. Moreover, keeping repeated cross validation in caret can reduces model’s overfiting.
#This approach create many model caret scenarios with different manual parameters and compare its accuracy. Let look at example we do this to evaluate different ntree while hodling mtry constant.
#need a sufficient number of decision trees to form a random forest model
#Metric compare model is Accuracy
metric <- "Accuracy"
#5-cross-validation:
control <- trainControl(method="cv", number=5, search="grid")
#Number randomly variable selected is mtry
#this will automatically check all the possible tuning parameters and return the optimized parameters on which the model give the best accuracy.
tunegrid <- expand.grid(.mtry=c(2,4,6,8))
#rf_default <- train(Class~., 
#                    data=dataset, 
#                    method='rf', 
#                    metric='Accuracy', 
#                    tuneGrid=tunegrid, 
#                    trControl=control)

modellist <- list()
#random forest: train with different ntree parameters
for (ntree in seq(25,375,by=50)){
  rf <- train(label~., data=train.larger, method="rf", 
              metric=metric, tuneGrid=tunegrid, trControl=control) #cross-validation
  key <- toString(ntree)
  modellist[[key]] <- rf
}
#Compare results
results <- resamples(modellist)
dotplot(results)
modellist$`175`$results


 

#section 3.2
#store data for sample of 15 accuracies
accuracy.max <- c()
#refit the model 15 times
for (i in 1:15){
  accuracy.max.tree <- c()
  #base on model in section 3.1
  for (ntree in seq(25,375,by=50)){
    rf <- train(label~., data=train.larger, method="rf", 
                metric=metric, tuneGrid=tunegrid, trControl=control)
    #store the data for one times random forest
    accuracy.max.tree <- c(accuracy.max.tree, max(rf$results$Accuracy))
  }
  accuracy.max <- c(accuracy.max, max(accuracy.max.tree))
}
#using t.test function:
t.test(accuracy.max, mu=0.7769, alternative="greater")










