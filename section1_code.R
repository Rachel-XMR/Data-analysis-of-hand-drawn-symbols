#section1: Evaluating the performance of logistic regression models
#reference: https://www.kaggle.com/code/tentotheminus9/quick-glm-using-caret/notebook
data <- read.csv("features.csv")#read data from file
#known by assignment:
set.seed(42)#set random seed to ensure the reproducibility of the program
#simulation methods in Statistics require the possibility to generate pseudorandom numbers (reference:https://r-coder.com/set-seed-r/)

#prepare the packages:
install.packages("caret")
install.packages("dplyr")
install.packages("MLeval")
install.packages("class")
install.packages("ggplot2")
install.packages("ROCR")
library(caret)
library(dplyr)
library(MLeval)
library(class)
library(ggplot2)
library(ROCR)

#section1.1: about logistic regression model
#由於只識別“字母”類別的概率 那麼就有兩個類別一個是字母一個是非字母簡單操作就是把他們通過label直接標記數據結果 通過數據結果來區分數據
#only identify letter  use letter to set the data become differ 
#set dataset add a coloumn "letter" to identify it is letter or not.
data$letter <- rep(1,140)
data$letter[which(data$label=='sad')] <- 0
data$letter[which(data$label=='smiley')] <- 0
data$letter[which(data$label=='xclaim')] <- 0
#data$letter <- factor(data$letter, levels = c(0,1))
data$letter <- as.factor(data$letter)
levels(data$letter) <- c('No','Yes')
#two data set:
letterIndex <- createDataPartition(data$letter, p = 0.8,list=FALSE)
train <- data[letterIndex,]
test <- data[-letterIndex,]

#glm():
#build model:
model <-glm(letter ~ nr_pix + aspect_ratio, 
           data = train, 
           family = binomial) 
summary(model)
model2 <- step(object = model,trace = 0)
summary(model2)
#Significance test of the model
anova(object = model2, test = "Chisq")
#predict:
prob<-predict(object =model2,newdata=test,type = "response")
pred<-ifelse(prob>=0.5,"Yes","No")
pred<-factor(pred,levels = c("Yes","No"),order=TRUE)
f<-table(test$letter,pred)
f
#TPR = TP/(TP+FN)
tPosiRate = 15/(15+1)
#FPR = FP/(FP+TN)
fPosiRate = 1/12
print(tPosiRate)
print(fPosiRate)
confusionMatrix(data=pred, reference=test$letter, positive="Yes", mode="prec_recall")

#The "caret" package has several functions that attempt to streamline the process of building models and evaluating them.
#train function is in "caret" package that can generates parameters that further control how models are created
#train
model_1 <- train(letter ~ nr_pix + aspect_ratio, data=train, 
                  method="glm", family="binomial")
pred_1 <- predict(model_1, test)
confusionMatrix(data=pred_1, reference=test$letter, positive="Yes", mode="prec_recall")
#this two function can get same result but the result from train

#1.2:
#first I find cv.glm function which in boot package can use but question metion trainControl() function
#crossvalidation
#cv:for repeated training/test splits 
#saveP:an indicator of how much of the hold-out predictions for each resample should be saved.
train_control <- trainControl(method="cv", number=5, savePredictions=T, classProbs=T)

#logistic regression with crossvalidation
model_1_cv <- train(letter ~ nr_pix + aspect_ratio, data=data, 
                     method="glm", family="binomial",
                     trControl=train_control)
getTrainPerf(model_1_cv)#gives the mean performance results of the best tuned parameters averaged across the repeated cross validations folds.
confusionMatrix(data=model_1_cv$pred$pred, reference=model_1_cv$pred$obs,
                positive="Yes", mode="prec_recall")
tPosiRate2 = 74/(74+6)
fPosiRate2 = 2/(58+2)
print(tPosiRate2)
print(fPosiRate2)

#1.3：
#reference:https://cache.one/read/16840237
#Decision Tree
pred_1_cv <- predict(model_1_cv, type = "prob")
roc <- evalm(data.frame(pred_1_cv, data$letter), positive='Yes', plots='r') #get Images
roc
roc$stdres$Group1




























