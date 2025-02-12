# Data-analysis-of-hand-drawn-symbols

## Introduction

In this Assignment, data from Assignment 2 is used to solve classification problems by Machine learning. All models predicting the class label will be fit and evaluate classifier for Assignment 2 data.

Prepare the packages that would be used later.

```{r
library(caret)
library(dplyr)
library(MLeval)
library(class)
library(ggplot2)
library(ROCR)
```

Read the data into r and set the random seed to ensure the reproducibility of the program.

```{r}
data <- read.csv("features.csv")
set.seed(42)
```

# Section 1

## Section 1.1

Using a logistic regression model and decision threshold of 0.5 reports the confusion matrix, accuracy, true positive rate, false-positive rate, precision, recall and F1-score for the model on the test data.

Since this problem only requires the probability of belonging to the "letter" category, a new variable is constructed with the name letter, and if the letter is "Yes", then label is a letter, and if the letter is "No", then label is not a letter. Noteworthy, using "1" and "0" to distinguish the letter will have an error in section 1.2. Therefore, "Yes" and "No" are used to be its levels instead of "0" and "1".

```{r}
data$letter <- rep(1,140)
data$letter[which(data$label=='sad')] <- 0
data$letter[which(data$label=='smiley')] <- 0
data$letter[which(data$label=='xclaim')] <- 0
data$letter <- as.factor(data$letter)
levels(data$letter) <- c('No','Yes')
```

The samples were split into a train set and a test set, and 20% of the samples were randomly selected as the test set.

```{r}
index <- createDataPartition(data$letter, p = 0.8,list=FALSE)
train <- data[index,]
test <- data[-index,]
```

Using the "logistic regression model" to predict the probability of the "letter" category and report the test data.

First of all, I choose the glm() function to build a model by training data with nr_pix and aspect_ratio features. Then predict the data with test data and probably data, calculate TPR by "TPR = TP/(TP+FN)" function and FPR by "FPR = FP/(FP+TN)" function and use the confusionMatrix() function to calculate data such as accuracy, precision, recall and F1-score.

```{r
model <-glm(letter ~ nr_pix + aspect_ratio, 
           data = train, 
           family = binomial) 
summary(model)
model2 <- step(object = model,trace = 0)
summary(model2)
prob<-predict(object =model2,newdata=test,type = "response")
pred<-ifelse(prob>=0.5,"Yes","No")
pred<-factor(pred,levels = c("No","Yes"),order=TRUE)
f<-table(test$letter,pred)
f
tPosiRate = 15/(15+1)
fPosiRate = 1/12
print(tPosiRate)
print(fPosiRate)
confusionMatrix(data=pred, reference=test$letter, positive="Yes", mode="prec_recall")
```

However, after reading the question1.2 find there is a function in package "caret" called "train" which can build the logistic regression model and do the cross-validation.

```{r
model_1 <- train(letter ~ nr_pix + aspect_ratio, data=train, 
                  method="glm", family="binomial")
pred_1 <- predict(model_1, test)
tPosiRate = 15/(15+1)
fPosiRate = 1/12
```

```{r}
print(tPosiRate)
print(fPosiRate)
confusionMatrix(data=pred_1, reference=test$letter, positive="Yes", mode="prec_recall")
```

These two methods get the same result. The accuracy of the model on the test set is 0.9286, the true positive rate is 0.9375, the false positive rate is 0.0833, the precision is 0.9375, the recall is 0.9375 and the F1-score is 0.9375. The model has a good classification effect.

## Section 1.2

Using the cross-validation method to calculate the data by train() function and getTrainPerf() function from the "caret" package. Specially, getTrainPerf() function gives the mean performance results of the best tuned parameters averaged across the repeated cross validations folds.

```{r
train_control <- trainControl(method="cv", number=5, savePredictions=T, classProbs=T)

model_1_cv <- train(letter ~ nr_pix + aspect_ratio, data=data, 
                     method="glm", family="binomial",
                     trControl=train_control)
getTrainPerf(model_1_cv)
confusionMatrix(data=model_1_cv$pred$pred, reference=model_1_cv$pred$obs,
                positive="Yes", mode="prec_recall")
tPosiRate2 = 74/(74+6)
fPosiRate2 = 3/(57+3)
print(tPosiRate2)
print(fPosiRate2)
```

The model has an accuracy of  0.9429, a true positive rate of 0.9250, a false positive rate of 0.0333, a precision of 0.9737 recall of 0.9250 and an F1-score of  0.9487 on each test set in cross-validation. Precision increased and recall decreased. Of course, these two indicators can be controlled by adjusting the threshold value, and the overall classification effect of the model is still very good.

## Section 1.3

ROC mapping is painted for performance visualization. Using "Decision Trees" to predict data and evalm() function for assessing the performance of the model.

```{r
pred_1_cv <- predict(model_1_cv, type = "prob")
roc <- evalm(data.frame(pred_1_cv, data$letter), positive='Yes', plots='r')
roc$stdres$Group1
```

```{r
roc
roc <- evalm(data.frame(pred_1_cv, data$letter), positive='Yes', plots='r')
```

![1736032525305](image/README/1736032525305.png)

![1736032540359](image/README/1736032540359.png)

![1736032561168](image/README/1736032561168.png)

![1736032573366](image/README/1736032573366.png)

![1736032605085](image/README/1736032605085.png)





The AUC of the model can reach 0.99, which is a great model! When the false positive rate is 0.1, the true positive rate is almost close to 1. This classification is very effective.

Plotting the curve of the true probability versus the predicted probability, it can be seen that only around the probability of 0.5, which is not a straight line, there is a little deviation. After the probability is less than 0.25 or more than 0.75, it is a perfectly straight line. It indicates that the model only classifies some samples with relatively small differences slightly poorly and classifies samples with relatively large differences very well.

# Section 2

Choose items which are a, j, happy face, sad face and exclamation from 140 items performing 4-way classification. Therefore, the letter a and the letter j are named as letters been one category, and the other three categories are happy faces, sad faces and exclamation marks.

```{r}
data_k <- data[which((data$label=='a') | 
                     (data$label=='j') | 
                     (data$letter=='No')),]
data_k$label[which(data_k$label=='a')] <- 'letter'
data_k$label[which(data_k$label=='j')] <- 'letter'
```

## Section 2.1

Based on the analysis in assignment2, several features have significantly different distributions in the four categories of letters, happy faces, sad faces, and exclamation marks, and because there are significant correlations between some features, the four features 1, 3, 5, and 16 are selected.

```{r}
data_knn <- data_k[,c(1,3,5,7,18,19)] 
data_knn <- data_knn[,-6]
data_knn$label <- as.factor(data_knn$label)
```

Due to without using cross-validation and separate test set, using all items as training and test data. Using knn() function to.

```{r}
acc1 <- rep(0,7)
i <- 1
for (k in seq(1,13,by=2)){
  print(sprintf('K is %d',k))
  knn.pred <- knn(train=data_knn[,-1], test=data_knn[,-1],
              cl=data_knn[,1], k=k)
  acc <- sum(knn.pred==data_knn$label)/length(data_knn$label)
  acc1[i] <- acc
  i <- i+1
  print(sprintf('Accuracy is %.3f',acc))
  cat("\n")
}
```

When using all items as training and test data, a smaller k means a smaller number of neighbors are used, which will have higher accuracy.

## Section 2.2

Choosing knn.cv() function from "class" package and the default value of cross-validation is 5 replace knn() function inside for loop.

```{r}
acc2 <- rep(0,7)
i <- 1
for (k in seq(1,13,by=2)){
  print(sprintf('K is %d',k))
  knn.pred2 <- knn.cv(train=data_knn[,-1], cl=data_knn[,1], k=k)
  acc <- sum(knn.pred2==data_knn$label)/length(data_knn$label)
  acc2[i] <- acc
  i <- i+1
  print(sprintf('Accuracy is %.3f',acc))
  cat("\n")
}
```

In cross-validation, unlike the conclusion above, it is not the case that the smaller k is the higher the accuracy. Rather, the accuracy is highest when k = 7.

## Section 2.3

Using knn.cv() function to predict the data when k is equal to 7 which gets from section 2.2 and confusion matrix processing data.

```{r}
pred_7 <- knn.cv(train=data_knn[,-1], cl=data_knn[,1], k=7)
confusionMatrix(data=pred_7, reference=data_knn$label,
                positive="Yes", mode="prec_recall")
```

According to the results, data for letter and xclaim is centralized in Reference. But it is most difficult to distinguish between happy faces and sad faces. Their data are similar and not easy to differentiate.

## Section 2.4

All data is collected into one data frame.

```{r}
acc_k <- data.frame(Accuracy=c(acc1,acc2), K=c(seq(1,13,by=2),seq(1,13,by=2)),
                    Model=c(rep('Train set',7),rep('CV',7)))
acc_k$K <- 1/acc_k$K

ggplot(data = acc_k,aes(x=K,y=Accuracy,group=Model,color=Model,shape=Model))+
  geom_point()+
  geom_line()+
  xlab("1/K")+
  ylab("Accuracy")+
  theme_bw()
```



![1736032636825](image/README/1736032636825.png)



It is clear that when testing with training data, the accuracy is significantly higher than that of cross-validation. But using the training data for testing and tuning the parameters, k=1 will be chosen, which is incorrect because this will only perform well on the training data, without good generalization performance, meaning that the model will be overfitted. And in cross-validation, because the training data and test data are different, the model will have better generalization performance although the accuracy is reduced on the test data.

# Section 3

All data are got from the "all_features.csv" file. Train set and test set are prepared for the question.

```{r}
data.larger <- read.csv('all_features.csv', sep='\t', header=FALSE)
names(data.larger)[1] <- 'label'
trainIndex.larger <- createDataPartition(data.larger$label, p=0.8, times=1, list=FALSE)
train.larger <- data.larger[trainIndex.larger,]
test.larger  <- data.larger[-trainIndex.larger,]
```

## Section 3.1

Accuracy is a set metric for comparing models. Variable named "control" prepare for 5-fold cross-validation.
Using expand.grid()function to check all the possible tuning parameters and return the optimized parameters on which the model gives the best accuracy automatically. As shown before, the train() function can modelling and validated, parameter "method=rf" means modelling the Random Forest Model and its Tuning Parameter is mtry. Variable "modellist" is used to store data for the train with different ntree parameters.

```{r}
metric <- "Accuracy"
control <- trainControl(method="cv", number=5, search="grid")
tunegrid <- expand.grid(.mtry=c(2,4,6,8))
modellist <- list()
for (ntree in seq(25,375,by=50)){
  rf <- train(label~., data=train.larger, method="rf", 
                       metric=metric, tuneGrid=tunegrid, trControl=control)
  key <- toString(ntree)
  modellist[[key]] <- rf
}
```

All result are compare by using resamples() function.

```{r}
results <- resamples(modellist)
dotplot(results)
```

```{r}
modellist$`175`$results
```



![1736032683886](image/README/1736032683886.png)



The accuracy of the model is not high for both more and fewer numbers of decision trees, because when the number of decision trees is high, it causes overfitting; when the number of decision trees is low, it causes underfitting. Also, when the number of decision trees is determined, the number of predictor variables considered at each node varies, and the accuracy rate varies.

After several attempts, the model was most accurate when 175 decision trees were used, considering 8 numbers of predictor variables per node.

## Section 3.2

Variable "accuracy.max" stores data for samples of 15 accuracies. Using for loop refits the model 15 times and uses the model of section 3.1 for the one-time random forest.

```{r}
accuracy.max <- c()
for (i in 1:15){
  accuracy.max.tree <- c()
  for (ntree in seq(25,375,by=50)){
    rf <- train(label~., data=train.larger, method="rf", 
                       metric=metric, tuneGrid=tunegrid, trControl=control)
    accuracy.max.tree <- c(accuracy.max.tree, max(rf$results$Accuracy))
  }
  accuracy.max <- c(accuracy.max, max(accuracy.max.tree))
}
```

```{r}
t.test(accuracy.max, mu=0.7769, alternative="greater")
```

The p-value of the one-sided hypothesis test is less than 0.05, indicating that the original hypothesis can be rejected, and the alternative hypothesis is accepted at the 95% confidence level. That is, the mean value of the accuracy scores of the 15 cross-validations is greater than 0.7769. This indicates that the performance of the above-selected model is not significantly better than chance.

# Reference

- https://www.kaggle.com/code/tentotheminus9/quick-glm-using-caret/notebook
- https://stackoverflow.com/questions/41171045/cross-validate-predictions-for-caret-and-svm
- https://r-coder.com/set-seed-r/)
- https://topepo.github.io/caret/model-training-and-tuning.html
- https://cache.one/read/16840237
- https://towardsdatascience.com/k-nearest-neighbors-algorithm-with-examples-in-r-simply-explained-knn-1f2c88da405c
- https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
- https://rpubs.com/phamdinhkhanh/389752
- https://discuss.analyticsvidhya.com/t/how-to-choose-the-value-of-k-in-knn-algorithm/2606/3
- http://topepo.github.io/caret/available-models.html
- http://idata8.com/rpackage/caret/00Index.html
