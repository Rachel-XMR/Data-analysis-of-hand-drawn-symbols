#section2: 將字母笑臉悲傷臉感歎號進行四分類
#4-way classification for letter, happy faces, sad faces and exclamation marks
library(caret)
library(dplyr)
library(MLeval)
library(class)
library(ggplot2)
#data:
data <- read.csv("features.csv")#read data from file
set.seed(42)
#choose a, j happy face, sad face and exclamation for performing 4-way classification
data_k <- data[which((data$label=="a") | 
                       (data$label=="j") | 
                       (data$letter=="No")),]
data_k$label[which(data_k$label=='a')] <- 'letter'
data_k$label[which(data_k$label=='j')] <- 'letter'

#section2.1:
#feature for k-nearest-neighbour classification
#not use cross-validation or a separate test set
#choose 4 features nr_pix clos_with_1 cols_with_3p custom

data_knn <- data_k[,c(1,3,5,7,18,19)] 
data_knn <- data_knn[,-6] # delete $letter
data_knn$label <- as.factor(data_knn$label)
levels(data_knn$label)

# knn: knn.pred=knn(train.X,test.X,train.Direction,k=k)
#no separate test set so use data same as training(all items)
#reference: https://towardsdatascience.com/k-nearest-neighbors-algorithm-with-examples-in-r-simply-explained-knn-1f2c88da405c
acc1 <- rep(0,7) #initialize "accuracy" data set 
i <- 1 #star from acc1[1] to store the result by for loop
for (k in seq(1,13,by=2)){ #odd values of k between 1 and 13 
  print(sprintf("K is %d",k))#ouput the k value
  knn.pred <- knn(train=data_knn[,-1], test=data_knn[,-1],
              cl=data_knn[,1], k=k)
  #tab <- table(knn.pred,test) not use cross-validation or a separate test set
  #accuracy:
  acc <- sum(knn.pred==data_knn$label)/length(data_knn$label)
  acc1[i] <- acc
  i <- i+1
  print(sprintf('Accuracy : %.3f',acc))#output the accuracy
  #will show like "K is (number) Accuracy is (number)"
  cat("\n")
}
#acc1
#show all accuracy


#section2.2:using 5-fold cross-validation for 2.1 with same function and same data
acc2 <- rep(0,7)
i <- 1
for(k in seq(1,13,by=2)){
  print(sprintf('K is %d',k))
  #cross-validation and knn can use knn.cv() function and the default value is 5
  #trainx=data_knn[,-1],trainy=data_knn[,1] 
  knn.pred2 <- knn.cv(train=data_knn[,-1], cl=data_knn[,1], k=k)
  acc <- sum(knn.pred2==data_knn$label)/length(data_knn$label)
  tab <- table(knn.pred2,data_knn[,1]) 
  acc2[i] <- acc
  i <- i+1
  print(sprintf('Accuracy is %.3f',acc))
  cat("\n")
}

#find best k value: https://discuss.analyticsvidhya.com/t/how-to-choose-the-value-of-k-in-knn-algorithm/2606/3

#However, the random nature of the partitioning of the training and 
#validation sets will result in the two packages of functions 
#partitioning the datasets differently and the final results 
#will be different

#ctrl <- trainControl(method="cv",number=5)  
#knn.pred2 <- train(label ~ ., data = data_knn, method = "knn", preProcess = c('center','scale'),
#                trControl = ctrl,tuneLength = 7)

#section2.3:
#predict:
pred_7 <- knn.cv(train=data_knn[,-1], cl=data_knn[,1], k=7)
#same as section 1:
confusionMatrix(data=pred_7, reference=data_knn$label,
                positive="Yes", mode="prec_recall")




#section 2.4:
#create a new data frame to store Accuracy K and it's data set 
acc_k <- data.frame(Accuracy=c(acc1,acc2), K=c(seq(1,13,by=2),seq(1,13,by=2)),
                    Model=c(rep('Train set',7),rep('CV',7)))

#horizontal coordinate is 1/k
acc_k$K <- 1/acc_k$K

#grouping by model(there are two model Train set and CV)
#color: Different sets come in different colours
#shape:Different graphs are used to represent different groups
ggplot(data = acc_k,aes(x=K,y=Accuracy,group=Model,color=Model,shape=Model))+
  geom_point()+#Coordinates
  geom_line()+ #Line Graph
  xlab("1/K")+ #Name of horizontal coordinate
  ylab("Accuracy")+ #Name of vertical coordinate
  theme_bw() #Remove background colour







































