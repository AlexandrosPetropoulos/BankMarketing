# Clear plots
if(!is.null(dev.list())) dev.off()

# Clean workspace
rm(list=ls())

# Clear console
cat("\014")

# Set working directory
setwd("~/Big_Data")

# Load libraries (mporei na ksemine kapoia apo tis dokimes kai tora na min xreiazetai)
# entoli gia install packages install.packages("caret")
#library(rpart)
#library(rpart.plot)
#library(mice)
#library(VIM)
#library(gdata)
#library(plyr)#prosoxi prota prepei na ginei load i plyr kai meta i dplyr
#library(dplyr)
#library(ggplot2)
#library(FSelector)
library(e1071) #NAIVE BAYES
library(caret)
#library(pander)
#library(class)
library(MLmetrics)# xreiazetai gia to confusionMatrix
#library(mlbench)
#library(klaR)
#library(class)
#library(ROCR)
#library(cluster)
#library(factoextra)
#library(NbClust)
#library(pracma)
#library(mixtools)
#library(dbscan)
#library(DataExplorer) #create_report(train)


# Import data from csv
bank_data_original=read.table(file ="bank-additional/bank-additional-full.csv",header=TRUE,sep=";")
bank <- bank_data_original


#Data preparation cleaning

#svinoume ta xaraktiristika pou den xreiazonte
bank$duration <- NULL
bank$default <- NULL
bank$pdays <- NULL
#efoson svisame stiles, ksanasvinoume ta duplicates pou proekipsan
bank <- unique(bank)


#shuffle Data
#PROSOXI PREPEI OPOSDIPOTE NA EINAI FALSE, allios vgainoun xiliades duplicates
#search google for sampling with replacement
set.seed(248)
bank <- bank[sample(1:nrow(bank),replace=FALSE), ]

#split in train and validation test stratified
#createDataPartition epistrefei mia lista me FALSE kai TRUE
inBank <- createDataPartition(y = bank$y, p = .70, list = FALSE)
training <- bank[inBank,]
testing <- bank[-inBank,]

#library(ROSE)
#balance data
#data.balanced.ou <- ovun.sample(y~., data=training,N=nrow(training), p=0.5, seed=1, method="both")$data
#training<-data.balanced.ou

############################################################################################
#NAIVE BAYES
############################################################################################
#den xreiazetai standarization/normalization. Vgenoun kalitera apotelesmata xoris auta

#kalitera xoris na ta kanoume binarization
trainset <- training
testset <- testing

#model <- naiveBayes(y ~ ., data = trainset, laplace = 1)

#to FALSE se periptosi pou p.x. meso cfs exo dialeksei mono mia stili
#xtest = testset[,-ncol(testset),drop=FALSE]
#ytest = testset[,ncol(testset),drop=FALSE]

#pred = predict(model, testset[,-ncol(testset),drop=FALSE])

#predprob = predict(model, xtest, type = "raw")


#results <- confusionMatrix(pred, testset[,ncol(testset)],positive = "yes")
#setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))



#crossvalidation for finding optimal laplace
#stratified folds
n=5
folds <- createFolds(trainset$y, k = n)

k=n
accuracies = c()
precisions <- c()
laplacevalues = c(0.3,0.5,1,4)

for (lapl in laplacevalues) {
  predictions <- data.frame()
  testsets <- data.frame()
  for(i in 1:k){
    # Select k-1 out of k folds for training and 1 for validation
    cvtrainingset <- trainset[unlist(folds[-i]),]
    cvvalidationset <- trainset[unlist(folds[i]),]
    
    # Train and apply the model
    model <- naiveBayes(y ~ ., data = cvtrainingset, laplace = lapl)
    pred = predict(model, newdata = cvvalidationset[,-ncol(cvvalidationset),drop = FALSE])
    
    # Save predictions and testsets(to kanoume me rbind giati to exoume spasei se folds kai sto telos theloume na vroume ena metric gia olo to trainset)
    predictions <- rbind(predictions, as.data.frame(pred))
    testsets <- rbind(testsets, as.data.frame(cvvalidationset[,ncol(cvvalidationset),drop=FALSE]))
    
  }
  # Calculate the new accuracy and precision and add it to the previous ones
  results <- confusionMatrix(predictions$pred, testsets$y,positive = "yes")
  accuracies <-c(accuracies,results$overall["Accuracy"])
  precisions = c(precisions,results$byClass["Precision"])
  #epeidi den douleue arxika i confusionMatrix eprepe na dokimaso to table sketo 
  #table(predictions$pred,testsets$y)
}

print(precisions)
bestlaplace = laplacevalues[which.max(precisions)]
print(bestlaplace)
print(accuracies)
bestlaplace = laplacevalues[which.max(precisions)]
print(bestlaplace)


#efarmogi sto testset gia to veltisto laplace
model <- naiveBayes(y ~ ., data = trainset, laplace = bestlaplace)

pred = predict(model, testset[,-ncol(testset),drop=FALSE])


results <- confusionMatrix(pred, testset[,ncol(testset)],positive = "yes")
setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))

source("draw_confusion_matrix.R")
draw_confusion_matrix(results)
