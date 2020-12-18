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
library(rpart)
library(rpart.plot)
#library(mice)
#library(VIM)
#library(gdata)
#library(plyr)#prosoxi prota prepei na ginei load i plyr kai meta i dplyr
#library(dplyr)
#library(ggplot2)
#library(FSelector)
#library(e1071)
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
set.seed(107)
inBank <- createDataPartition(y = bank$y, p = .70, list = FALSE)
training <- bank[inBank,]
testing <- bank[-inBank,]


############################################################################################
#DECISION TREES
############################################################################################
#den xreiazetai kapoio standarization / i kapoio dummy

#decision trees
trainset <- training
testset <- testing

#model <- rpart(y ~ ., method = "class", data = trainset, minsplit = 1)
#rpart.plot(model, extra = 104, nn = TRUE)

#pred = predict(model, newdata = testset[,-ncol(testset)], type="class")

#results <-confusionMatrix(pred, testset[,ncol(testset)],positive = "yes")
#setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))


#crossvalidation for finding optimal minsplit
#stratified folds
n=5
folds <- createFolds(trainset$y, k = n)

k=n
accuracies = c()
precisions <- c()
minsplitvalues = c(1,5,10,100,1000)

for (minsp in minsplitvalues) {
  predictions <- data.frame()
  testsets <- data.frame()
  for(i in 1:k){
    # Select k-1 out of k folds for training and 1 for validation
    cvtrainingset <- trainset[unlist(folds[-i]),]
    cvvalidationset <- trainset[unlist(folds[i]),]
    
    # Train and apply the model
    model <- rpart(y ~ ., method = "class", data = cvtrainingset, minsplit = minsp)
    pred = predict(model, newdata = cvvalidationset[,-ncol(cvvalidationset)], type="class")                                          
    
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
bestminsplit = minsplitvalues[which.max(precisions)]
print(bestminsplit)
print(accuracies)
bestminsplit = minsplitvalues[which.max(precisions)]
print(bestminsplit)


#efarmogi tou kaliterou minsplit pou vrikame sto testset

model <- rpart(y ~ ., method = "class", data = trainset, minsplit = bestminsplit)
rpart.plot(model, extra = 104, nn = TRUE)

pred = predict(model, newdata = testset[,-ncol(testset)], type="class")

results <-confusionMatrix(pred, testset[,ncol(testset)],positive = "yes")
setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))

source("draw_confusion_matrix.R")
draw_confusion_matrix(results)
