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
#library(rpart) #DECISION TREES
#library(rpart.plot) #DECISION TREES
#library(mice)
#library(VIM)
#library(gdata)
#library(plyr)#prosoxi prota prepei na ginei load i plyr kai meta i dplyr
#library(dplyr)
#library(ggplot2)
#library(FSelector)
#library(e1071) #NAIVE BAYES
library(caret)
#library(pander)
#library(class)
library(MLmetrics)# xreiazetai gia to confusionMatrix
#library(mlbench)
#library(klaR)
library(class) #KNN
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
#bank_data_original=read.table(file ="bank-additional/bank-additional.csv",header=TRUE,sep=";")
bank <- bank_data_original

############################################################################################
#Data preparation cleaning
############################################################################################

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


#prota kanoume scale ta data kai meta binarization
#prosxi training kai test prepei na ginoun me siggekrimeno tropo, opos edo
#temp_testing <- testing[,-ncol(testing)]
#temp_training <- training[,-ncol(training)]

#me center-scale exo kalitera apotelesmata sto knn
preProcValues <- preProcess(training, method = c("center", "scale"))
#preProcValues <- preProcess(training, method = "range",rangeBounds = c(0, 1))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)


# dummify the data

#kano dummy to y tote pernei diafores times metaksi 0 kai 1
#dmy <- dummyVars(" ~ .", data = trainTransformed)
dmy <- dummyVars(" ~ .", data = trainTransformed, fullRank=T)
trainset <- data.frame(predict(dmy, newdata = trainTransformed))
#epidi to target egine y.no kai y.yes kratame mono to ena apo ta 2
#to kato energopoieitai otan DEN exo fullrank,ousiastika petame to y.no
#trainset$y.no <- NULL
colnames(trainset)[dim(trainset)[2]] <- "y"

#dmy <- dummyVars(" ~ .", data = testTransformed)
dmy <- dummyVars(" ~ .", data = testTransformed, fullRank=T)
testset <- data.frame(predict(dmy, newdata = testTransformed))
#epidi to target egine y.no kai y.yes kratame mono to ena apo ta 2
#to kato energopoieitai otan DEN exo fullrank,ousiastika petame to y.no
#testset <-testset[,-dim(testset)[2]]
colnames(testset)[dim(testset)[2]] <- "y"


############################################################################################
#KNN
############################################################################################
# theloume ta dedomena na einai standarized kai oxi sto 0-1
#https://www.r-bloggers.com/k-nearest-neighbor-step-by-step-tutorial/
#to exei kai sto vivlio tous pou zitaei sti sel 51 preProc = c("center", "scale"))

#ta kato gia na dokimaso me to arxiko set
#trainset<-trainset[complete.cases(trainset),]
#testset<-testset[complete.cases(testset),]
#pred = knn(trainset[,-ncol(trainset)], testset[,-ncol(testset)], trainset[,ncol(trainset)], k = 5, prob = TRUE)


#allakse kai pleon sto positive vazo ena akomi kai gia to center scale, afou pleon petao tin y.no
#results <- confusionMatrix(factor(pred), factor(testset[,ncol(testset)]),positive = "1")
#setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))

#ConfusionMatrix(factor(pred,levels = c(0,1)), factor(testset[,ncol(testset)]))
#as.table(results)
#as.matrix(results,what="overall")
#as.matrix(results, what = "classes")

#accuracy = Accuracy(factor(pred), factor(testset[,ncol(testset)]))
#recall <- Recall(factor(testset[,ncol(testset)]), factor(pred),positive="1")
#precision <- Precision(factor(testset[,ncol(testset)]), factor(pred),positive="1")
#f1 = F1_Score(factor(testset[,ncol(testset)]), factor(pred),positive="1")
#data.frame(accuracy,precision, recall, f1)


#crossvalidation for finding optimal k
#stratified folds
set.seed(107)
n=5
folds <- createFolds(trainset$y, k = n)

k=n #gia to loop
accuracies = c()
precisions <- c()
kvalues = c(1,2,5,10,20,30)

for (kval in kvalues) {
  predictions <- data.frame()
  testsets <- data.frame()
  for(i in 1:k){
    # Select k-1 out of k folds for training and 1 for validation
    cvtrainingset <- trainset[unlist(folds[-i]),]
    cvvalidationset <- trainset[unlist(folds[i]),]
    
    # Train and apply the model
    set.seed(2548)
    pred = knn(cvtrainingset[,-ncol(cvtrainingset)], cvvalidationset[,-ncol(cvvalidationset)], cvtrainingset[,ncol(cvtrainingset)], k = kval, prob = TRUE)
    
    #model <- naiveBayes(y ~ ., data = cvtrainingset, laplace = lapl)
    #pred = predict(model, newdata = cvvalidationset[,-ncol(cvvalidationset),drop = FALSE])
    #model <- rpart(y ~ ., method = "class", data = cvtrainingset, minsplit = minsp)
    #pred = predict(model, newdata = cvvalidationset[,-ncol(cvvalidationset)], type="class")                                          
    
    # Save predictions and testsets(to kanoume me rbind giati to exoume spasei se folds kai sto telos theloume na vroume ena metric gia olo to trainset)
    predictions <- rbind(predictions, as.data.frame(pred))
    testsets <- rbind(testsets, as.data.frame(cvvalidationset[,ncol(cvvalidationset),drop=FALSE]))
    
  }
  # Calculate the new accuracy and precision and add it to the previous ones
  results <- confusionMatrix(factor(predictions$pred), factor(testsets$y),positive = "1")
  accuracies <-c(accuracies,results$overall["Accuracy"])
  precisions = c(precisions,results$byClass["Precision"])
  #epeidi den douleue arxika i confusionMatrix eprepe na dokimaso to table sketo 
  #table(predictions$pred,testsets$y)
  #gia knn
  res<-confusionMatrix(table(predictions$pred,testsets$y),positive="1")
}

print(precisions)
bestk = kvalues[which.max(precisions)]
print(bestk)
print(accuracies)
bestk = kvalues[which.max(precisions)]
print(bestk)

set.seed(2548)
pred = knn(trainset[,-ncol(trainset)], testset[,-ncol(testset)], trainset[,ncol(trainset)], k = 3, prob = TRUE)


#allakse kai pleon sto positive vazo ena akomi kai gia to center scale, afou pleon petao tin y.no
results <- confusionMatrix(factor(pred), factor(testset[,ncol(testset)]),positive = "1")
setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))

source("draw_confusion_matrix.R")
draw_confusion_matrix(results)


ctrl <- trainControl(method = "repeatedcv", repeats = 3)
kNNFit2 <- train(y ~ .,
                 data = trainset,
                 method = "knn",
                 tuneLength = 15,
                 trControl = ctrl)
print(kNNFit2)
plot(kNNFit2)

