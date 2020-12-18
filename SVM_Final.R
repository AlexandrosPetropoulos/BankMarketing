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
library(e1071) #NAIVE BAYES
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
bank <- bank_data_original

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


library(ROSE)
#balance data
data.balanced.ou <- ovun.sample(y~., data=training,N=nrow(training), p=0.5, seed=1, method="both")$data
training<-data.balanced.ou

#prota kanoume scale ta data kai meta binarization
#prosxi training kai test prepei na ginoun me siggekrimeno tropo, opos edo

#preProcValues <- preProcess(training, method = c("center", "scale"))
preProcValues <- preProcess(training, method = "range",rangeBounds = c(0, 1))

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
#SVM
############################################################################################
# sto svm theloume na einai sto idio euros ara ta kanoume normalization sto 0-1

#SVM
#an kai sto knn douleue me y.no kai y.yes edo me mperdepse opote to evgala
#load("svm_variable.RData")
#svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainset, gamma = 0.02)
#save(svm_model, file = "svm_variable.RData")

#trainDataX <- trainData[,-ncol(trainData)]
#pred = predict(svm_model, testset[,-ncol(testset),drop=FALSE])

#results <- confusionMatrix(factor(pred), factor(testset[,ncol(testset)]),positive = "1")
#setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))

#source("draw_confusion_matrix.R")
#draw_confusion_matrix(results)

#ConfusionMatrix(factor(pred,levels = c(0,1)), factor(testset[,ncol(testset)]))
#as.table(results)
#as.matrix(results,what="overall")
#as.matrix(results, what = "classes")

#accuracy = Accuracy(factor(pred), factor(testset[,ncol(testset)]))
#recall <- Recall(factor(testset[,ncol(testset)]), factor(pred),positive="1")
#precision <- Precision(factor(testset[,ncol(testset)]), factor(pred),positive="1")
#f1 = F1_Score(factor(testset[,ncol(testset)]), factor(pred),positive="1")
#data.frame(accuracy,precision, recall, f1)

#KFOLD for finding optimum gamma
gammavalues = c(0.02, 0.1,1,1.5,2)

k = 10
dsize = nrow(train)
folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))

accuracies <- c()
f1s<-c()
for (gamma in gammavalues) {
  predictions <- data.frame()
  testsets <- data.frame()
  for(i in 1:k){
    # Select 9 out of 10 folds for training and 1 for validation
    trainingset <- trainset[unlist(folds[-i]),]
    validationset <- trainset[unlist(folds[i]),]
    # Train and apply the model
    svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingset, gamma = gamma)
    pred = predict(svm_model, validationset[, c(1:2)])
    # Save predictions and testsets
    predictions <- rbind(predictions, as.data.frame(pred))
    testsets <- rbind(testsets, as.data.frame(validationset[,3]))
  }
  # Calculate the new accuracy and add it to the previous ones
  accuracies = c(accuracies, Accuracy(predictions, testsets))
  f1s <- c(f1s, F1_Score(predictions, testsets,positive="1"))
}

svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainset, gamma = 0.02)
#save(svm_model, file = "svm_variable.RData")

#trainDataX <- trainData[,-ncol(trainData)]
pred = predict(svm_model, testset[,-ncol(testset),drop=FALSE])

results <- confusionMatrix(factor(pred), factor(testset[,ncol(testset)]),positive = "1")
setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))

source("draw_confusion_matrix.R")
draw_confusion_matrix(results)
