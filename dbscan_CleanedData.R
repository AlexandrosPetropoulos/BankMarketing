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
library(ggplot2)
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
library(cluster)#KMEANS,HIERARCHICAL
#library(factoextra)
#library(NbClust)
#library(pracma)
#library(mixtools)
library(dbscan)
#library(DataExplorer) #create_report(train)

# Import data from csv
bank_data_original=read.table(file ="bank-additional/bank-additional-full.csv",header=TRUE,sep=";")
bank <- bank_data_original

#bank <- bank[complete.cases(bank), ]

#Data preparation cleaning

#shuffle Data
#PROSOXI PREPEI OPOSDIPOTE NA EINAI FALSE, allios vgainoun xiliades duplicates
#search google for sampling with replacement
set.seed(1243)
bank <- bank[sample(1:nrow(bank),replace=FALSE), ]

#split in train and validation test stratified
#createDataPartition epistrefei mia lista me FALSE kai TRUE
inBank <- createDataPartition(y = bank$y, p = .70, list = FALSE)
#inBank <- createDataPartition(y = bank, p = .70, list = FALSE)
training <- bank[inBank,]
testing <- bank[-inBank,]

#prota kanoume scale ta data kai meta binarization
#prosxi training kai test prepei na ginoun me siggekrimeno tropo, opos edo
#temp_testing <- testing[,-ncol(testing)]
#temp_training <- training[,-ncol(training)]
preProcValues <- preProcess(training, method = c("center", "scale"))
#preProcValues <- preProcess(training, method = "range",rangeBounds = c(0, 1))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, testing)

trainTransformed <- training
testTransformed <- testing

# dummify the data
#kano dummy to y tote pernei diafores times metaksi 0 kai 1
#dmy <- dummyVars(" ~ .", data = trainTransformed)
dmy <- dummyVars(" ~ .", data = trainTransformed, fullRank=T)
trainset <- data.frame(predict(dmy, newdata = trainTransformed))
#epidi to target egine y.no kai y.yes kratame mono to ena apo ta 2
#trainset <-trainset[,-dim(trainset)[2]]
colnames(trainset)[dim(trainset)[2]] <- "y"

#dmy <- dummyVars(" ~ .", data = testTransformed)
dmy <- dummyVars(" ~ .", data = testTransformed, fullRank=T)
testset <- data.frame(predict(dmy, newdata = testTransformed))
#epidi to target egine y.no kai y.yes kratame mono to ena apo ta 2
#testset <-testset[,-dim(testset)[2]]
colnames(testset)[dim(testset)[2]] <- "y"



###DBSCAN


#model = dbscan(trainset[,-ncol(trainset)], eps = 2, minPts = 2)

#Optimizing parameter eps
knndist = kNNdist(trainset[,-ncol(trainset)], k = 10)

# Get the distances of the 10 nearest neighbours for each point
kdist = knndist[, 10]
# Plot distances
plot(sort(kdist), type = 'l', xlab = "Points sorted by distance",
     ylab = "10-NN distance")
  
abline(h=3, lty=2)

#apo help tis R
#minPts is often set to be dimensionality of the data plus one or higher
#model = dbscan(trainset[,-ncol(trainset)], eps = 0.3, minPts = 7)

#ara tora ksanatrexo gia veltisto eps
model = dbscan(trainset[,-ncol(trainset)], eps = 3, minPts = 54)


#head(model$cluster)

#https://stackoverflow.com/questions/19962204/what-command-returns-the-number-of-clusters-in-dbscan-as-a-value
#aritmos clusters
max(model$cluster)

#apla agnoo ta 0 pou einai noise

#vrisko poia einai i pliopsifousa timi se kathe cluster
#diladi vazo ta y pou ksero kai metrao poia einai perissotera 0 i 1 apo to y
#pio aplo xoris to tt
majorityClass <- NULL
for (i in 1:max(model$cluster)){
  if(dim(trainset[model$cluster==i & trainset$y==0,])[1]>dim(trainset[model$cluster==i & trainset$y==1,])[1]){
    majorityClass <- c(majorityClass,0)
  }
  else{
    majorityClass <- c(majorityClass,1)
  }
  
}
majorityClass

#anathesi sto kontinotero simeio
#prediction = knn(trainset[,-ncol(trainset)], testset[,-ncol(testset)], model$cluster, k = 1)

#prosoxi an petao ta 0 na prosekso sto majority class na metrao apo to 1 kai oxi apo to 0
mymod <- model$cluster[model$cluster>0]
x_train<-trainset[model$cluster>0,-ncol(trainset)]
prediction = knn(x_train, testset[,-ncol(testset)], mymod, k = 1)

prediction_int<-as.numeric(prediction)
for(i in 1:size(prediction_int)[2]){
  prediction_int[i] <- majorityClass[prediction_int[i]]
}

results <- confusionMatrix(factor(prediction_int,levels = c(0,1)), factor(testset[,ncol(testset)]),positive="1")
setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))

source("draw_confusion_matrix.R")
draw_confusion_matrix(results)

load("dist_hierarchical.Rdata")
mean(silhouette(model$cluster, d)[, 3])

model_silhouette = silhouette(model$cluster, d)
model_silhouette = silhouette(model$cluster, dist(trainset[,-ncol(trainset)]))
mean(model_silhouette[,3])
# 0.4135718

save(d, file = "model_silh_dbscan.Rdata")

for (i in 1:2){
  a=dim(trainset[model$cluster==i & trainset$y==1,])[1]
  b=dim(trainset[model$cluster==i & trainset$y==0,])[1]
  
  if(majorityClass[i] == 0){
    x= (b/(b+a))
    
  }
  else{
    x = (a/(b+a))
  }
  print(x)
  
}

