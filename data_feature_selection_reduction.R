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
library(mice)
library(VIM)
library(gdata)
library(plyr)#prosoxi prota prepei na ginei load i plyr kai meta i dplyr
library(dplyr)
library(ggplot2)
library(FSelector)
library(e1071)
library(caret)
library(pander)
library(class)
library(MLmetrics)
library(mlbench)
library(klaR)
library(ROCR)
library(cluster)
library(factoextra)
library(NbClust)
library(pracma)
library(mixtools)
library(dbscan)

# Import data from csv
bank_data_original=read.table(file ="bank-additional/bank-additional-full.csv",header=TRUE,sep=";")
bank <- bank_data_original

############################################################################################

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

#prota kanoume scale ta data kai meta binarization
#prosxi training kai test prepei na ginoun me siggekrimeno tropo, opos edo
#temp_testing <- testing[,-ncol(testing)]
#temp_training <- training[,-ncol(training)]
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
#trainset <-trainset[,-dim(trainset)[2]]
colnames(trainset)[dim(trainset)[2]] <- "y"

#dmy <- dummyVars(" ~ .", data = testTransformed)
dmy <- dummyVars(" ~ .", data = testTransformed, fullRank=T)
testset <- data.frame(predict(dmy, newdata = testTransformed))
#epidi to target egine y.no kai y.yes kratame mono to ena apo ta 2
#testset <-testset[,-dim(testset)[2]]
colnames(testset)[dim(testset)[2]] <- "y"

############################################################################################


############################################################################################
#FEATURE SELECTION

#feature selection with filter method CFS
subset <- cfs(y ~ ., training)
f <- as.simple.formula(subset, "y")
print(f)
#y ~ poutcome + emp.var.rate

#feature selection with wrapper method RFE
# define the control using a random forest selection function
control <- rfeControl(functions=nbFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(training[,-ncol(training)], training[,ncol(training)], sizes=c(1:dim(training)[2]), rfeControl=control)
print(results)
#nr.employed

#forward search method
evaluator <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(training))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- training[test.idx, , drop=FALSE]
    train <- training[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "y"), train)
    error.rate = sum(test$y != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

subset <- forward.search(names(training)[-17], evaluator)

f <- as.simple.formula(subset, "y")
print(f)

#y ~ poutcome + emp.var.rate

############################################################################################

############################################################################################
#PCA DIMENSINALITY REDUCTION
trainsetX <- trainset
train.pca <- prcomp(trainsetX)
summary(train.pca)


############################################################################################


