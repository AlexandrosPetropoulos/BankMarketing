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
#library(dbscan)
#library(DataExplorer) #create_report(train)

#hkmeans

# Import data from csv
bank_data_original=read.table(file ="bank-additional/bank-additional-full.csv",header=TRUE,sep=";")
bank <- bank_data_original

#Data preparation cleaning

#shuffle Data
#PROSOXI PREPEI OPOSDIPOTE NA EINAI FALSE, allios vgainoun xiliades duplicates
#search google for sampling with replacement
set.seed(248)
bank <- bank[sample(1:nrow(bank),replace=FALSE), ]

#split in train and validation test stratified
#createDataPartition epistrefei mia lista me FALSE kai TRUE
inBank <- createDataPartition(y = bank$y, p = .70, list = FALSE)
#inBank <- createDataPartition(y = bank, p = .70, list = FALSE)
training <- bank[inBank,]
testing <- bank[-inBank,]

#library(ROSE)
#balance data
#data.balanced.ou <- ovun.sample(y~., data=training,N=nrow(training), p=0.5, seed=1, method="both")$data
#training<-data.balanced.ou

#prota kanoume scale ta data kai meta binarization
#prosxi training kai test prepei na ginoun me siggekrimeno tropo, opos edo
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


#trainset<-trainset[,c(1:40,54)]
#testset<-testset[,c(1:40,54)]

###################UNSUPERVISED
#
#Hierarchical
#load("dist_un")
load("dist_unscaled.Rdata")
#d = dist(trainset[,-ncol(trainset)])

#dim(trainset) 27364 54 range 0-1
#save(d, file = "dist_hierarchical_70_30.Rdata")

#poia methodo
#p.x. spherical clusters me wards
hc_single = hclust(d, method = "single")
#plot(hc_single)
clusters = cutree(hc_single, k = 8)

hc_complete = hclust(d, method = "complete")
#plot(hc_complete)
clusters = cutree(hc_complete, k = 8)

hc_ave = hclust(d, method = "ave")
#plot(hc_complete)
clusters = cutree(hc_ave, k = 3)

# Silhouette vgenei veltisto 2
slc = c()
for (i in 2:10){
  # Create clusters
  clusters = cutree(hc_ave, k = i)
  # Calculate and store silhouette values
  slc [i-1] = mean(silhouette(clusters, d)[, 3])
}


plot(2:10, slc, type="b", xlab="Number of Clusters",
     ylab="Silhouette")

abline(v=3, lty=2)

#Elbow gia hierarchical
#enonoume ta dedomena train kai test
#?
#?
#?
nCluster = 3
#clusters = cutree(hc_single, k = nCluster)
#head(clusters)


majorityClass <- NULL
for (i in 1:nCluster){
  #trainset[clusters == i &,,drop=FALSE]
  if(dim(trainset[clusters==i & trainset$y==0,])[1]>dim(trainset[clusters==i & trainset$y==1,])[1]){
    majorityClass <- c(majorityClass,0)
  }
  else{
    majorityClass <- c(majorityClass,1)
  }
  
}

majorityClass



#anatheto san neo y ta clusters sta opoia anikei i kathe grammi
#temptrainset <-trainset
#temptrainset$y<-NULL
#temptrainset$yy <- clusters
#apla edo anti gia to y tou train vazoume ta clusters pou dimiourgithikan
#prediction = knn(trainset[,-ncol(trainset)], testset[,-ncol(testset)], temptrainset$yy, k = 1)
#head(prediction)
#arkei kai to parakato , mporo na sviso ta temp(to clusters isos thelei na ginei dataframe , alla kai etsi douleui)
prediction = knn(trainset[,-ncol(trainset)], testset[,-ncol(testset)], clusters, k = 1)

#gia error sto train set
#prediction = knn(trainset[,-ncol(trainset)], trainset[,-ncol(trainset)], clusters, k = 1)

########
#den xreiazetai
#for (i in 1:size(clusters)[2]){
#  clusters[[i]] <- majorityClass[clusters[[i]]]
#  
#}
######
#peritto i metatropi , alla auto skeftika pio grigora
#ggplot2 gia ti size
prediction_int<-as.numeric(prediction)
for(i in 1:length(prediction_int)){
  prediction_int[i] <- majorityClass[prediction_int[i]]
}

results <- confusionMatrix(factor(prediction_int,levels = c(0,1)), factor(testset[,ncol(testset)]),positive="1")
setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))

source("draw_confusion_matrix.R")
draw_confusion_matrix(results)

#error gia trainset

results <- confusionMatrix(factor(prediction_int,levels = c(0,1)), factor(trainset[,ncol(trainset)]),positive="1")
setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))


############################################################################################
#den xrisimopoiithike telika , alla einai kali sinartisi , tin afino gia na tin exo
#gia na anatheso nea stoixeia ipologizo tin apostasi
#https://stackoverflow.com/questions/39005958/r-how-to-get-row-column-subscripts-of-matched-elements-from-a-distance-matri
## 1D index to 2D index
finv <- function (k, dist_obj) {
  if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
  n <- attr(dist_obj, "Size")
  valid <- (k >= 1) & (k <= n * (n - 1) / 2)
  k_valid <- k[valid]
  j <- rep.int(NA_real_, length(k))
  j[valid] <- floor(((2 * n + 1) - sqrt((2 * n - 1) ^ 2 - 8 * (k_valid - 1))) / 2)
  i <- j + k - (2 * n - j) * (j - 1) / 2
  cbind(i, j)
}

#dd is dist object
#finv(which.min(dd),dd)

head(trainset[clusters==i,-ncol(trainset)])


#pososta yes kai no se kathe cluster gia na poume poso kala einai
#single
#gia to 1
dim(trainset[clusters==1 & trainset$y==1,])[1]
# 2572 yes
dim(trainset[clusters==1 & trainset$y==0,])[1]
#25206 no
2572/(25206+2572)
# 9% yes 90% no
#gia to 2
dim(trainset[clusters==2 & trainset$y==1,])[1]
#375
dim(trainset[clusters==2 & trainset$y==0,])[1]
#gia to 3
dim(trainset[clusters==3 & trainset$y==1,])[1]
#0
dim(trainset[clusters==3 & trainset$y==0,])[1]
#1
#gia to 4
dim(trainset[clusters==4 & trainset$y==1,])[1]
#0
dim(trainset[clusters==4 & trainset$y==0,])[1]
#1


#complete
#gia to 1
dim(trainset[clusters==1 & trainset$y==1,])[1]
# 3042 yes
dim(trainset[clusters==1 & trainset$y==0,])[1]
#25408 no
#gia to 2
dim(trainset[clusters==2 & trainset$y==1,])[1]
#201
dim(trainset[clusters==2 & trainset$y==0,])[1]
#95
dim(trainset[clusters==3 & trainset$y==1,])[1]
#5
dim(trainset[clusters==3 & trainset$y==0,])[1]
#7
#gia to 4
dim(trainset[clusters==4 & trainset$y==1,])[1]
#0
dim(trainset[clusters==4 & trainset$y==0,])[1]
#74

for (i in 1:nCluster){
  a=dim(trainset[clusters==i & trainset$y==1,])[1]
  b=dim(trainset[clusters==i & trainset$y==0,])[1]
  
  if(majorityClass[i] == 0){
    x= (b/(b+a))
    
  }
  else{
    x = (a/(b+a))
  }
  print(x)
  
}
  
  