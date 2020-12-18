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
#library(class) #KNN
#library(ROCR)
library(cluster)#KMEANS,HIERARCHICAL
#library(factoextra)
#library(NbClust)
#library(pracma)
#library(mixtools)
#library(dbscan)
#library(DataExplorer) #create_report(train)

# Import data from csv
bank_data_original=read.table(file ="bank-additional/bank-additional-full.csv",header=TRUE,sep=";")
bank <- bank_data_original

#bank_data_original=read.table(file ="bank-additional/bank-additional.csv",header=TRUE,sep=";")


#shuffle Data
#PROSOXI PREPEI OPOSDIPOTE NA EINAI FALSE, allios vgainoun xiliades duplicates
#search google for sampling with replacement
set.seed(248)
bank <- bank[sample(1:nrow(bank),replace=FALSE), ]


#bank <- bank[,c("duration","poutcome","emp.var.rate","euribor3m","y")]

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
dmy <- dummyVars(" ~ .", data = trainTransformed, fullRank=T)
trainset <- data.frame(predict(dmy, newdata = trainTransformed))
#epidi to target egine y.no kai y.yes kratame mono to ena apo ta 2 kai to leme y
colnames(trainset)[dim(trainset)[2]] <- "y"


dmy <- dummyVars(" ~ .", data = testTransformed, fullRank=T)
testset <- data.frame(predict(dmy, newdata = testTransformed))
#epidi to target egine y.no kai y.yes kratame mono to ena apo ta 2
colnames(testset)[dim(testset)[2]] <- "y"


###################UNSUPERVISED
#
#Kmeans

#https://stackoverflow.com/questions/34543338/good-way-to-find-majority-in-a-vector-in-r/34543390
#kmeans

#to an tha vgaloume 0 epireazei toso apo to scaling pano, an ginei diladi se center/variance i 0-1
#alla kai an valo nstart. Me center/variance kai xoris nstart vgazi 0 gia pio mikra k

#me isoropimeno set kai 3 kentra vgenei konta sto 0.16
nCenter = 3
#set.seed(208)
#model = kmeans(trainset[,-ncol(trainset)],centers = nCenter)
model = kmeans(trainset[,-ncol(trainset)],centers = nCenter,nstart = 50,iter.max = 15)
#model = kmeans(trainset[,-ncol(trainset)], nstart = 2,centers = 3)
#model = kmeans(trainset[,-ncol(trainset)],centers = trainset[10:11,])

#posa stoixeia exei to kathe cluster
#model$size


#Clustering evaluation
# Compute WSS
cohesion = model$tot.withinss
cohesion
# Compute BSS
separation = model$betweenss
separation
# Compute Silhouette
d = dist(trainset[,-ncol(trainset)])
load("dist_unscaled.Rdata")
model_silhouette = silhouette(model$cluster, d)
#model_silhouette
mean(model_silhouette[, 3])

#vrisko poia einai i pliopsifousa timi se kathe cluster
#diladi vazo ta y pou ksero kai metrao poia einai perissotera 0 i 1 apo to y
#pio aplo xoris to tt
majorityClass <- NULL
for (i in 1:nCenter){
  if(dim(trainset[model$cluster==i & trainset$y==0,])[1]>dim(trainset[model$cluster==i & trainset$y==1,])[1]){
    majorityClass <- c(majorityClass,0)
  }
  else{
    majorityClass <- c(majorityClass,1)
  }
  
}
majorityClass

#stackoverflow
closest.cluster <- function(x) {
  cluster.dist <- apply(model$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}
pred.test <- apply(testset[-ncol(testset)], 1, closest.cluster)
#to pano egine apply sta arxika kentra, emeis omos dosame sta kentra tin timi
#tis pleiopsifousas klasis, opote metatrepoume tin timi tou kentrou se auti pou
#dosame
for(i in 1:nrow(testset)){
  pred.test[[i]] <- majorityClass[model$cluster[[i]]]
}

#tora kratame mono to 2 kommati pou einai i provlepsi
pred<-NULL
for(i in 1:nrow(testset)){
  pred[i] <- pred.test[[i]]
}

#create file for statistics
#sink("kmeans.txt", append=TRUE, split=TRUE)
#cat("kmeans")
#sto factor thelei to levels c(0,1), giati epeidi sto pred ola einai 1 , otan kanoume factor
#den vriskei kapoio 0 , opote exoume mono ena factor to 1, eno to testset exei 2 factors 0 k 1
#akomi kai na min to valoume omos, pali douleuei , vgazei sosto apotelesma me kapoio warning
results <- confusionMatrix(factor(pred,levels = c(0,1)), factor(testset[,ncol(testset)]),positive = '1')
setNames(data.frame(results$overall["Accuracy"],results$byClass["Precision"],results$byClass["Recall"],results$byClass["F1"],row.names = NULL),c("Accuracy","Precision","Recall","F1"))


source("draw_confusion_matrix.R")
draw_confusion_matrix(results)

#epistrefoume tin eksodo stin konsola
#sink()


#####ELBOW
#prosoxi edo einai gia olo to set
#episis pio pano exo petaksi to y apo to trainset eno edo den xreiazetai
#enonoume ta dedomena train kai test
cdata <- rbind(trainset,testset)
target <- cdata[,ncol(cdata),drop = FALSE]
cdata <- cdata[,-ncol(cdata)]

cdata<-trainset[,-ncol(trainset)]
SSE <- (nrow(cdata) - 1) * sum(apply(cdata, 2, var))

for (i in 2:10)
  SSE[i] <- kmeans(cdata, centers = i)$tot.withinss

plot(1:10, SSE, type="b", xlab="Number of Clusters",ylab="SSE")
abline(v=3, lty=2)

#plot heatmap
train_ord = trainset[order(model$cluster),]
d = dist(train_ord)#an to valoume apeutheias mesa sto heatmap den trexei
g=as.matrix(d)
heatmap(as.matrix(d), Rowv = NA, Colv = NA,col = heat.colors(256), revC = TRUE)


#pososta yes kai no se kathe cluster gia na poume poso kala einai
#gia to 1
dim(trainset[model$cluster==1 & trainset$y==1,])[1]
# 681 yes
dim(trainset[model$cluster==1 & trainset$y==0,])[1]
#391 no
681/(681+391)
# 63%
#gia to 2
dim(trainset[model$cluster==2 & trainset$y==1,])[1]
#1660
dim(trainset[model$cluster==2 & trainset$y==0,])[1]
#6800
1660/(1660+6800)
# 19% gia yes i 80% gia no
#gia to 3
dim(trainset[model$cluster==3 & trainset$y==1,])[1]
#907
dim(trainset[model$cluster==3 & trainset$y==0,])[1]
#18393
907/(18393+907)
#4% gia yes 95% gia no


for (i in 1:nCenter){
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
