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
library(VIM)
library(gdata)
#library(plyr)#prosoxi prota prepei na ginei load i plyr kai meta i dplyr
#library(dplyr)
library(ggplot2)
#library(FSelector)
#library(e1071)
library(caret)
#library(pander)
#library(class)
#library(MLmetrics)
#library(mlbench)
#library(klaR)
#library(ROCR)
#library(cluster)
#library(factoextra)
#library(NbClust)
#library(pracma)
#library(mixtools)
#library(dbscan)

#load our functions
source("plot_characteristics_hist.R")
source("multiplot.R")
source("plot_characteristics_by_y.R")
source("impute_with_mice.R")


#library(DataExplorer)
#create_report(train)
# Import data from csv
bank_data_original=read.table(file ="bank-additional/bank-additional-full.csv",header=TRUE,sep=";")
bank <- bank_data_original

# Data exploration
head(bank)
class(bank)
dim(bank)
str(bank)
summary(bank)

############################################################################################
#prosthetoume to xaraktiritiko year
dummy1=0
for(i in 1:dim(bank)[1]){
  bank$year[i]<-2008+dummy1
  if(bank$month[i]=="dec" && bank$month[i+1]=="mar"){dummy1 <- dummy1+1}
}
dim(bank)
#epidi to year mpike san teleutaia stili kai to y einai proteleutaio, ta ksanavazoume opos prepei dil to y teleutaio
#kanoume reorder ta columns
bank <- bank[,c(1:20,22,21)]
############################################################################################
#diafora simperasmata-patterns gia ta dedomena mas
sum(bank$y=="yes")
sum(bank$y=="no")

# p.x. to 54% auton pou ekanan prothesmiaki katathsei itan pantrmenoi
dim(bank[bank$y=="yes" & bank$marital== "married",])[1]
# 54% = (100x2532)/4640

#not have any credit default
dim(bank[bank$y=="yes" & bank$default== "no",])[1]
#90%

#den exoun prosopiko daneio
dim(bank[bank$y=="yes" & bank$loan== "no",])[1]
#82%

#den eixan progoumeni tilefoniki epikoinonia apo thn kampania
dim(bank[bank$y=="yes" & bank$poutcome== "nonexistent",])[1]
#67.6

dim(bank[bank$y=="yes" & bank$poutcome== "nonexistent",])[1]

dim(bank[bank$y=="yes" & bank$year== 2008,])[1]
#1339/4640 = 28%
dim(bank[bank$y=="yes" & bank$year== 2009,])[1]
#48%
dim(bank[bank$y=="yes" & bank$year== 2010,])[1]
#23%

#rithmos ana xronia
#2008
dim(bank[bank$y=="yes" & bank$year== "2008",])[1]
#1339
dim(bank[bank$y=="no" & bank$year== "2008",])[1]
#26351
1339/(26351+1339)
#0.04835681
#2009
dim(bank[bank$y=="yes" & bank$year== "2009",])[1]
#2228
dim(bank[bank$y=="no" & bank$year== "2009",])[1]
#9212
2228/(9212+2228)
#0.1947552
#2010
dim(bank[bank$y=="yes" & bank$year== "2010",])[1]
#1073
dim(bank[bank$y=="no" & bank$year== "2010",])[1]
#985
1073/(985+1073)
#0.52138

bank$year<-NULL
############################################################################################
#plot histogram ton xaraktiristikon

#PROSOXI ta kanei save se .png arxeia
#plot_characteristics_hist(bank)

#plot ton xaraktiristikon se sxesi me tin target variable y
#plot_characteristics_by_y(bank,"default",save_to_file = FALSE)
############################################################################################

#Metatrepoume ta unknown se NA gia na ta anagnorizei san agnosotus i R
bank=unknownToNA(x=bank,unknown = "unknown")
#metrame ta NAs
sum(is.na(bank$default))
sum(is.na(bank) & bank$y=="yes")
sapply(bank, function(x) sum(is.na(x)))
anyNA(bank)

#plot
aggr(bank, col=c('green','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#arxika vlepoume se pososto % poses times loipoun apo to kathe xaraktiristiko
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(bank,2,pMiss)

#gia na doume posa yes leipoun sta NAs
sum(is.na(bank$job) & bank$y=="yes") 
#37
sum(is.na(bank$marital) & bank$y=="yes") 
#12
sum(is.na(bank$education) & bank$y=="yes") 
#251
sum(is.na(bank$default) & bank$y=="yes") 
#443
sum(is.na(bank$housing) & bank$y=="yes") 
#107
sum(is.na(bank$loan) & bank$y=="yes") 
# 107
############################################################################################

#Data preparation cleaning

#svisimo duplicates(nai exei eksarxis)
bank <- unique(bank)

#duplicated
#anyDuplicated


#vlepoume poia xaraktiristika zero/Near-zero variance predictors
x = nearZeroVar(bank, saveMetrics = TRUE)
str(x, vec.len=2)
#explore which ones are the zero variance predictors
x[x[,"zeroVar"] > 0, ] 
#explore which ones are the near-zero variance predictors
x[x[,"zeroVar"] + x[,"nzv"] > 0, ] 

#kanoume imputation sta dedomena pou leipoun

#arxika vlepoume se pososto % poses times loipoun apo to kathe xaraktiristiko
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(bank,2,pMiss)

#epeidi argei to impute exoume kanei save to workspace kai to kanoume load
#load("data_imputed_with_MICE.RData") 
load("imputed_mice_variable.Rdata")
bank <- imputed_mice
#imputation ton dedomenon me mice
imputed_mice = impute_with_mice(bank)
#to save image einai gia olo to workspace
#save.image(“data_imputed_with_MICE.RData”)

#svinoume ta xaraktiristika pou den xreiazonte
bank$duration <- NULL
#bank$month <- NULL
#bank$day_of_week <- NULL
bank$default <- NULL
bank$pdays <- NULL
summary(bank)
#efoson svisame stiles, ksanasvinoume ta duplicates pou proekipsan
bank <- unique(bank)
dim(bank)

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
summary(factor(training$y))
summary(factor(testing$y))

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




