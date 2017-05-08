rm(list = ls())

require(arules)

library(readr)
library(dplyr)
library(xgboost)
library(ggplot2)
library(gridExtra)

train <- read.csv("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\life insurance\\train.csv",stringsAsFactors = T) #59,381 observations, 128 variables
test <- read.csv("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\life insurance\\test.csv",stringsAsFactors = T) #19,765 observations, 127 variables - test does not have a response field

dim(train)
dim(test)
str(train)
table(train$Response)

cat("Train data has", nrow(train), "rows and", ncol(train), "columns! \n")
cat("Test data has", nrow(test), "rows and", ncol(test), "columns! \n")

sum(is.na(train)) / (nrow(train) * ncol(train))
sum(is.na(test)) / (nrow(test) * ncol(test))

#no.of missing values in each col
data.frame(sort(apply(train, 2, function(x) { sum(is.na(x)) })))
data.frame(sort(apply(test, 2, function(x) { sum(is.na(x)) })))

# %ge of missing values in each col
data.frame(sort(apply(train, 2, function(x) { sum(is.na(x))/length(x)*100 })))
data.frame(sort(apply(test, 2, function(x) { sum(is.na(x))/length(x)*100 })))


cat.var.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Family_Hist_1", paste("Medical_History_", c(2:14, 16:23, 25:31, 33:41), sep=""))
cont.var.names <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                    "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                    "Family_Hist_5")
disc.var.names <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
                    paste("Medical_Keyword_", 1:48, sep=""))

train.cat <- train[, cat.var.names]
test.cat <- test[, cat.var.names]

train.cont <- train[, cont.var.names]
test.cont <- test[, cont.var.names]

train.disc <- train[, disc.var.names]
test.disc <- test[, disc.var.names]

train.cat <- as.data.frame(lapply(train.cat, factor))
test.cat <- as.data.frame(lapply(test.cat, factor))

#------------------
str(train.cat)
str(train.cont)

str(train.disc)

str(test.cont)

str(test.disc)

summary(train.cont)

summary(train.disc)

summary(test.cont)

summary(test.disc)

str(train.cat)

str(test.cat)

summary(train.cat)

summary(test.cat)
#-----------------------

train1 = cbind(train.cat,train.cont,train.disc,train$Response)
test1 = cbind(test.cat,test.cont,test.disc)

library(dplyr)
train1 = rename(train1, Response=`train$Response`)

data.frame(apply(train1, 2, function(x) { sum(is.na(x)) }))
data.frame(apply(test1, 2, function(x) { sum(is.na(x)) }))

####################################################################
#Can we see any different missing data structure depending on the response?
train.na.per.response <- sapply(sort(unique(train1$Response)), function(x) { apply(train1[train1$Response == x, ], 2, function(y) { sum(is.na(y)) }) })
train.na.per.response

round(colSums(train.na.per.response) / sum(train.na.per.response), digits=4)


for (i in which(sapply(train1, is.numeric))) {
  train1[is.na(train1[,i]), i] = median(train1[, i],na.rm = TRUE)
}

ncol(is.na(train1))

for (i in which(sapply(train1, is.factor))) {
  train1[is.na(train1[,i]), i] <- mode(train1[, i])
}


table(train1$Medical_History_10)


##################################################################
library(randomForest)
library(dplyr)

train1.i = na.roughfix(train1)

data.frame(apply(train1.i, 2, function(x) { sum(is.na(x)) }))

test1.i = na.roughfix(test1)

data.frame(apply(test1.i, 2, function(x) { sum(is.na(x)) }))

str(train1.i)

train1.i$Medical_History_2 = as.numeric(train1.i$Medical_History_2)
train1.i$Medical_History_10 = as.numeric(train1.i$Medical_History_10)


#----------------------------------
varNames = names(train1.i)

# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("Responce")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
form <- as.formula(paste("Response2", varNames1, sep = " ~ "))

######################
# Model building
######################

#multinom logistic regression
#==============================
library(nnet)

train1.i$Response = as.factor(train1.i$Response)

fit = multinom(Response~., data = train1.i)

fit = lm(Response~., data = train1.i)


#Ordinal Logistic Regression (OLR) in R
#========================================
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

m <- polr(Response ~., data = train1.i, Hess=TRUE)

summary(m)
help(glm)
help(family) 

install.packages("rms")
library(rms)
xx = lrm(Response ~ .,data = train1.i)


###########################################################################
library(foreach) 
library(doParallel)
#register cluster for parallel processing
cl = makeCluster(detectCores())
registerDoParallel(cl)
stopCluster(cl)

library(doParallel)
registerDoParallel(cores=2)
foreach(i=1:3) %dopar% sqrt(i)

library(randomForest)

fit1 = randomForest(Response~.,data =train1.i, ntree = 600 )


#Variable Importance Using Random Forest  
#Run time constrain - Better results with ntree = 35
rf = randomForest(Response~., data = train.imp.com , ntree=30, importance=TRUE )
img=varImpPlot(rf, sort=TRUE, n.var =30, bg=4, lcolor="red" , color="black" , main="RondomForest-Importance Plot")

#------------------------------------------------------------------

train1.i
test1.i

train.rf = train1.i[sample(nrow(train1.i), nrow(train1.i)*.7),]
test.rf <- train1.i[-train.rf, ]

ntree = c(600,700,800,900,1000)
mtry = c(11,12,13)
nodesize = c(4,5,6)


for (t in ntree){
  for (m in mtry){
    for (n in nodesize){
      
        rf_train<- train.rf
        
        rf_test<- test1.i[,-c(127)]
        
        rf_fit<-randomForest(x=rf_train[,-c(127)], y = rf_train[,c(127)], 
                             ntree = t, mtry = m, nodesize = n)
        
        rf_pred <- predict(rf_fit, rf_test, type = "prob")[,2]
        
        rf_pred_class <- ifelse(rf_pred>0.5, 1, 0)
        
        rf_accuracy_poss[i]<- 1 - sum(rf_test$Response != rf_pred_class)/nrow(rf_test)
      } 
      print(paste("number of trees: ",t,"number of features: ", m, "nodesize :", n,
                  "Cross-Validation mean Accuracy",mean(rf_accuracy_poss)))
      
      rf_accuracy_all<- rbind(rf_accuracy_all, data.frame(t,m,n,mean(rf_accuracy_poss)))
    }
  }



train.rf$
train$Response





########################################################################
train.xgb = train1.i

y = train.xgb$Response
require(xgboost)

train.xgb$Response = ifelse(train.xgb$Response ==8,0,train.xgb$Response) 
table(train.xgb$Response)
table(train$Response)

bstDense <- xgboost(data = data.matrix(train.xgb[,-127]), label = y, 
                    max.depth = 2, eta = 1, nthread = 2, nround = 100, 
                    objective = "multi:softmax",num_class = 8)




summary(bstDense)

