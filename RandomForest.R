
library(ggplot2)
library(randomForest)
library(readr)
library(dplyr)
setwd("C://Users//Vamsi//Desktop//R.Alg//practice//kaggle//ottto data set")

train = read.csv('train.csv', header = T, stringsAsFactors = F)
test <- read.csv('test.csv', header=TRUE, stringsAsFactors = F)

dim(train)
dim(test)

train$id = NULL
test$id = NULL

table(train$target)


train$target = gsub("Class_","",train$target) %>% {as.integer(.) -1}

table(train$target)
class(train$target)
train$target = as.factor(train$target)

varNames = names(train)

# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("target")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("target", varNames1, sep = " ~ "))


library(foreach) 
library(doParallel)

registerDoParallel(cores=2)

require(randomForest)

#Tuning no.of Trees
#-------------------
seed = 100
modellist <- list()
for (ntree in c(100,500,750,1000,1500,2000,2500)) {
  set.seed(seed)
  fit <- randomForest(target~., data=train, metric="accuracy", ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

stopCluster(cl)

# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


#Tuning MTRY :
#--------------
mtry <- tuneRF(train[,-94], as.factor(train$target),  mtrystart = 1,  ntreeTry=20,  
               stepFactor=1.5,  improve=0.01, trace=TRUE,  plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


#Finally building model with best mtry and ntrees
#-------------------------------------------------
train$target = as.factor(train$target)
fit.rf = randomForest(rf.form, data = train, mtry = 19, ntree=500, importance=T)

fit.rf
fit.rf$confusion
fit.rf$mtry
fit.rf$importance


