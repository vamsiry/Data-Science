#--------------------------------------
library(foreach) 
library(doParallel)

#register cluster for parallel processing
cl = makeCluster(detectCores())
registerDoParallel(cl)
stopCluster(cl)
#----------------------
library(doParallel)
registerDoParallel(cores=2)
foreach(i=1:3) %dopar% sqrt(i)
#-------------------------------
library(caret)

db_train <- read.csv("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\Santander Customer Satisfaction\\train.csv", na.strings = "")
db_test = read.csv("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\Santander Customer Satisfaction\\test.csv", na.strings = "")

dim(db_train)
dim(db_test)

db_train$ID = NULL
db_test$ID = NULL

y = db_train$TARGET
db_train$TARGET = NULL

db_train$sep = 1
db_test$sep = 0

full_data = rbind(db_train,db_test)


#########################################################################
#-------------------------
#Cheecking for Missing Data

non_mis_val_data = full_data[!complete.cases(full_data),]
dim(non_mis_val_data)

#no NA's in the data

#--------------------------------------------------
#Creating Dummy Variables for categorical variables

# no categorical variables in the data

#########################################################################
#-------------------------------------------
# Removing NearZerovar col

x = full_data[,-nearZeroVar(full_data)]

dim(full_data)
dim(x)
class(x)

#-------------------------------------------------
#Removing constant features(same NearZerovar in diff way)

cat("\n## Removing the constants features.\n")

for (f in names(x)) {
  if (length(unique(x[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    x[[f]] <- NULL
    
  }
}

dim(x)

#########################################################################
#----------------------------------
# Removing identical features

features_pair <- combn(names(x), 2, simplify = F)

toRemove <- c()

for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(x[[f1]] == x[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

x$ind_var37 = NULL

dim(x)


#----------------------------------------------------------------------
# find and remove vectors that are linear combinations of other vectors

x1 <- findLinearCombos(x) #need to have a clear info 
head(x1)

x1 <- x[ ,-x1$remove]
dim(x1)
str(x1)

#----------------------------------------------------------------------
# find and remove vectors that are highly corrolated to other vectors
#searches through corrlation matrix and returns a vector of integers 
#correxponding to columns to remove to reduce pairwise correlation

HIGHCOR <- findCorrelation(cor(x1[,1:ncol(x1)]), cutoff = .95, verbose = FALSE)

length(HIGHCOR)
names(x1[,HIGHCOR])

x1 <- x1[,-HIGHCOR]

#--------------------------
# multicollinearity

# use VIF fun to identifie highly correlated col (or) use cor() fun insted 
#otherwise if u hv more correlated col do PCA transformation

library(usdm)

vif(x1)
dim(x1)

x1 = x1[,vif(x1)$VIF<10]
dim(x1)


viff = as.data.frame(vif(x1))
viff[order(viff$VIF,decreasing = T), ]


########################################################################
#PCA computed using Covariance/correlation matrix
#--------------------------------------------------
pca1 = princomp(x1, cor=F)
names(pca1)
summary(pca1)
plot(pca1, type="lines")
pca1$loadings
pca1$scores
sum(diag(cov(pca1$scores)))


#PCA computed using SVD
#-----------------------
pca2 = prcomp(x1, scale.=TRUE)
names(pca2)
summary(pca2)
pca2$rotation
pca2$x
plot(pca2, type="lines")

x11 = pca2$x[,1:23]
dim(xx1)

# pca using caret package
#-------------------------
library(caret)
preObj = preProcess(x1, method=c("pca"), thresh = 1.0)
preObj$rotation

pca3 = predict(preObj,x1)
dim(pca3)

#######################################################################
#probability based sapling

#sample.ind <- sample(2, nrow(data.rf), replace = T, prob = c(0.8,0.2))
#table(sample.ind)
#data.rf1 <- data.rf[sample.ind==1,]
#data.rf2 <- data.rf[sample.ind==2,]

############################################################
################ Model Building ####################

# use x1 or pca1 or pca2 or pca3 objects for independent variables

###########################################################
################ Random forest model ######################
#-----------------------------------------------------------

rf.train = x1[x1$sep==1,]
rf.test = x1[x1$sep==0,]

rf.train$sep = NULL
rf.test$sep = NULL

rf.train = cbind(rf.train,y)

dim(rf.train)
dim(rf.test)

table(rf.train$y)
class(rf.train$y)
rf.train$y = as.factor(rf.train$y)
str(rf.train$y)


varNames = names(rf.train)

# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("y")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("y", varNames1, sep = " ~ "))


require(randomForest)

mtry <- tuneRF(rf.train[,-19], rf.train$y,  mtrystart = 1,  ntreeTry=200,  
               stepFactor=1.5,  improve=0.01, trace=TRUE,  plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


fit.rf = randomForest(rf.form, data = rf.train, mtry = 5, ntree=500, importance=T)

fit.rf
fit.rf$confusion
fit.rf$mtry
fit.rf$importance

############################################################################
#var imprtance plot : Top 5 variables are selected and plotted based on 
#Model Accuracy and Gini value. We can also get a table with decreasing 
#order of importance based on a measure (1 for model accuracy and
#2 node impurity)

varImpPlot(fit.rf,sort = T, main="Variable Importance",n.var=5)


#variable importnce table
var.imp2 <- data.frame(importance(fit.rf,type=2)) #MeanDecreaseGini

var.imp1 <- data.frame(importance(fit.rf,type=1)) #MeanDecreaseAccuracy


# make row names as columns
var.imp2$Variables <- row.names(var.imp2)
var.imp2[order(var.imp2$MeanDecreaseGini,decreasing = T),]

var.imp1$Variables <- row.names(var.imp1)
var.imp1[order(var.imp1$MeanDecreaseAccuracy,decreasing = T),]

###########################################################################



#---------------------------------------------------------------------------
#Predict Response Variable Value using Random Forest
#---------------------------------------------------

predicted.response.rf1 <- predict(fit.rf ,data.rf1)
class(predicted.response.rf1)


predicted.response.rf11 <- predict(fit.rf ,data.rf1,"prob")

predicted.response.rf11 <- predicted.response.rf11[,2]

# Create Confusion Matrix : based on actual response variable and predicted value.
confusionMatrix(data=predicted.response.rf1,reference=data.rf1$y, positive='1')


#Now we can predict response for the validation sample and calculate model accuracy for the sample.
#---------------------------------------------------------------------------------

predicted.response.rf2 <- predict(fit.rf ,data.rf2)
class(predicted.response.rf2)
class(data.rf2$y)


predicted.response.rf2 <- predict(fit.rf ,data.rf2,"prob")# o get the probabilites

head(predicted.response.rf2)


# Create Confusion Matrix : based on actual response variable and predicted value.
confusionMatrix(data=predicted.response.rf2, reference=data.rf2$y)




#################################################################################
#ROC curve : 
#============

#Creating performance object
library("ROCR")

perf.obj <- prediction(predicted.response.rf11, data.rf1$y)

# Get data for ROC curve
roc.obj <- performance(perf.obj, measure="tpr", x.measure="fpr")
plot(roc.obj,
     main="ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")


#Getting Lift Charts : use measure="lift", x.measure="rpp" in the performance function.
#=========================

#Creating performance object
perf.obj <- prediction(predicted.response.rf11, data.rf1$y)

# Get data for lift curve
lift.obj <- performance(perf.obj, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")


#Cumulative Lift Chart using R
#==============================

install.packages("gains")
library(gains)
# gains table
actual <- ifelse(data.rf1$y==1,1,0)
gains.cross <- gains(actual=actual , 
                     predicted=predicted.response.rf11,
                     groups=10)
print(gains.cross)

############################################################################



########################################################################
#data set is soo imbalenced so apply doing under sampling and run the model
#========================================================

all.0 = rf.train[rf.train$y==0,]
all.1 = rf.train[rf.train$y==1,]
dim(all.0)
dim(all.1)


train_ind <- sample(nrow(all.0),nrow(all.0)*.35)

rf.train1 <- all.0[train_ind, ]
new.rest <- all.0[-train_ind, ]

train_ind <- sample(nrow(new.rest),nrow(new.rest)*.50)

rf.train3 = new.rest[-train_ind,]
rf.train3 = new.rest[train_ind,]

dim(rf.train1)
dim(rf.train2)
dim(rf.train3)

new.data1 = rbind(rf.train1,all.1)
dim(new.data1)

mtry <- tuneRF(new.data1[,-50], new.data1$y, data = new.data1, mtryStart = c(1:7), ntreeTry=100,trace=TRUE,plot=TRUE)

new.data2

rm(db_test)
rm(db_train)
rm(mtry)

fit.rf = randomForest(new.data1[,-50], new.data1$y, data = new.data1, 
                      mtry = 45, ntree=500, importance=T)



table(new.data1$y)


control <- trainControl(search="grid")
seed <- 7
set.seed(seed)
tunegrid <- expand.grid(.mtry=c((1:7)^2))

rf_gridsearch <- train(y~., data=new.data1, method="rf", metric=metric, 
                       tuneGrid=tunegrid, trControl=control)


print(rf_gridsearch)
plot(rf_gridsearch)


#=================================================================
####################### Random forest (CARET) #########################
#=================================================================

#Random Forest uses Gini Index based impurity measures for building decision 
#tree. Gini Index is also used for building Classification and Regression 
#Tree (CART).

#normal sampling
#------------------
data.rf = cbind(x22,y)

data.rf1 = data.rf[sample(nrow(data.rf),replace=F,size=0.80*nrow(data.rf)),]

dim(data.rf1)
table(data.rf1$y)

data.rf2 = data.rf[-data.rf1$y,]


#probability sampling
#---------------------
sample.ind <- sample(2, nrow(data.rf),replace = T,prob = c(0.8,0.2))
table(sample.ind)

data.rf1 <- data.rf[sample.ind==1,]

data.rf2 <- data.rf[sample.ind==2,]

#==============================================================
#Fine tuning RF Model(CARET)
#-----------------------------
#Test Algorithm with default parameters
#----------------------------------------
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
set.seed(seed)
metric <- "Accuracy"
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)

rf_fit <- train(rf.form, data=rf.train, method="rf", metric=metric,
                tuneGrid=tunegrid, trControl=control,ntree = 50)
print(rf_fit)

rf_fit$results
rf_fit$finalModel
table(rf.train$y)
rf_fit$resample
rf_fit$resampledCM


#1. Tune Using Caret
#---------------------
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
metric <- "Accuracy"
mtry <- sqrt(ncol(x))
rf_random <- train(rf.form, data=rf.train, method="rf", 
                   metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


#Grid Search
#---------------
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))

rf_gridsearch <- train(rf.form, data=rf.train, method="rf",
                       metric=metric, tuneGrid=tunegrid, trControl=control)

print(rf_gridsearch)
plot(rf_gridsearch)


#2. Tune Using Algorithm Tools
#-------------------------------
# Algorithm Tune (tuneRF)
set.seed(seed)
mtry <- tuneRF(rf.form, data=rf.train, mtryStart = 1, stepFactor=1.5, improve=1e-5, 
               ntree=500, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#3. Craft Your Own Parameter Search
#-------------------------------------
#Often you want to search for both the parameters that must be tuned (handled by caret) and the 
#those that need to be scaled or adapted more generally for your dataset.

#You have to craft your own parameter search.

#Two popular options that I recommend are:

#1.Tune Manually: Write R code to create lots of models and compare their accuracy using caret
#2.Extend Caret: Create an extension to caret that adds in additional parameters to caret for the algorithm you want to tune.


#1.Tune Manually
#--------------
# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")

tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))

modellist <- list()

for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(rf.form, data=rf.train, method="rf", metric=metric,
               tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


#2.Extend Caret
#-------------
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), 
                                  class = rep("numeric", 2), label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]

customRF$levels <- function(x) x$classes

#Now, let's make use of this custom list in our call to the caret train function,
#and try tuning different values for ntree and mtry.
# train model
#------------
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
metric <- "Accuracy"
set.seed(seed)
custom <- train(rf.form, data=rf.train, method=customRF, metric=metric,
                tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)














