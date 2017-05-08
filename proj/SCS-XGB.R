#--------------------------------------
library(foreach) 
library(doParallel)

registerDoParallel(cores=2)

#-------------------------------------
#register cluster for parallel processing
library(foreach) 
library(doParallel)

cl = makeCluster(detectCores())
registerDoParallel(cl)
stopCluster(cl)
#----------------------

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

#----------------------------------------------------------------------
# find and remove vectors that are highly corrolated to other vectors
#searches through corrlation matrix and returns a vector of integers 
#correxponding to columns to remove to reduce pairwise correlation

HIGHCOR <- findCorrelation(cor(x1[,1:ncol(x1)]), cutoff = .95, verbose = FALSE)

length(HIGHCOR)
names(x1[,HIGHCOR])

x1 <- x1[,-HIGHCOR]

###########################
x1$var38 <- log(x1$var38)

dim(x1)
#--------------------------
# multicollinearity

# use VIF fun to identifie highly correlated col (or) use cor() fun insted 
#otherwise if u hv more correlated col do PCA transformation

library(usdm)

vif(x1)
dim(x1)

x2 = x1[,vif(x1)$VIF<10]
dim(x1)


viff = as.data.frame(vif(x1))
viff[order(viff$VIF,decreasing = T), ]
x2 = x1[,vif(x1)$VIF<15]


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
################ XGB model ######################
#-----------------------------------------------------------

xgb.train = x1[x1$sep==1,]
xgb.test = x1[x1$sep==0,]

xgb.train$sep = NULL
xgb.test$sep = NULL

xgb.train = cbind(xgb.train,y)

dim(xgb.train)
dim(xgb.test)

y=as.numeric(y)
class(y)


#===========================================================================

require(xgboost)
require(Matrix)

#---------------------------------------------------------------------
#Remember Y must be y = as.numeric(y) for XGB bcz in XGB "y" var should be numeric
#--------------------------------------------------------------------
train <- sparse.model.matrix(y ~ ., data = xgb.train)

dtrain <- xgb.DMatrix(data=train, label=y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eval.metric         = "error",
                eval.metric         = "logloss",
                eta                 = 0.0202048,#1 #learning rate - Number of Trees
                max_depth           = 5, # maximum depth of a tree
                subsample           = 0.6815, # subsample ratio of the training instance
                colsample_bytree    = 0.701, # subsample ratio of columns
                min_child_weight    = 1, # minimum sum of instance weight (defualt)
                scale_pos_weight    = 1, # helps convergance bc dataset is unbalanced
                print.every.n       = 1
                #early.stop.round   = 10 # stop if no improvement within 10 trees
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 560,
                    print.every.n       =  10,
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = TRUE
                    
)

clf$finalmodel
summary(names(clf))
summary(clf$finalmodel)
names(clf)


#========================================================================== 
# cross-validate xgboost to get the accurate measure of error
xgb_cv = xgb.cv(params = param,
                data = dtrain,
                #data = as.matrix(train.1)
                #label = train.1$y,
                nrounds = 100, 
                nfold = 10,         # number of folds in K-fold
                prediction = TRUE,  # return the prediction using the final model 
                #showsd = TRUE,      # standard deviation of loss across folds
                stratified = TRUE,  # sample is unbalanced; use stratified sampling
                verbose = TRUE,
                print.every.n = 1, 
                early.stop.round = 10
)


pred <-predict(clf,train)

prediction <- as.numeric(pred > 0.5)

print(head(prediction))

err <- mean(prediction != y)
print(paste("test-error=", err))
  
table(prediction,y)
table(y)



#################################################################################
# Using CARET package to build XGBoost model
#############################################
?make.names

train1 = xgb.train

fitControl <- trainControl(method = "cv", number = 10, repeats = 2, search = "random")
# train a xgbTree model using caret::train
model <- train(factor(y)~., data = train1, method = "xgbTree", trControl = fitControl)

model$resample
model$resampledCM
#---------------------------------------------------
# Tuning hyperparameters
# pack the training control parameters


# train the model for each parameter combination in the grid, 
# using CV to evaluate
library(dplyr)

train <- sparse.model.matrix(y ~ ., data = xgb.train)
dtrain <- xgb.DMatrix(data=train, label=y)

y = as.factor(y)
dim(xgb.train)

train = xgb.train

set.seed(45)

xgb.trcontrol = trainControl(
                method = "repeatedcv",
                repeats = 1,
                number = 10,
                verboseIter = TRUE, 
                returnData = FALSE,
                returnResamp = "all", # save losses across all models
                classProbs = TRUE, # set to TRUE for AUC to be computed
                summaryFunction = twoClassSummary,
                allowParallel=TRUE
)


xgb.grid = expand.grid(
           nrounds = 1000,
           #eta = c(0.01,0.05,0.1)
           eta = c(0.01, 0.001, 0.0001),
           max_depth = c(2, 4, 6, 8, 10),
           min_child_weight = 1,
           gamma = 1
)

xgb.tune = train(
           x = as.matrix(train %>% select(-y)),
           y = as.factor(train$y),
          method = "xgbTree",
          trControl  = xgb.trcontrol,
          tuneGrid   = xgb.grid
          #verbose=T,
          #metric="Kappa",
          #nthread =3
)

###############################################################################
