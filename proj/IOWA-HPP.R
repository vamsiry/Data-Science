require(data.table)
require(readr)

library(data.table)
library(xgboost)
library(Metrics)
library(Matrix)
library(mice)
library(dplyr)


setwd("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\iowa h.p.p")

train = fread("train.csv")
test = fread(("test.csv"))

y_train=train$SalePrice

#Remove Id since of no use
train$Id=NULL
test$Id=NULL
train$SalePrice=NULL

#############################################
x = train[, sapply(train, is.factor)]
str(x)
names(x)
###########################################

#Row binding train & test set for feature engineering
train_test = rbind(train, test)
ntrain=nrow(train)

features=names(train)
#convert character into integer
for(f in features){
  if(class(train_test[[f]])=="character"){
    levels=sort(unique(train_test[[f]]))
    train_test[[f]]=as.integer(factor(train_test[[f]],levels = levels))
  }
}

dim(train_test)


#splitting whole data back again
train_x=train_test[1:ntrain,]
test_x=train_test[(ntrain+1):nrow(train_test),]

str(train_x)
library(mice)

#missing values imputation with mice
set.seed(144)
to_impute<-as.data.frame(test_x)
impute=to_impute[c("MSZoning","Exterior1st","Exterior2nd","BsmtFinSF1",
                   "BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath","BsmtHalfBath",
                   "KitchenQual","Functional","GarageCars","GarageArea","SaleType")]
imputed=complete(mice(train_x,m=2))

data.frame(sort(colSums(is.na(train_x)))) #no.of missing values in each column


data.frame(rowSums(is.na(train_x)))  #no.of missing values in each row



to_impute$MSZoning=imputed$MSZoning
to_impute$Utilities=imputed$Utilities
to_impute$Exterior1st=imputed$Exterior1st
to_impute$Exterior2nd=imputed$Exterior2nd
to_impute$BsmtFinSF1=imputed$BsmtFinSF1
to_impute$BsmtFinSF2=imputed$BsmtFinSF2
to_impute$BsmtUnfSF=imputed$BsmtUnfSF
to_impute$TotalBsmtSF=imputed$TotalBsmtSF
to_impute$BsmtHalfBath=imputed$BsmtHalfBath
to_impute$BsmtFullBath=imputed$BsmtFullBath
to_impute$KitchenQual=imputed$KitchenQual
to_impute$Functional=imputed$Functional
to_impute$GarageCars=imputed$GarageCars
to_impute$GarageArea=imputed$GarageArea
to_impute$SaleType=imputed$SaleType

test_x=as.data.table(to_impute)









###############################################################
library(data.table)
library(xgboost)
library(Metrics)
library(Matrix)
library(mice)
library(dplyr)

setwd("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\iowa h.p.p\\train.csv")

TRAIN = "C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\iowa h.p.p\\train.csv"

TEST = "C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\iowa h.p.p\\test.csv"

#load data
train=fread(TRAIN,showProgress = T)
test=fread(TEST,showProgress = T)
y_train=train$SalePrice

#Remove Id since of no use
train$Id=NULL
train$SalePrice=NULL
test$Id=NULL

#Row binding train & test set for feature engineering
train_test = rbind(train, test)
ntrain=nrow(train)

features=names(train)

#convert character into integer
for(f in features){
  if(class(train_test[[f]])=="character"){
    levels=sort(unique(train_test[[f]]))
    train_test[[f]]=as.integer(factor(train_test[[f]],levels = levels))
  }
}

#feature to exclude
features_to_drop<-c("Utilities","LotFrontage","Alley","MasVnrType","MasVnrArea","BsmtQual",
                    "BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
                    "Electrical","FireplaceQu","GarageType","GarageYrBlt",
                    "GarageFinish","GarageQual","GarageCond","PoolQC",
                    "Fence","MiscFeature")









require(xgboost)
#convert into numeric for XGBoost implementation

train_x[] <- lapply(train_x, as.numeric)
test_x[]<-lapply(test_x, as.numeric)

dtrain=xgb.DMatrix(as.matrix(train_x),label= y_train)
dtest=xgb.DMatrix(as.matrix(test_x))

#xgboost parameters
xgb_params = list(
  seed = 0,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.02, 
  objective = 'reg:linear',
  max_depth = 12,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

best_n_rounds=150 # try more rounds

#train data
gb_dt=xgb.train(xgb_params,dtrain,nrounds = as.integer(best_n_rounds))
submission=fread(SUBMISSION,colClasses = c("integer","numeric"))
submission$SalePrice=predict(gb_dt,dtest)
write.csv(submission,"xgb.csv",row.names = FALSE)





