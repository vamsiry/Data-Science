#To turn sthg into a tibble, are you supposed to use tibble::as_data_frame()? Or 
#dplyr::as.tbl()? Why isn't it tibble::as_tbl_df()? I guess because data_frame() 
#exists? There are a lot of ways to get confused here.

## fork from Brandon's script
## two features added: number of photos and number of description characters
library(data.table)
library(jsonlite)
library(h2o)
library(lubridate)
h2o.init(nthreads = -1)

install.packages("h2o")

# Load data
t1 <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\train.json")
head(t1,5)
dim(t1)
summary(t1)



# There has to be a better way to do this while getting repeated rows for the "feature" and "photos" columns
t2 <- data.table( bathrooms=unlist(t1$bathrooms)
                 ,bedrooms=unlist(t1$bedrooms)
                 ,latitude=unlist(t1$latitude)
                 ,longitude=unlist(t1$longitude)
                 ,created=as.POSIXct(unlist(t1$created))
                 ,price=unlist(t1$price)
                 ,n_photos = as.numeric(sapply(t1$photos, length))
                 ,features = as.numeric(sapply(t1$features, length))
                 ,n_description = as.numeric(sapply(t1$description, nchar))
               # ,description=unlist(t1$description) # parse errors
               # ,display_address=unlist(t1$display_address) # parse errors
               # ,building_id=as.factor(unlist(t1$building_id))
               # ,manager_id=as.factor(unlist(t1$manager_id))
               # ,listing_id=unlist(t1$listing_id)
               # ,street_adress=unlist(t1$street_address) # parse errors
                 ,interest_level=as.factor(unlist(t1$interest_level))
                 
)
t2[,":="(yday=yday(created)
         ,month=month(created)
         ,mday=mday(created)
         ,wday=wday(created)
         ,hour=hour(created))]


t2$created = NULL
dim(t2)
names(t2)
head(t2)
class(t2)
t2 = as.data.frame(t2)

train <- as.h2o(t2, destination_frame = "train.hex")

varnames <- setdiff(colnames(t2), "interest_level")

gbm1 <- h2o.gbm(x = varnames
                ,y = "interest_level"
                ,training_frame = train
                ,distribution = "multinomial"
                ,model_id = "gbm1"
                #,nfolds = 5
                ,ntrees = 2500
                ,learn_rate = 0.01
                ,max_depth = 7
                ,min_rows = 20
                ,sample_rate = 0.8
                ,col_sample_rate = 0.7
                ,stopping_rounds = 5
                ,stopping_metric = "logloss"
                ,stopping_tolerance = 0
                ,seed=321
)


sapply(t2, length)
####################################################################
y= as.data.frame(as.numeric(t2$interest_level))

x =setdiff(colnames(t2), "interest_level")

require(xgboost)
require(Matrix)
train <- sparse.model.matrix(y ~ ., data = t2)

class(t2)
class(y)

dtrain <- xgb.DMatrix(data=t2,label=y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "multi:softprob", 
                booster             = "gbtree",
                eval_metric         = "merror",
                eval.metric         = "mlogloss",
                eta                 = 0.01,#1 #learning rate - Number of Trees
                max_depth           = 7, # maximum depth of a tree
                subsample           = 0.8, # subsample ratio of the training instance
                colsample_bytree    = 0.7, # subsample ratio of columns
                min_child_weight    = 1, # minimum sum of instance weight (defualt)
                scale_pos_weight    = 1, # helps convergance bc dataset is unbalanced
                  print.every.n       = 1
                #early.stop.round   = 10 # stop if no improvement within 10 trees
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 1000,
                    print.every.n       =  10,
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = TRUE
                    
)

dtrain <- xgb.DMatrix(data=t1_sparse, label=labels)
dtest <- xgb.DMatrix(data=s1_sparse)

param <- list(booster="gbtree",
              objective="multi:softprob",
              eval_metric="mlogloss",
              nthread=13,
              num_class=3,
              eta = .02,
              gamma = 1,
              max_depth = 4,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .5
)
dtrain <- xgb.DMatrix(data=t1_sparse, label=labels)
dtest <- xgb.DMatrix(data=s1_sparse)

param <- list(booster="gbtree",
              objective="multi:softprob",
              eval_metric="mlogloss",
              nthread=13,
              num_class=3,
              eta = .02,
              gamma = 1,
              max_depth = 4,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .5
)

#set.seed(201609)
#(tme <- Sys.time())
#xgb2cv <- xgb.cv(data = dtrain,
#                 params = param,
#                 nrounds = 50000,
#                 maximize=FALSE,
#                 prediction = TRUE,
#                 folds = cvFoldsList,
#                 # nfold = 5,
#                 print_every_n = 50,
#                 early_stopping_round=300)
#Sys.time() - tme
#watch <- list(dtrain=dtrain)
xgb2 <- xgb.train(data = dtrain,
                  params = param,
                  # watchlist=watch,
                  # nrounds = xgb2cv$best_ntreelimit
                  nrounds = 2710
)

############################################################################
# Load data
s1 <- fromJSON("../input/test.json")
# There has to be a better way to do this while getting repeated rows for the "feature" and "photos" columns
s2 <- data.table(bathrooms=unlist(s1$bathrooms)
                 ,bedrooms=unlist(s1$bedrooms)
                 ,building_id=as.factor(unlist(s1$building_id))
                 ,created=as.factor(unlist(s1$created))
                 ,n_photos = as.numeric(sapply(s1$photos, length))
                 ,n_description = as.numeric(sapply(s1$description, nchar))
                 # ,description=unlist(s1$description) # parse errors
                 # ,display_address=unlist(s1$display_address) # parse errors
                 ,latitude=unlist(s1$latitude)
                 ,longitude=unlist(s1$longitude)
                 ,listing_id=unlist(s1$listing_id)
                 ,manager_id=as.factor(unlist(s1$manager_id))
                 ,price=unlist(s1$price)
                 # ,street_adress=unlist(s1$street_address) # parse errors
)
s2[,":="(yday=yday(created)
         ,month=month(created)
         ,mday=mday(created)
         ,wday=wday(created)
         ,hour=hour(created))]
test <- as.h2o(s2[,-"created"], destination_frame = "test.hex")

preds <- as.data.table(h2o.predict(gbm1, test))

testPreds <- data.table(listing_id = unlist(s1$listing_id), preds[,.(high, medium, low)])
fwrite(testPreds, "submission.csv")

#################################################################################
#version-2-----------------

knitr::opts_chunk$set(echo = TRUE)
my.install <- function(pkg) {
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (require(pkg,character.only=TRUE))
}

library("jsonlite")
library("dplyr")
library("purrr")
library("xgboost")


#Step 1: Load the data
# Train data
train_data <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\train.json")
vars <- setdiff(names(train_data), c("photos", "features"))
train_data <- map_at(train_data, vars, unlist) %>% tibble::as_tibble(.)

class(train_data)
str(train_data)
summary(train_data)


# Test data
test_data <- fromJSON("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\2sigma\\test.json")
vars <- setdiff(names(test_data), c("photos", "features"))
test_data <- map_at(test_data, vars, unlist) %>% tibble::as_tibble(.)

names(train_data)
head(train_data$listing_id)

#Step 2: Preprocess data

word_features = c("building_id", "created", "description", "display_address", 
                  "street_address", "features", "listing_id", "manager_id", "photos")

# Remove wordy features out of the dataset
processed_train = train_data
processed_train[word_features] = NULL

names(processed_train)

processed_test = test_data
processed_test[word_features] = NULL

# Create processed X and processed Y
train_X = processed_train
train_X$interest_level = NULL
train_y = processed_train$interest_level
train_y[train_y == "low"] = 0
train_y[train_y == "medium"] = 1
train_y[train_y == "high"] = 2

# Create processed X and processed Y
test_X = processed_test

summary(train_X)

#Step 3: XGBoost prediction
set.seed(100)
pmt = proc.time()
model = xgboost(data = as.matrix(train_X), 
                label = train_y,
                eta = 0.8,
                max_depth = 11, 
                nround=1500, 
                subsample = 0.7,
                colsample_bytree = 0.7,
                seed = 100,
                eval_metric = "merror",
                eval_metric = "mlogloss",
                objective = "multi:softprob",
                num_class = 3,
                missing = NaN,
                silent = 1)
show(proc.time() - pmt)


pred = predict(model,  as.matrix(test_X), missing=NaN)
pred_matrix = matrix(pred, nrow = nrow(test_data), byrow = TRUE)
pred_submission = cbind(test_data$listing_id, pred_matrix)
colnames(pred_submission) = c("listing_id", "low", "medium", "high")
pred_df = as.data.frame(pred_submission)
write.csv(pred_df, "second_submission.csv", row.names = FALSE)






