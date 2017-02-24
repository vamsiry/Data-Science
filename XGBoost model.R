require(xgboost)
require(methods)
require(data.table)
require(magrittr)
library(caret)

setwd("C://Users//Vamsi//Desktop//R.Alg//practice//kaggle//ottto data set")

## Loading required package: magrittr
train <- fread('train.csv', header = T, stringsAsFactors = F)
test <- fread('test.csv', header=TRUE, stringsAsFactors = F)

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


trainIndex <- createDataPartition(train$target, p = .8,list = FALSE,times = 1)

otto_train <- train[ trainIndex,]
otto_test  <- train[-trainIndex,]

dim(otto_train)
dim(otto_test)

y = otto_train$target
y1 = otto_test$target

otto_train$target = NULL
otto_test$target = NULL

library(Matrix)

train1 <- sparse.model.matrix(y ~ ., data = otto_train)
test1 = sparse.model.matrix(y1~.,data = otto_test)

dtrain <- xgb.DMatrix(data=train1, label=y)
dtest = xgb.DMatrix(data = test1, label = y1)


watchlist <- list(train=dtrain, test = dtest)

param <- list(  objective           = "multi:softprob", 
                booster             = "gbtree",
                num_class           = 9,
                eval.metric         = "merror",
                eval.metric         = "mlogloss",
                eta                 = 0.8, #0.0202048,#1 #learning rate - Number of Trees
                max_depth           = 6, # maximum depth of a tree
                subsample           = 0.70,#0.6815, # subsample ratio of the training instance
                colsample_bytree    = 0.70,#0.701, # subsample ratio of columns
                min_child_weight    = 1, # minimum sum of instance weight (defualt)
                scale_pos_weight    = 1, # helps convergance bc dataset is unbalanced
                print.every.n       = 1,
                #early.stop.round   = 10 # stop if no improvement within 10 trees
                lamda = 0.5,
                alpha = .5
)


clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 200,
                    print.every.n       = 1,
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = TRUE
                    
)


#========================================================================== 
# cross-validate xgboost to get the accurate measure of error
xgb_cv = xgb.cv(params = param,
                data = dtrain,
                #data = as.matrix(train.1)
                #label = train.1$y,
                nrounds = 250, 
                nfold = 10,         # number of folds in K-fold
                prediction = TRUE,  # return the prediction using the final model 
                #showsd = TRUE,      # standard deviation of loss across folds
                stratified = TRUE,  # sample is unbalanced; use stratified sampling
                verbose = TRUE,
                print.every.n = 10, 
                early.stop.round = 10
)


##=========================================================================

train1 = read.csv('train.csv', header = T, stringsAsFactors = F)
test1 <- read.csv('test.csv', header=TRUE, stringsAsFactors = F)

train1$id = NULL
test1$id = NULL

table(train1$target)

train1$target = gsub("Class_","",train1$target) %>% {as.integer(.) -1}

table(train1$target)
class(train1$target)

y = train1$target


library(Matrix)

train11 <- sparse.model.matrix(y ~ ., data = train1)

dtrain1 <- xgb.DMatrix(data=train11, label=y)


bst = xgboost(param=param, data = dtrain, label = y, nrounds=70)


# Make prediction

test11 = sparse.model.matrix(~.,data = test1)
dtest <- xgb.DMatrix(data=test11)


pred = predict(bst, dtest)

pred1 = matrix(pred,9,length(pred)/9)
pred1 = t(pred1)

pred1 = format(pred1, digits=2,scientific=F) # shrink the size of submission

class(pred1)


pred1 = data.frame(1:nrow(pred1),pred1)

names(pred1) = c('id', paste0('Class_',1:9))
write.csv(pred1, file='submission.csv', quote=FALSE,row.names=FALSE)


#-------------------------------------------------------------------

#4 Model understanding
#4.1 Feature importance

model <- xgb.dump(bst, with.stats = T)
model[1:10]


#Hopefully, XGBoost offers a better representation: feature importance.
#Feature importance is about averaging the gain of each feature for all split and all trees.
#Then we can use the function xgb.plot.importance.

# Get the feature real names
names <- dimnames(dtrain)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:50,])

#Tree graph
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)



