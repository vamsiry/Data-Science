require(data.table)
require(magrittr)
library(caret)
require(dplyr)
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
train$target = as.factor(train$target)


trainIndex <- createDataPartition(train$target, p = .8,list = FALSE,times = 1)

train1 <- train[ trainIndex,]
test1  <- train[-trainIndex,]

dim(train1)
dim(test1)

y = train1$target
y1 = test1$target

train1$target = NULL
test1$target = NULL


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }


train1 <- as.data.frame(lapply(train1, normalize))
test1 = as.data.frame(lapply(test1,normalize))


library(class)

bstm <- knn(train = train1, test = test1, cl = y, k=99)

install.packages("gmodels")
library(gmodels)
CrossTableble(x = y1, y = bstm, prop.chisq=FALSE)

table(y1,bstm)

mean(y1 == bstm)


###################################################################
require(caret)


#tr_ctrl1 = trainControl(method = "cv", verboseIter = T) 
tn_grid = data.frame(.k=c(4,5))

model_knn = train(x = train, y, method = "knn",trControl = tr_ctrl1, tuneGrid = tn_grid)
            
model_knn



digit_test2 = predict(pca_obj,digit_test1)
dim(digit_test2)

digit_test2$label = predict(model_knn, digit_test2)
digit_test2$ImageId = 1:nrow(digit_test2)
submission = digit_test2[,c("ImageId","label")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")


y = train$target

library(foreach) 
library(doParallel)

registerDoParallel(cores=2)

set.seed(400)
#ctrl <- trainControl(method="cv") #,classProbs=TRUE,summaryFunction = twoClassSummary) # trControl = ctrl,
knnFit <- train(target ~ ., data = train, method = "knn", preProcess = c("center","scale"), tuneLength = 10)

stopCluster()

#####################################################################################

km.out =kmeans (train1, 9, nstart =20)
km.out$cluster #cluster assignments of the 50 observations
table(km.out$cluster)


mean(km.out$cluster == yyy)

table(km.out$cluster, yy)

head(train$target)

yy = train$target

yyy = gsub("Class_","",train$target)
table(yyy)


set.seed(20)
irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
irisCluster

##############################################################################
train11 = read.csv('train.csv', header = T, stringsAsFactors = F)

dim(train)


train11$id = NULL


table(train11$target)

train11$target = gsub("Class_","",train11$target) 

table(train11$target)
class(train11$target)

train11$target = as.factor(train11$target)

dim(train11)

clusters <- hclust(dist(train[,1:93]))

plot(clusters)
clusterCut <- cutree(clusters, 3)

table(clusterCut, iris$Species)

clusters <- hclust(dist(train[,1:93]), method = 'centroid')
plot(clusters)















