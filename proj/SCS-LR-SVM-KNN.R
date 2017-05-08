
#-----------------------------------------------
# KNN model
#######################

class(y)
y=as.factor(y)

tr_ctrl1 = trainControl(method = "cv", verboseIter = T) 

tn_grid = data.frame(.k=c(4,5))

model_knn = train(x = x22, y, method = "knn",
                  trControl = tr_ctrl1, tuneGrid = tn_grid)


model_knn

model_knn$method  #KNN

model_knn$modelType  #Classification

model_knn$preProcess  #preprocess done or not


model_knn$finalModel #details if KNN(K) classification model (Training set class distribution)

model_knn$control    #Gives the complete details of the Controler used for the KNN

model_knn$resample   #Accuracy In Each fold

model_knn$resampledCM

model_knn$control$index  #  I/P Input data 

model_knn$control$indexOut  #  O/P Input data

model_knn$control$index$Resample1 #Index in values of the test data sample1

model_knn$control$indexOut$Resample1 #Index out of the test data sample1


xx1$var38[model_knn$control$index$Resample1] #Index in values of (variable x22$var38) the train data sample1

xx1$var38[model_knn$control$indexOut$Resample1] #Index out values (variable x22$var38) of the test data sample1



#Lets check the values of "y" used in the first sample, statification is applied or not

y[model_knn$control$index$Resample1]


#Lets check the values of suvived used in the first sample, statification is applied or not
y[model_knn$control$indexOut$Resample1]



#--------------------------------------------------
## Fit logistic regression
##########################################

y = as.factor(y)

data1 = cbind(xx1,y)

data1 = as.data.frame(data1)

head(data1)

data.fit = glm(y ~ ., data = data1, family = binomial)


#prediction
data.pred <- predict.glm(data.fit, type = "response")
head(data.pred)

data.pred = as.factor(ifelse(data.pred > .5, 1,0))


#confusion matrix 
confusionMatrix(data.pred, data1$y)

#-----------------------------------------------------------
# display results
summary(data.fit)
confint(data.fit) # 95% CI for the coefficients
exp(coef(summary(data.fit))) # exponentiated coefficients
exp(confint(data.fit)) # 95% CI for exponentiated coefficients
predict(resLogit, type="response") # predicted values
residuals(data.fit, type="deviance") # residuals 
#------------------------------------------------------------

#Using CARET package to fit logistic regression model to do CV
#--------------------------------------------------------------
#10-fold cross-validation estimation.
#-----------------------------------

tc <- trainControl("cv", 10, savePredictions=T)  #"cv" = cross-validation, 10-fold

fit <- train(y ~ ., method = "glm",data = data1, family = binomial, trControl = tc)

summary(fit)$coef

head(fit$pred)
summary(fit)

#sample fitted values you are looking for are in fit$finalModel$fitted.values



fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

class(x33)
set.seed(2014)

glmBoostModel <- train(y ~ ., data=x33, method = "glmboost", metric="ROC", 
                       trControl = fitControl, tuneLength=5, 
                       center=TRUE, family=Binomial(link = c("logit")))

#Hosmer and Lemeshow goodness of fit (GOF) test(warning : not meaningful for factors)
#=============================================

install.packages("ResourceSelection")

library(ResourceSelection)

hoslem.test(data1$y, fitted(resLogit))

hoslem.test(data1$y, fitted(resLogit), g=10)

#============================================================

summary(data1$predClassLogit)

library(boot)

cv.glm(data1, resLogit,  K = 10)

summary(data1)
count(data1)
dim(data1)

glm.probs = predict(resLogit, type = "response")
length(glm.probs)
glm.probs[1:10]

contrasts(Direction)



#----------------------------------------------------------------
#SVM model-----------

library(foreach) 
library(doParallel)

#register cluster for parallel processing
cl = makeCluster(detectCores())
registerDoParallel(cl)

library(e1071)
library(tictoc)


data.svm = cbind(x22,y)
test.svm = data.svm
test.svm$y = NULL

dim(data.svm)
table(data.svm$y)

class(data.svm$y)
data.svm$y = as.factor(data.svm$y)

tic()

fit1 <- svm(y ~ ., data=data.svm)
summary(fit1)
print(fit1)

toc()

p=predict(fit1,  test.svm, type="class")
plot(p)

table(p,data.svm$y)
mean(p==data.svm$y)



tuned1 = tune.svm(y~., data = data.svm,
                  cost = c(0.001,0.01,0.1,1,10,100))

summary(tuned)



########################################################################


