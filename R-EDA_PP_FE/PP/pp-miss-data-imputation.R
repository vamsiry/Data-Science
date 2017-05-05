

#########################################################################################
##################################################
########### Dealing with missing data ###########
#################################################
#Missing values are represented by capital NA.
#To create a new data without any missing value,

#LIC data set
train <- read.csv("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\kaggle\\life insurance\\train.csv",stringsAsFactors = T) #59,381 observations, 128 variables
dim(train)

df <- na.omit(train) #extract samples without NA 

df1 = train[!complete.cases(train),] #extract rows, having missing values

df2 = train[complete.cases(train),] #extract rows, not having missing values


#checking no.of missing values for each column
data.frame(sort(apply(train, 2, function(x) { sum(is.na(x)) }))) #no.of missing values in each column
data.frame(sort(colSums(is.na(train)))) #no.of missing values in each column


data.frame(rowSums(is.na(train)))  #no.of missing values in each row


sum(colSums(is.na(train))>0) #no.of columnas havig missing values
sum(rowSums(is.na(train))>0)  #no.of rows havig missing values

#----------------------------------
#checking %ge of missing values for each column
pMiss <- function(x){sum(is.na(x))/length(x)*100}
data.frame(sort(apply(train,2,pMiss)))
#----------------------------------

mydata[mydata$Q1==999,"Q1"] <- NA  #Convert a value to missing

table(is.na(train)) #total no.of NA,s and non NA's

which(is.na(train)) #indexes of missing values

which(!is.na(train)) #indexes of non missing values

#below is used to get the values of indices
data[which(!is.na(data$var1))]

# To identify the factor mismatch in Train & Test
#for (n in names(train.imp.com))
#  if (is.factor(train.imp.com[[n]])) {
#      if (length(levels(train.imp.com[[n]])) != length(levels(test.imp.com[[n]]))) {    
#            print(n)     
#    }     
#  }
##-----------------------------------------------------------
#===============================
# mean and/ median imputation
#===============================

# if histogram of var1 look like symmetric use mean imputation bcz that is best represantation of center
data$var1[is.na(data$var1)] = mean(data$var1[!is.na(data$var1)])

# if histogram of var1 look like not symmetric(skewed) use mode imputation bcz that is best represantation of center
data$var1[is.na(data$var1)] = median(data$var1[!is.na(data$var1)])

#need correction
aa = apply(train, 2, function(x) {if(is.numeric(x)) ifelse(is.na(x), median(x,na.rm=T),x) else x})
aa = apply(train, 2, function(x) {if(is.factor(x)) ifelse(is.na(x), mode(x,na.rm=T,x)) else x})
class(aa)

aa = as.data.frame(aa)
table(is.na(aa$Medical_History_10))

class(aa$Medical_History_10)
#-------------------------------------------------------------
#Can we see any different missing data structure depending on the response?
train.na.per.response <- sapply(sort(unique(train$Response)), function(x) { apply(train[train$Response == x, ], 2, function(y) { sum(is.na(y)) }) })
train.na.per.response

round(colSums(train.na.per.response) / sum(train.na.per.response), digits=4)


for (i in which(sapply(train, is.numeric))) {
  train[is.na(train[,i]), i] = median(train[, i],na.rm = TRUE)
}

ncol(is.na(train))

for (i in which(sapply(train, is.factor))) {
  train[is.na(train[,i]), i] <- mode(train[, i])
}

#------------------------------------------------------------
#packages for imputations
library(mi)
library(randomForest)
library(missForest)
library(mice)


#Imputtion using Random forest
#==============================
library(randomForest)
library(dplyr)

imputed.train = na.roughfix(train)

data.frame(apply(imputed.train, 2, function(x) { sum(is.na(x)) })) #checking

# Imputation uing Simple Linear Regression 
#=============================================

# step 1 : find the mosst correlate variable with missing data variable
cor(data,na.omit = T)

symnum(cor(data), use = "complete.obs")

#step 2 : Create indicator varible
ind = function(t)
{
  x = dim(length(t))
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 1
  return(x)
  
}

# or 

data$I = ifelase(is.na(data$var1), 0,1)

#stpe 3 : fitting linear regression model of y on x
lm(y~x,data = data)
summary(lm(y~x,data = data))

#gives y=9.732+1.590*x

#step 4 : imputing missing values

for (i in 1:nrow(data))
{
  if(data$I[i]==0)
  {
    data$y[i] = 9.732+1.590*data$x[i]
  }
}

# finally check imputed data

# Imputation using KNN 
#=========================

library(VIM)

?kNN

data1 = kNN(data, variable = colnames(data),k=6)

data1 = kNN(data, variable = c("colname"),k=6)

data1 = kNN(data)

data1 = subset(data1, select = 1:10)


###################################################################################

library(caret)
library(RANN)
library(mice)

set.seed(100)
df = data.frame(sample(1:100,20), sample(1:100,20), sample(1:100,20))

for (i in 1:20) {
  if (i %% 3 == 0) df[i,1] = NA; 
  if (i %% 5 == 0) df[i,2] = NA;
  if (i %% 10 ==0) df[i,3] = NA;
}

names(df) = c('v1', 'v2', 'v3')

preObj3 = preProcess(df, method = c("center","scale"))
df3=predict(preObj3,df)
df3

#median based imputation
preObj1 = preProcess(df, method = c("center","scale","medianImpute"))
df1=predict(preObj1,df)
df1
median(df$v1,na.rm = TRUE)

#knn based imputation
preObj2 = preProcess(df, method = c("knnImpute") , k = 1)
df2=predict(preObj2,df)
df2

#bagged trees based imputation
preObj4 = preProcess(df, method = c("center", "scale","bagImpute"))
df4=predict(preObj4,df)
df4

#multiple imputation
md.pattern(df)
imputed = mice(df)
complete(imputed)









