
#convert character into integer
#================================
features=names(train)

for(f in features){
  if(class(train_test[[f]])=="character"){
    levels=sort(unique(train_test[[f]]))
    train_test[[f]]=as.integer(factor(train_test[[f]],levels = levels))
  }
}


#Basic descriptive stats about data
#===================================
require(psych)
describe(train,na.rm = TRUE)

# target Class Distribution (proportation or %ge )
#=================================================
# data normally distributed or not(for binar class)

y <- PimaIndiansDiabetes$diabetes
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#to check the cumulative frequency distribution of a categorical variable
#------------------------------------------------------------------------
t = data.frame(table(y))

t$cumfreq = cumsum(t$Freq)

t$cumpercent= round(t$cumfreq / sum(t$Freq)*100,2)
t

##################################################
########### checking Skewness of data ###########
#################################################
require(e1071)
names(mtcars)
tune(lm, mpg~., data = mtcars)
skewness(c(y), na.rm = FALSE, type = 3)

x = iris[,-5]
apply(x,2,skewness)


pairs(iris[,-5])
plot(iris[,-5])
model <- svm(Species ~ ., data = iris,gamma = 1)

#a. Get a boxplot for each numerical column of the 'iris' dataset (four boxplots).
head(iris)
apply(iris[,1:4], 2, boxplot)

apply(iris[,1:4], 2, hist)
apply(iris[,1:4], 2, plot)

# Get one violin box plot for each numeric column,
install.packages("vioplot")
library(vioplot)
apply(iris[,1:4], 2, vioplot, col = "salmon", names = "")


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


data.frame(sort(colSums(is.na(train)))) #no.of missing values in each column

data.frame(rowSums(is.na(train)))  #no.of missing values in each row

sum(colSums(is.na(train))>0) #no.of columnas havig missing values
sum(rowSums(is.na(train))>0)  #no.of rows havig missing values

#----------------------------------
#checking no.of missing values for each column
data.frame(sort(apply(train, 2, function(x) { sum(is.na(x)) })))

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
apply(train$Medical_History_10, 2, function(x) {if(is.numeric(x)) ifelse(is.na(x), median(x,na.rm=T),x) else x})
  
is.na(train$Medical_History_10)

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
library(randomForest)
library(dplyr)

imputed.train = na.roughfix(train)

data.frame(apply(imputed.train, 2, function(x) { sum(is.na(x)) })) #checking

#ackages for imputations
library(mi)
library(randomForest)
library(missForest)
library(mice)
#============================================
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

#=================
# KNN imputation
#=================

library(VIM)

?kNN

data1 = kNN(data, variable = colnames(data),k=6)

data1 = kNN(data, variable = c("colname"),k=6)

data1 = kNN(data)

data1 = subset(data1, select = 1:10)


###################################################################################

##################################################
#### Correlation or multicollinearity ###########
#################################################
install.packages("combinat")
require(combinat)
permn(names(train))
combn(names(train),4)

#===============================
#Correcting Collinearity with Correlation Matrix in R 
#The absolute values of pair-wise correlations are considered.
#If some variables have a high correlation, the function looks at the mean 
#absolute correlation of each variable and keeps only the variable with the 
#smallest mean absolute correlation and remove the larger absolute correlation.

# Identifying numeric variables
numericData <- dat2[sapply(dat2, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

#================================================================
# find and remove vectors that are highly corrolated to other vectors
#searches through corrlation matrix and returns a vector of integers 
#correxponding to columns to remove to reduce pairwise correlation

HIGHCOR <- findCorrelation(cor(TRAIN[,1:ncol(TRAIN)]), cutoff = .95, verbose = FALSE)
dim(HIGHCOR)

TRAIN <- TRAIN[,-HIGHCOR]


#=====================================
# most correlate variable with  data 
cor(data,na.omit = T)
symnum(cor(data), use = "complete.obs")


#=======================================================================
# find and remove vectors that are linear combinations of other vectors
#QR decomposition is used to determine if the matrix is full rank and then identify
#the sets of columns that are involved in the dependencies. 

#To "resolve" them, columns are iteratively removed and the matrix rank is rechecked.
#The trim.matrix function in the subselect package can also be used to accomplish the same goal.

LINCOMB <- findLinearCombos(x)
head(LINCOMB)

TRAIN <- x[, -LINCOMB$remove]
dim(TRAIN)


#========================================
#use vif to find correation amoung more than one variable
library(usdm)
train11 = vif(TRAIN)

train11 = as.data.frame(vif(TRAIN))

train11$Variables1 <- row.names(train11$Variables)
train11[order(train11$VIF,decreasing = T), ]




###################################################################################

#====================================
########## Removing NearZerovar col
x=db_train[,-db_train$TARGET]

x22 = x[,-nearZeroVar(x)]

str(x22)


#-------------------------------------------------
##### Removing constant features(same NearZerovar in diff way)
cat("\n## Removing the constants features.\n")
for (f in names(x22)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    x22[[f]] <- NULL
    
  }
}


#--------------------------------------
##### Removing identical features
features_pair <- combn(names(x22), 2, simplify = F)

toRemove <- c()

for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(x22[[f1]] == x22[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}


###################################################################################

#================================================
##PCA computed using Covariance/correlation matrix
pca1 = princomp(x22, cor=F)
names(pca1)
summary(pca1)
plot(pca1, type="lines")
pca$loadings
pca$scores
sum(diag(cov(pca1$scores)))

#==========================
#PCA computed using SVD
pca2 = prcomp(x22, scale.=TRUE)
names(pca2)
summary(pca2)
pca2$rotation
pca2$x
plot(pca2, type="lines")

xx1 = pca2$x[,1:23]
dim(xx1)



#========================
# pca using caret package
library(caret)
preObj = preProcess(x22, method=c("pca"), thresh = 1.0)
preObj$rotation
newdata = predict(preObj,x22)
dim(newdata)

x33 = newdata


#########################################################################

#============================================
###### makin over sampling and under sampling

train.x = cbind(x33, y)

dim(train.x)

all.1s = train.x[train.x$y == "1", ]
dim(all.1s)

all.0s = train.x[train.x$y=="0",]
dim(all.0s)








