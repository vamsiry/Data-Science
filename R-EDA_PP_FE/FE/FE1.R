
#feature-engineering-pca(dimensionality reduction)
#=================================================
x1 = c(10,2,8,9,12)
x2 = c(20,5,17,20,22)
x3 = c(10,2,7,10,11)
x4 = c(25,20,18,7,5)
data = data.frame(x1,x2,x3,x4)

#check the assumption of PCA
cor(data)
cov(data)
sum(diag(cov(data)))

#================================================
#PCA computed using Covariance/correlation matrix
pca = princomp(data, cor=F)
names(pca)
summary(pca)
plot(pca, type="lines")
pca$loadings
pca$scores
sum(diag(cov(pca$scores)))

#=========================
# pca using caret package
library(caret)
preObj = preProcess(data, method=c("pca"), thresh = 1.0)
preObj$rotation
newdata = predict(preObj,data)

#=======================
#PCA computed using SVD
pca = prcomp(data, scale.=TRUE)
names(pca)
summary(pca)
pca$rotation
pca$x
plot(pca, type="lines")



##########################################################################
#creating-new-features.R
#=========================

# function to extract title from names of passengers
extract_title = function(x) {
  title = str_trim(strsplit(x, split='[,.]')[[1]][2])
  if(title %in% c('Mme', 'Mlle') ) 
    return('Mlle')
  else if(title %in% c('Dona', 'Lady', 'the Countess'))
    return('Lady')
  else if(title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer', 'Dr') )
    return('Sir')
  else
    return(title)
}

# Feature engineering of name column
titanic$Title = sapply(titanic$Name, FUN=extract_title)
titanic$Title = factor(titanic$Title)

xtabs(~Survived + Title, data=titanic_train)
ggplot(titanic_train, aes(x = Title, fill = Survived)) + geom_bar(position = "fill")


####### OR ##########

extract_id = function(x) {
  lname = str_trim(strsplit(x, split='[,.]')[[1]][1])
  return(lname)
}

titanic_train1$Surname = sapply(titanic_train$Name, FUN=extract_id)

# Feature engineering of family size column
titanic$FamilySize = titanic$Parch + titanic$SibSp + 1

titanic_train1$FamilySize = titanic_train1$SibSp + titanic_train1$Parch + 1
titanic_train1$FamilyId = paste(titanic_train1$Surname, titanic_train1$FamilySize,sep="")
titanic_train1$FamilyId[titanic_train1$FamilySize <= 3] = "Small"
titanic_train1$FamilyId = factor(titanic_train1$FamilyId)

#------------------------------------------------------------------------
#creating-new-features.R
#========================
library(caret)

setwd("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\datasets\\restaurent-rp")

restaurant_train = read.csv("train.csv", header = TRUE, na.strings=c("NA",""), stringsAsFactors = T)
dim(restaurant_train)
str(restaurant_train)

restaurant_train$Open.Date = as.character(restaurant_train$Open.Date)
#add the missing level to existing levels
levels(restaurant_train$Type) = c(levels(restaurant_train$Type), "MB")

#explore the relation between revenue vs type of restaurant
X11()
ggplot(restaurant_train) + geom_histogram(aes(x = revenue)) + facet_grid(Type ~ .)

#creating a new feature
restaurant_train$num_days = as.numeric(as.Date("31-12-2014", format="%d-%m-%Y") - as.Date(restaurant_train$Open.Date, format= "%m/%d/%Y"))

#filter unwanted features
restaurant_train1 = restaurant_train[,-c(1,2,3,43)]
dim(restaurant_train1)














