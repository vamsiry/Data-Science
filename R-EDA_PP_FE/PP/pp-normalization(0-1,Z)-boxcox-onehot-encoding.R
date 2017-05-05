
#standardize the numeric features
#===================================
titanic_train_numeric = titanic_train[sapply(titanic_train, is.numeric)]
preObj = preProcess(titanic_train_numeric, method=c("BoxCox","center","scale","medianImpute"))
titanic_train_std = predict(preObj,titanic_train_numeric)
titanic_train_all = data.frame(titanic_train[c("Pclass","Sex","Embarked","Survived")], titanic_train_std)

####################################################################################

#normalization.R
#================
library(caret)

# Age vector
age = c(25, 35, 50, 40, 60, 25, 35, 50, 80, 100)

# Salary vector
salary = c(200000, 120000, 100000, 300000, 250000, 200000, 100000, 500000, 300000, 350000)


# Data frame created using age and salary
df = data.frame( age, salary)
dim(df)
str(df)
summary(df)


min_max_normalize = function(x,new_min=0, new_max=1) {
  new_min + (x - min(x)) * (new_max - new_min) / (max(x) - min(x));
}


z_score_normalize = function(x) {
  (x - mean(x)) / sd(x)
}


x11()
hist(df$age, col="lightblue")

#min-max normalization using custom function
df1 = as.data.frame(apply(df, 2, min_max_normalize))
hist(df1$age, col="lightblue")

#zero-one normalization using preProcess
preObj = preProcess(df, method=c("range"))
preObj$ranges
df2 = predict(preObj,df)
hist(df2$age, col="lightblue")

#z-score normalization using custom function
df3 = as.data.frame(apply(df, 2, z_score_normalize))
hist(df3$age, col="lightblue")

#z-score normalization using preProcess
preObj = preProcess(df, method=c("center","scale"))
preObj$mean
preObj$std
df4 = predict(preObj,df)
hist(df4$age, col="lightblue")


#data-preparation-boxcox.R
#============================
library(caret)
library(e1071)
setwd("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\datasets")

winedata = read.csv("wine.txt", header = TRUE)
dim(winedata)
str(winedata)
head(winedata)
names(winedata) = c('Label','Alcohol','MalicAcid')

summary(winedata)

hist(winedata$Alcohol,col="lightblue")
skewness(winedata$Alcohol)
hist(winedata$MalicAcid,col="lightblue")
skewness(winedata$MalicAcid)

preObj = preProcess(winedata[2:3], method=c("BoxCox"))
preObj$bc
winedata1 = predict(preObj,winedata[2:3])
hist(winedata1$Alcohol,col="lightblue")
skewness(winedata1$Alcohol)

hist(winedata1$MalicAcid,col="lightblue")
skewness(winedata1$MalicAcid)

###############################################################################
#Dummy var creation
#===================
age = c(10,20,30)
gender = factor(c("M","F","M"))
pclass = factor(c("1","2","3"))
passengers = data.frame(age, gender,pclass)
str(passengers)

tmp = dummyVars(~age+gender+pclass, passengers)
predict(tmp, passengers)

#=============================
library(caret)
library(car)

v1 = c("male","female","male","female")
v1_f = factor(v1)

v2 = c("1","2","3","-1")
v2_f = factor(v2)
v2_f=recode(v2_f,"'1' = 'a'; '2'= 'b'; '3'= 'c' ")

v3 = c("low","medium","high","low")
v3_f = factor(v3,ordered = TRUE)
v3_f = factor(v3,levels = c("low","medium","high"),ordered = TRUE)

df = data.frame(v1_f,v2_f,v3_f)
names(df) = c("gender","category","rating")

dummyObj = dummyVars(~gender + category + rating,df,fullRank = FALSE)
predict(dummyObj,df)

dummyObj = dummyVars(~gender + category + rating,df,fullRank = TRUE)
predict(dummyObj,df)

dummyObj = dummyVars(~gender + category + rating,df)
predict(dummyObj,df)








