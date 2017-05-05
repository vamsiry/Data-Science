#Bivariate stats(numeric-numeric)(covariance and correlation)to know data is normally distributed or not......

library(caret)
set.seed(50)
x=sample(1:100,20)
y=sample(1:100,20)
x
y
#1...............
cov(x,y)
cor(x,y)

x=x*30
cov(x,y)
y=y*30
cov(x,y)
cov(x,y)/900
cor(x,y)

#1.1 scaling(removing bias) wil impact covariance and correlation or not...........

x=as.data.frame(x)
preobj1 = preProcess(x,method=c("scale"))
x1=predict(preobj1,x)
y=as.data.frame(y)
preobj2=preProcess(y,method = "scale")
y1=predict(preobj2,y)
cov(x1,y1)
cor(x1,y1)
x1;y1

#1.2 range(0-1) wil impact covariance and correlation or not...........

x=as.data.frame(x)
preobj1 = preProcess(x,method=c("range"))
x1=predict(preobj1,x)
y=as.data.frame(y)
preobj2=preProcess(y,method = "range")
y1=predict(preobj2,y)
cov(x1,y1)
cor(x1,y1)

x1;y1

#2 centring wil impact covariance and correlation or not...........
x=as.data.frame(x)
preobj1 = preProcess(x,method=c("center"))
x1=predict(preobj1,x)
y=as.data.frame(y)
preobj2=preProcess(y,method = "center")
y1=predict(preobj2,y)
x1
y1
cov(x1,y1)

#2.1 z score(-3:+3) wil impact covariance and correlation or not...........
x=as.data.frame(x)
preobj1 = preProcess(x,method=c("center","scale"))
x1=predict(preobj1,x)
y=as.data.frame(y)
preobj2=preProcess(y,method = c("center","scale"))
y1=predict(preobj2,y)
cov(x1,y1)
cor(x1,y1)

#Bivariate stats(factor-factor)(x^2 chi-square test) to know data normally distributed or not.......
#chi-square test11111111111111111111111111111111111111111111111.....

# coin fairness test
outcomes1=c(60,40)
chisq.test(outcomes1)

outcomes2=c(50,50)
chisq.test(outcomes2)

outcomes3=c(55,45)
chisq.test(outcomes3)

outcomes4=c(80,20)
chisq.test(outcomes4)

outcomes5=c(30,40,60,70)
chisq.test(outcomes5)
chisq.test

# fairness of dice
dice_outcomes1 = c(10,15,20,30,40,5)
chisq.test(dice_outcomes1)

dice_outcomes2 = c(20,20,20,15,30,15)
chisq.test(dice_outcomes2)

dice_outcomes3 = c(20,20,20,20,20,20)
chisq.test(dice_outcomes3)


#chi-square test22222222222222222222222222222222222.....
# checking the depedance between type of handed-ness and gender

# left handed and right handed persons among male and female
left_handed1 = c(12,7)
right_handed1 = c(108,133)
df1 = data.frame(left_handed1, right_handed1)
names(df1) = c("male","female")
df1
chisq.test(df1)

left_handed2 = c(15,50)
right_handed2 = c(105,90)
df2 = data.frame(left_handed2, right_handed2)
names(df2) = c("male","female")
df2
chisq.test(df2)

# checking the dependance between survived and pclass/embarked factor variables
setwd("D:\\VAMSI ALL\\R\\data sets\\titanic\\data")
titanic_train = read.table("train.csv", TRUE, ",")
dim(titanic_train)
str(titanic_train)

titanic_train$Survived = as.factor(titanic_train$Survived)
titanic_train$Pclass = as.factor(titanic_train$Pclass)
titanic_train$Embarked = as.factor(titanic_train$Embarked)
chisq.test(titanic_train$Survived, titanic_train$Pclass)
chisq.test(titanic_train$Embarked,titanic_train$Survived)

#bivariate state 33333333333333333333333333333333333333.....

library(caret)
setwd("D:\\VAMSI ALL\\R\\data sets\\titanic\\data")

titanic = read.csv("train.csv", header = TRUE, na.strings=c("NA",""))
dim(titanic)
str(titanic)

plot(titanic$SibSp, titanic$Parch)
cov(titanic$SibSp, titanic$Parch)
cor(titanic$SibSp, titanic$Parch)

plot(titanic$Fare, titanic$Parch)
cov(titanic$Fare, titanic$Parch)
cor(titanic$Fare, titanic$Parch)

# multivariate states44444444444444444444444444444444444444
setwd("E:/data analytics/datasets")

winedata = read.csv("wine.data", header = TRUE)
dim(winedata)
str(winedata)
head(winedata)

pairs(~X2.8+X3.06,data=winedata)
cov_matrix = cov(winedata)
cor_matrix = cor(winedata)

#univariate stats555555555555555555555555555555555555555555555
library(caret)
setwd("D:\\VAMSI ALL\\R\\data sets\\titanic\\data")

titanic = read.csv("train.csv", header = TRUE, na.strings=c("NA",""))
dim(titanic)
str(titanic)
class(titanic)

names(titanic)
head(titanic)
head(titanic)
tail(titanic)

summary(titanic)
summary(titanic$Sex)
summary(titanic$Age)



