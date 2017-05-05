#EDA - NUMERICAL
#===============

#univariate-stats.R
#===================

library(caret)
setwd("C:\\Users\\Thimma Reddy\\Documents\\GitHub\\datascience\\2014\\kaggle\\titanic\\data")

titanic = read.csv("train.csv", header = TRUE, na.strings=c("NA",""))
dim(titanic)
str(titanic)
class(titanic)

names(titanic)
head(titanic)
head(titanic)
tail(titanic)

sd(students$Height)
mad(students$Height)
IQR(students$Height)
mean(students$MilesHome)

summary(titanic)
summary(titanic$Sex)
summary(titanic$Age)

#bivariate-stats.R
#===================
library(caret)
setwd("E:/data analytics/kaggle/titanic/data")

titanic = read.csv("train.csv", header = TRUE, na.strings=c("NA",""))
dim(titanic)
str(titanic)

plot(titanic$SibSp, titanic$Parch)
cov(titanic$SibSp, titanic$Parch)
cor(titanic$SibSp, titanic$Parch)

plot(titanic$Fare, titanic$Parch)
cov(titanic$Fare, titanic$Parch)
cor(titanic$Fare, titanic$Parch)

#multivariate-stats-cor-cov.R
###############################
setwd("E:/data analytics/datasets")

winedata = read.csv("wine.data", header = TRUE)
dim(winedata)
str(winedata)
head(winedata)

cov_matrix = cov(winedata)
cor_matrix = cor(winedata)

############################################################################################
#bivariate-chisquare1.R
#===========================
# coin fairness test

# 1.checking fairness of coin 
# observed data for 100 coin tosses
outcomes1=c(60,40)
chisq.test(outcomes1)

outcomes2=c(50,50)
chisq.test(outcomes2)

outcomes3=c(55,45)
chisq.test(outcomes3)

outcomes4=c(80,20)
chisq.test(outcomes4)


# 2.checking fairness of dice
# observed data for 120 dice throws
dice_outcomes1 = c(10,15,20,30,40,5)
chisq.test(dice_outcomes1)

dice_outcomes2 = c(20,20,20,15,30,15)
chisq.test(dice_outcomes2)

dice_outcomes3 = c(20,20,20,20,20,20)
chisq.test(dice_outcomes3)


#bivariate-chisquare2.R
#=======================
# 3.checking the depedance between type of handed-ness and gender
# observed data for left handed and right handed persons among male and female(260 people)
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
setwd("C:\\Users\\Thimma Reddy\\Documents\\GitHub\\datascience\\2014\\kaggle\\titanic\\data")
titanic_train = read.table("train.csv", TRUE, ",")
dim(titanic_train)
str(titanic_train)

titanic_train$Survived = as.factor(titanic_train$Survived)
titanic_train$Pclass = as.factor(titanic_train$Pclass)
chisq.test(titanic_train$Survived, titanic_train$Pclass)
chisq.test(titanic_train$Survived, titanic_train$Embarked)

#cov-cor
#=========

#revenue-prediction-EDA
#=======================

library(ggplot2)
library(rpart)
library(caret)
library(corrplot)
library(reshape2)
library(Amelia)

setwd("C:/Users/Thimma Reddy/Documents/GitHub/datascience/datasets/restaurant-revenue")

restaurant_train = read.csv("train.csv", na.strings=c("","NA"))
restaurant_test = read.csv("test.csv", na.strings=c("","NA"))

#combining train and test datasets for handling factor type differences
restaurant_test$revenue = NA
restaurant = rbind(restaurant_train, restaurant_test)
dim(restaurant)
str(restaurant)

restaurant_train = restaurant[1:137,]
dim(restaurant_train)
str(restaurant_train)

##EDA
#Exploring numerical summaries
summary(restaurant_train)

#Exploring data relationships
X11()
ggplot(restaurant_train) + geom_histogram(aes(x = revenue), fill = "white", colour = "black")

X11()
ggplot(restaurant_train, aes(x=City.Group, y=revenue)) +geom_point(shape=1)
   

X11()
ggplot(restaurant_train, aes(x=Type, y=revenue)) + geom_point(shape=1)
   

X11()
featurePlot(restaurant_train[,c('P1','P2','P3','P4','P5','P6','P7','P8','P9','P10','P11','P12')],
            restaurant_train$revenue,
            plot="scatter",
            type = c("g", "p", "smooth"),
            between = list(x = 1, y = 1),
            labels = rep("", 2))
X11()
featurePlot(restaurant_train[,c('P13','P14','P15','P16','P17','P18','P19', 'P20','P21','P22','P23','P24')],
            restaurant_train$revenue,
            plot="scatter",
            type = c("g", "p", "smooth"),
            between = list(x = 1, y = 1),
            labels = rep("", 2))
X11()
featurePlot(restaurant_train[,c('P25','P26','P27','P28','P29','P30', 'P31','P32','P33','P34','P35','P36', 'P37')],
            restaurant_train$revenue,
            plot="scatter",
            type = c("g", "p", "smooth"),
            between = list(x = 1, y = 1),
            labels = rep("", 2))

#Exploring correlations among features
numeric_attr = sapply(restaurant_train, is.numeric)
correlations = cor(restaurant_train[,numeric_attr])
X11()
corrplot(correlations)
corrplot(correlations, order = "hclust")
corrplot(correlations, order = "hclust", addrect=3)
corrplot(correlations, method = "circle", type="upper", order = "hclust")
#Exploring missing data
X11()
missmap(restaurant_train)
X11()
d = melt(restaurant_train[,-c(1:5)])

d = melt(restaurant_train[,-c(1:5)], variable_name = "p", value.name="v")
str(d)
ggplot(d,aes(x = value)) + 
  facet_wrap(~p,scales = "free_x") + 
  geom_histogram()
