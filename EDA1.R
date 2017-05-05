library(ggplot2)
setwd("C:\\Users\\Vamsi\\Desktop\\R.Alg\\practice\\data sets\\titanic\\data set\\")
titanic_train = read.csv("train.csv")
dim(titanic_train)
str(titanic_train)
titanic_train$Survived = as.factor(titanic_train$Survived)
titanic_train$Pclass = as.factor(titanic_train$Pclass)
titanic_train$Name = as.character(titanic_train$Name)

##univariate EDA
#categorical variables
X11()
xtabs(~Survived,titanic_train)
summary(titanic_train$Survived)
ggplot(titanic_train) + geom_bar(aes(x=Survived))

summary(titanic_train$Sex)
ggplot(titanic_train) + geom_bar(aes(x=Sex))

summary(titanic_train$Pclass)
ggplot(titanic_train) + geom_bar(aes(x=Pclass))

#numerical variables
summary(titanic_train$Fare)
ggplot(titanic_train) + geom_histogram(aes(x=Fare),fill = "white", colour = "black")
ggplot(titanic_train) + geom_boxplot(aes(x=factor(0),y=Fare)) + coord_flip()
ggplot(titanic_train) + geom_density(aes(x=Fare))

summary(titanic_train$Age)
ggplot(titanic_train) + geom_histogram(aes(x=Age),fill = "white", colour = "black")
ggplot(titanic_train) + geom_boxplot(aes(x=factor(0),y=Age)) + coord_flip()


##bivariate EDA
#==================
#C-C relationships
X11()
xtabs(~Survived+Sex,titanic_train)
ggplot(titanic_train) + geom_bar(aes(x=Sex, fill=Survived) )

xtabs(~Survived+Pclass,titanic_train)
ggplot(titanic_train) + geom_bar(aes(x=Pclass, fill=Survived) )

xtabs(~Survived+Embarked,titanic_train)
ggplot(titanic_train) + geom_bar(aes(x=Embarked, fill=Survived) )

#N-C relationships
ggplot(titanic_train) + geom_boxplot(aes(x = Survived, y = Age))
ggplot(titanic_train) + geom_histogram(aes(x = Age),fill = "white", colour = "black") + facet_grid(Survived ~ .)

ggplot(titanic_train) + geom_boxplot(aes(x = Survived, y = Fare))
ggplot(titanic_train) + geom_histogram(aes(x = Fare),fill = "white", colour = "black") + facet_grid(Survived ~ .)



##multivariate EDA
#====================
X11()
xtabs(~Survived+Pclass+Sex,titanic_train)
ggplot(titanic_train) + geom_bar(aes(x=Sex, fill=Survived)) + facet_grid(Pclass ~ .)

X11()
xtabs(~Survived+Embarked+Sex,titanic_train)
ggplot(titanic_train) + geom_bar(aes(x=Sex, fill=Survived)) + facet_grid(Embarked ~ .)

X11()
xtabs(~ Pclass + Survived + Sex, titanic_train)
ggplot(titanic_train) + geom_bar(aes(x=Pclass, fill=Survived) ) + facet_grid(Sex ~ .)
ggplot(titanic_train) + geom_bar(aes(x=Sex, fill=Survived) ) + facet_grid(Pclass ~ .)

X11()
xtabs(~ Embarked + Survived + Sex+Pclass, titanic_train)
xtabs(~ Embarked + Survived +  Pclass + Sex, titanic_train)
ggplot(titanic_train) + geom_bar(aes(x=Embarked, fill=Survived) ) + facet_grid(Sex ~ Pclass)


# explore N-C relationships
X11()
ggplot(titanic_train) + geom_boxplot(aes(x = Survived, y = Fare))
ggplot(titanic_train) + geom_histogram(aes(x = Fare)) + facet_grid(Survived ~ .)
ggplot(titanic_train) + geom_histogram(aes(x = Fare)) + facet_grid(Survived ~ Sex)

# explore relation between continuous variables
ggplot(titanic_train) + geom_point(aes(x = Fare, y = Age))


##############################################################################################
tata = c(100,150,70,50,200,500)
ms = c(100,120,150,150,160,180)
mad(tata, center = mean(tata))
mad(ms, center = mean(ms))

sd(tata)
sd(ms)

cat_2015 = c(100,99,98,98,97,96)
cat_2016 = c(80,79,79,78,78,77)
cat_2014 = c(140,139,139,139,139,138)

cat_2015_z = (cat_2015 - mean(cat_2015)) / sd(cat_2015)
cat_2016_z = (cat_2016 - mean(cat_2016)) / sd(cat_2016)

cat_scores_z = data.frame(cat_2015, cat_2015_z, cat_2016, cat_2016_z)

cat_scores = data.frame(cat_2014, cat_2015, cat_2016)

require(caret)
?preProcess
#pre process computes required quantities for transformations
preobj = preProcess(cat_scores, method=c("center", "scale"))
preobj$mean
preobj$std
#predict method applies the transformation on data
cat_scores_z = predict(preobj, cat_scores)

