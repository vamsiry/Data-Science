#univariate-EDA
library(ggplot2)
library(Amelia)

setwd("C:\\Users\\Thimma Reddy\\Documents\\GitHub\\datascience\\2014\\kaggle\\titanic\\data")
titanic = read.table("train.csv", header = TRUE, sep= ",",na.strings=c("NA",""))
dim(titanic)
str(titanic)

titanic$Survived = as.factor(titanic$Survived)
titanic$Pclass = as.factor(titanic$Pclass)
missmap(titanic, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

# exploring survived feature using bar chart
table(titanic$Survived)
ggplot(titanic, aes(x = Survived)) + geom_bar()

# exploring pclass feature using bar chart
table(titanic$Pclass)
ggplot(titanic, aes(x = Pclass)) + geom_bar()

# exploring fare feature using boxplot and historgram
summary(titanic$Fare)
ggplot(titanic, aes(x = factor(0), y = Fare)) + geom_boxplot() + coord_flip()
ggplot(titanic, aes(x = Fare)) + geom_histogram()
ggplot(titanic, aes(x = Fare)) + geom_histogram(fill = "white", colour = "black")
ggplot(titanic, aes(x = Fare, y=..density..)) + geom_histogram(fill = "white", colour = "black")
ggplot(titanic, aes(x = Fare)) + geom_histogram(binwidth = 2, fill = "white", colour = "black")
ggplot(titanic, aes(x = Fare)) + geom_density()

# exploring age feature using boxplot and histogram
summary(titanic$Age)
ggplot(titanic, aes(x = factor(0), y = Age)) + geom_boxplot() + coord_flip()


#bivariate-EDA
#==============
library(ggplot2)

setwd("C:\\Users\\Thimma Reddy\\Documents\\GitHub\\datascience\\2014\\kaggle\\titanic\\data")
titanic = read.table("train.csv", header = TRUE, sep= ",",na.strings=c("NA",""))
dim(titanic)
str(titanic)

# converting the types of features
titanic$Survived = as.factor(titanic$Survived)
titanic$Pclass = as.factor(titanic$Pclass)
titanic$Family = titanic$SibSp + titanic$Parch + 1

# Comparing Survived and passenger class using table and histograms
xtabs(~Survived + Pclass, data=titanic)
ggplot(titanic, aes(x = Survived, fill = Pclass)) + geom_bar()
ggplot(titanic, aes(x = Survived, fill = Pclass)) + geom_bar(position = "fill")
chisq.test(titanic$Survived, titanic$Pclass)

# Comparing Survived and Sex using table and histograms
xtabs(~Survived + Sex, data=titanic)
ggplot(titanic, aes(x = Survived, fill = Sex)) + geom_bar()
ggplot(titanic, aes(x = Survived, fill = Sex)) + geom_bar(position="fill")
ggplot(titanic, aes(x = Sex, fill = Survived)) + geom_bar(position="fill")
chisq.test(titanic$Survived, titanic$Sex)

# Comparing Survived and Embarked using table and bar charts
xtabs(~Survived + Embarked, data=titanic)
ggplot(titanic, aes(x = Embarked , fill = Survived)) + geom_bar(position = "fill")
ggplot(titanic, aes(x = Survived, fill = Embarked)) + geom_bar(position = "fill")
chisq.test(titanic$Survived,titanic$Embarked)

# Comparing Age and Survived using boxplots 
ggplot(titanic, aes(x = Survived, y = Age)) + geom_boxplot()
ggplot(titanic, aes(x = Age)) + geom_histogram() + facet_grid(Survived ~ .)
ggplot(titanic, aes(x = Age, color = Survived)) + geom_density() 
summary(titanic$Age)

# Comparing Survived and Fare using boxplots 
ggplot(titanic, aes(x = Survived, y = Fare)) + geom_boxplot() 
ggplot(titanic, aes(x = Fare, color = Survived)) + geom_density() 

# Comparing Survived and Family using boxplots
ggplot(titanic, aes(x = Survived, y = Family)) + geom_boxplot()
ggplot(titanic, aes(x = Family)) + geom_histogram() + facet_grid(Survived ~ .)


# Comparing Sibsp and Fare using scatterplot
cor(titanic$SibSp, titanic$Fare)
ggplot(titanic, aes(x = SibSp, y = Fare)) + geom_point()

# Comparing Parch and Fare using scatterplot
cor(titanic$Parch, titanic$Fare)
ggplot(titanic, aes(x = Parch, y = Fare)) + geom_point()

# Comparing Family and Parch using scatterplot
cor(titanic$Parch, titanic$Family)
ggplot(titanic, aes(x = Parch, y = Family)) + geom_point()


#multivariate-EDA
#===================
library(ggplot2)
library(GGally)

setwd("C:\\Users\\Thimma Reddy\\Documents\\GitHub\\datascience\\2014\\kaggle\\titanic\\data")
titanic = read.table("train.csv", header = TRUE, sep= ",",na.strings=c("NA",""))
dim(titanic)
str(titanic)

# converting the types of features
titanic$Survived = as.factor(titanic$Survived)
titanic$Pclass = as.factor(titanic$Pclass)
titanic$Family = titanic$SibSp + titanic$Parch + 1
titanic = titanic[c("Pclass","Survived","Sex","Age","Family","Fare","Embarked")]

ggpairs(titanic, columns=1:4,axisLabels="show")

plot  = ggpairs(titanic, axisLabels="show")

pdf("out2.pdf", height=500, width=500)
print(plot)
dev.off()

ggpairs(titanic,columns=1:4, 
        upper = list(continuous = "density", discrete="facetbar", combo="box"),
        lower = list(discrete="facetbar", continous="cor", combo = "box")
)

setwd("C:\\Users\\Thimma Reddy\\Documents\\GitHub\\datascience\\datasets")

winedata = read.csv("wine.txt", header = TRUE)
dim(winedata)
str(winedata)
head(winedata)

cor_matrix = cor(winedata)
plot = ggpairs(winedata,columns=1:4, axisLabels="show")

plot = ggpairs(winedata, axisLabels="show")













